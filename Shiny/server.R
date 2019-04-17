#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Pohl
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#-------------------
#Open R Packages
#install.packages("shiny")
#install.packages("readxl")
#install.packages("plotly")
#install.packages("tidyr")
#install.packages("zoo")
#install.packages("shinywidgets")
#install.packages("shinythemes")


#library(dfoptim)
#library(ggplot2)
#library(slam)
#library(Rglpk)
#library(DT)

#library(dplyr)
#library(shiftR)
library(shiny)
library(readxl)
library(plotly)
library(tidyr)
library("zoo")
library(shinyWidgets)
library(shinythemes)

library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(data.table) 

#-------------------

#-------------------
#Set Seed = 5
set.seed(5)
#-------------------

#-------------------
#Point R to all functions
source("Shiny/Functions/cleandata2.R")
source("Shiny/Functions/Zero_OH_Qty_Method.v2.R")
source("Shiny/Functions/Weighted_OH_Method_v2.R")
source("Shiny/Functions/Method2.R")
source("Shiny/Functions/Graphs/Time_Series_Chart.R")
source("Shiny/Functions/Graphs/Bar_Chart.R")
source("Shiny/Functions/Graphs/Geo_Map.R")
source("Shiny/Functions/Graphs/Drilldown_Bar_Chart.R")
source("Shiny/Functions/Graphs/Cluster_Chart.R")
source("Shiny/Functions/Graphs/Cluster_Grid.R")
source("Shiny/Functions/Graphs/Cluster_Data.R")

#-------------------

#-------------------
#Server to output to UI

server = function(input, output){
  
  #Increase max limit on file size to 50MB
  options(shiny.maxRequestSize=500^1024^2) 
  
  #Wait for Submit Button
  observeEvent(input$submit, {
    
    #Validate an input file as been selected
    validate(
      need(input$data.in, "No File Selected")
    )

    
    withProgress(message = "Starting", value = 0,{
      
      req(input$data.in)

      #Table Title
      output$SalesData <- renderText( 
        paste("Sales Data Table")
      )
      
      #Pass input file to clean.data
      #incProgress(1/11, message = "Analyzing Missed Sales Opportunities")
      req(input$methods)
      clean_data = clean.data(input$data.in$datapath, input$file_type)
      
      mtd_list = list()
      mtd_list = input$methods
      
      nbr_mtds = length(mtd_list)
      
   #   empty_data <- data.frame("UPC" = c(0,0),"Store Nbr"=c(0,0),	"HSY Item Description" = c("empty data","empty data"),
    #                          "Building City" =c("empty data", "empty data"),	
     #                         "HSY Seasonal Segmentation" =c( "empty data","empty data"),
      #                        "WM Date" = c(0,0),	"Store Name" = c("empty data","empty data"),
       #                       "Building State/Prov" =c("empty data","empty data"),	
        #                      "Building Postal Code"= c("empty data","empty data"),
         #                     "SeasonAndYear" = c("empty data","empty data"),
          #                    "Unit Retail" =c(0,0) ,"OH Qty" = c(0,0),	
           #                   "POS Sales" =c(0,0),	"POS Qty"=c(0,0), "MSO" =c(0,0))
      
      empty_data <- data.frame("UPC"= c(0,0),"HSY Item Description" = c("empty data", "empty data"),
                               "HSY Seasonal Segmentation"= c("empty data","empty data"),"Store Nbr"=c(0,0),
                               "Store Name" = c("empty data", "empty data"),"Building City" =c("empty data", "empty data"),
                               "Building State/Prov" =c("empty data", "empty data"),"Building Postal Code" =c(0, 0),
                               "Store Type" = c("empty data", "empty data"),"WM Date" = c("empty data", "empty data"),
                                "SeasonAndYear" = c("empty data", "empty data"),"OH Qty" = c(0, 0),
                                "POS Qty" = c(0,0),"POS Sales"= c(0,0), "MSO" = c(0,0))
      
      col_names = c("UPC","HSY Item Description","HSY Seasonal Segmentation","Store Nbr",
                    "Store Name","Building City","Building State/Prov","Building Postal Code",
                    "Store Type","WM Date","SeasonAndYear","OH Qty","POS Qty","POS Sales", "MSO")
      
      colnames(empty_data) <- col_names
      
      MSO_1 = empty_data
      MSO_2 = empty_data
      MSO_3 = empty_data
      
      #View(mtd_list)
    #  View(nbr_mtds)
      for(i in 1: nbr_mtds){
        
        if(mtd_list[[i]] == "mtd1")
        {
          MSO_1 = Zero_OH_Qty_Method(clean_data)
          incProgress(1/11, message = "Making Table")
          
          #changed from data_cleaned_1
          output$MSO_table_1 = DT::renderDataTable(
            DT::datatable(MSO_1, options =list(
              pageLength = 5, paging = TRUE )))
          
          
          MSO_sum_1 = sum(MSO_1$MSO)
          
          #changed from sum_MSO_1 -> didn't fix issue though
          output$SalesData_1 = renderText({
            
            paste("Missed Sales Opportunities: $" , format(round(MSO_sum_1, digits = 0), big.mark = ",") )
          })
          
          
          output$line_chart_1 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Time_Series_Chart(MSO_1, input$line_view_by)
            })
          
          incProgress(1/11, message = "Making Bar Chart")
          
          output$bar_chart_1 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Bar_Chart(MSO_1, input$bar_view_by)
            })
          
          
          
          output$selection_1 <- renderPrint({
            s_1 <- event_data("plotly_click", source = "bar_chart_click")
            if (length(s_1) == 0) {
              "Please click on one bar to see more details. The little chart is a range slider. You can move the views!"
            }
          })
          
          output$drilldown_bar_chart_1 = renderPlotly(
            {
              click_event <- event_data("plotly_click", source = "bar_chart_click")
              Drilldown_Bar_Chart(MSO_1, click_event,
                                  input$drilldown_bar_view_by, input$bar_view_by)
            }
            
          )
          
          
          incProgress(1/11, message = "Making Map")
          output$map_1 = renderPlotly(
            {
              Geo_Map(clean_data, MSO_1)
            })
        }#end mtd1=====================================================================================================
        if (mtd_list[[i]] == "mtd2")
        {
          MSO_2 = Not_Enough_OH_Method(clean_data)
          incProgress(1/11, message = "Making Table")
          
          #changed from data_cleaned_2
          output$MSO_table_2 = DT::renderDataTable(
            DT::datatable(MSO_2, options =list(
              pageLength = 5, paging = TRUE )))
          
          
          MSO_sum_2 = sum(MSO_2$MSO)
          
          #changed from MSO_table_2
          output$SalesData_2 = renderText({
            
            paste("Missed Sales Opportunities: $" , format(round(MSO_sum_2, digits = 0), big.mark = ",") )
          })
          
          
          output$line_chart_2 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Time_Series_Chart(MSO_2, input$line_view_by)
            })
          
          incProgress(1/11, message = "Making Bar Chart")
          
          output$bar_chart_2 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Bar_Chart(MSO_2, input$bar_view_by)
            })
          
          
          
          output$selection_2 <- renderPrint({
            s_2 <- event_data("plotly_click", source = "bar_chart_click")
            if (length(s_2) == 0) {
              "Please click on one bar to see more details. The little chart is a range slider. You can move the views!"
            }
          })
          
          output$drilldown_bar_chart_2 = renderPlotly(
            {
              click_event <- event_data("plotly_click", source = "bar_chart_click")
              Drilldown_Bar_Chart(MSO_2, click_event,
                                  input$drilldown_bar_view_by, input$bar_view_by)
            }
            
          )
          
          
          incProgress(1/11, message = "Making Map")
          output$map_2 = renderPlotly(
            {
              Geo_Map(clean_data, MSO_2)
            })
          
        }#end mtd2=========================================================================================================
        if(mtd_list[[i]] == "mtd3")
        {
          MSO_3 = Weighted_OH_Method(clean_data)
          
          incProgress(1/11, message = "Making Table")
          
          #changed from data_cleaned_3
          output$MSO_table_3 = DT::renderDataTable(
            DT::datatable(MSO_3, options =list(
              pageLength = 5, paging = TRUE )))
          
          MSO_sum_3 = sum(MSO_3$MSO)
          
          #changed from sum_MSO_3
          output$SalesData_3 = renderText({
            
            paste("Missed Sales Opportunities: $" , format(round(MSO_sum_3, digits = 0), big.mark = ",") )
          })
          
          
          output$line_chart_3 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Time_Series_Chart(MSO_3, input$line_view_by)
            })
          
          incProgress(1/11, message = "Making Bar Chart")
          
          output$bar_chart_3 = renderPlotly(
            {
              #line_chart_input= input$line_input
              Bar_Chart(MSO_3, input$bar_view_by)
            })
          
          
          
          output$selection_3 <- renderPrint({
            s <- event_data("plotly_click", source = "bar_chart_click")
            if (length(s) == 0) {
              "Please click on one bar to see more details. The little chart is a range slider. You can move the views!"
            }
          })
          
          output$drilldown_bar_chart_3 = renderPlotly(
            {
              click_event <- event_data("plotly_click", source = "bar_chart_click")
              Drilldown_Bar_Chart(MSO_3, click_event,
                                  input$drilldown_bar_view_by, input$bar_view_by)
            }
            
          )
          
          
          incProgress(1/11, message = "Making Map")
          output$map_3 = renderPlotly(
            {
              Geo_Map(clean_data, MSO_3)
            })
        } #end mtd3=============================================================================================================
         # print(head(MSO))

       
      }
      
      #========Cluster Methods=======================
      
      cluster_methods = reactive({Cluster_Data(MSO_1, MSO_2, MSO_3, input$cluster_view_by)})
      
      #cluster Chart of methods
      output$cluster_chart = renderPlotly({cluster_chart = Cluster_Chart(cluster_methods(), input$cluster_view_by)})
      
      cluster_grid = reactive({Cluster_Grid(cluster_methods(), input$cluster_view_by)})
      output$cluster_grid = DT::renderDataTable(
                              DT::datatable({
                                cluster_grid()},
                                options =list(
                                pageLength = 5, paging = TRUE )))
        
    })
  
  
  #=============Create .CSV file to be downloaded==============================================
      download_data <- reactive({
        if(input$download_opt == "down_clean_data"){clean_data}
        else if(input$download_opt == "down_cluster"){cluster_grid()}
        else if(input$download_opt == "down_mtd1"){MSO_1}
        else if(input$download_opt == "down_mtd2"){MSO_2}
        else if(input$download_opt == "down_mtd3"){MSO_3}
      })
    download_name <- reactive({
      if(input$download_opt == "down_clean_data"){"CleanData"}
      else if(input$download_opt == "down_cluster"){"ClusterData"}
      else if(input$download_opt == "down_mtd1"){"Method_1_Data"}
      else if(input$download_opt == "down_mtd2"){"Method_2_Data"}
      else if(input$download_opt == "down_mtd3"){"Method_3_Data"}
    })
      output$download <- downloadHandler(
        #name the file
        filename = function() {
          paste(download_name(), ".csv", sep = "")
        },
        content = function(file) {
          #write.csv(download_data(), file, row.names = FALSE)
          fwrite(download_data(), file)
        }
      )
  })
}
#-------------------