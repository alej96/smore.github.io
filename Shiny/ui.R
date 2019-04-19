#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#-------------------
#User Interface

ui = fluidPage(#theme = shinytheme("spacelab"),
  #link to change themes: https://rstudio.github.io/shinythemes/
  #Title of the page
  titlePanel("S.M.O.R.E App"),
  setBackgroundImage(src = "https://github.com/alej96/HSY-Presentation/blob/master/HSY_Seasons4.png?raw=true"),
    #src = "http://www.thehersheycompany.com/content/corporate/en_us/this-is-hershey/_jcr_content/pagecontent/corporatebanner/image.img.jpg/1472141151739.jpg"),
    #src = "https://www.hersheys.com/is/image/content/dam/franchise/en_us/images/Common/hsy_tout_simpleingredients_570x320.jpg?hei=320&wid=570&fmt=jpg"),
  
  #Format Page	
  sidebarLayout(
    
    #Input Area
    sidebarPanel(
      
      wellPanel(
      #Input .CSV file
      fileInput("data.in", "Read Data: ",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xlsx"
                )
      ),
      selectInput("file_type", "Select File Type:",
                 choices = c(".xlsx" = "xlsx",
                             ".csv" = "csv",
                             ".txt" = "txt")),
      
      #Check Box for Method
      
      checkboxGroupInput("methods", 
                         "Choose Methods:",
                         c("Method 1" = "mtd1",
                           "Method 2" = "mtd2",
                           "Method 3" = "mtd3")),
      
      #Submit Button
      actionButton("submit", "Run Calculation")),
      
      wellPanel(
        selectInput("cluster_view_by", "View Combined Methods by:",
                    choices = c( "Seasonal Segmentation" = "HSY Seasonal Segmentation",
                                 "HSY Item Description" = "HSY Item Description",
                                 "UPC" = "UPC",
                                 "Store Nbr" = "Store Nbr",
                                 "Store Name" = "Store Name",
                                 "City" = "Building City",
                                 "State/Prov"= "Building State/Prov",
                                 "Postal Code" = "Building Postal Code"))
      ),
      
      wellPanel(
        selectInput("line_view_by", "View Line Chart by:",
                      choices = c("HSY Item Description" = "HSY Item Description",
                                  "UPC" = "UPC",
                                  "Seasonal Segmentation" = "HSY Seasonal Segmentation",
                                  "Store Nbr" = "Store Nbr",
                                  "Store Name" = "Store Name",
                                  "City" = "Building City",
                                  "State/Prov"= "Building State/Prov",
                                  "Postal Code" = "Building Postal Code"))),
      wellPanel(
      selectInput("bar_view_by", "View Bar Chart by:",
                  choices = c("State/Prov" = "Building State/Prov",
                              "HSY Item Description" = "HSY Item Description",
                              "UPC" = "UPC",
                              "Seasonal Segmentation" = "HSY Seasonal Segmentation",
                              "Store Nbr" = "Store Nbr",
                              "Store Name" = "Store Name",
                              "City" = "Building City",
                              "Postal Code" = "Building Postal Code")),
      
      selectInput("drilldown_bar_view_by", "Drill Down by:",
                  choices = c("Postal Code" = "Building Postal Code",
                              "State/Prov" = "Building State/Prov",
                              "HSY Item Description" = "HSY Item Description",
                              "UPC" = "UPC",
                              "Seasonal Segmentation" = "HSY Seasonal Segmentation",
                              "Store Nbr" = "Store Nbr",
                              "Store Name" = "Store Name",
                              "City" = "Building City"))),
 
      selectInput("download_opt", "Select Download:",
                  choices = c("Clean Data" = "down_clean_data",
                              "Cluster Methods"= "down_cluster",
                              "Method 1" = "down_mtd1",
                              "Method 2" = "down_mtd2",
                              "Method 3"= "down_mtd3")),
      
      #Download finished data
     # downloadButton("download", "Download Clean Data"),
    downloadButton("download", "Download"),
      #fix width to 4
      width = 4
    ),
    
    #Ouput Area
    mainPanel(
      #Add conditionalpanel() for all 3 methods
          tabsetPanel(
            tabPanel("Cluster data",
                     br(),
                     br(),
                      DT::dataTableOutput("cluster_grid"),
                     br(),
                     br(),
                     br(),
                        plotlyOutput("cluster_chart"),
                     br(),
                     br()
                     ),
           
            tabPanel("Charts Mtd 1",
              h2(textOutput("sum_MSO_1")),
              #Display Browsed Data
              plotlyOutput("line_chart_1"),
              br(),
              br(),
              plotlyOutput("bar_chart_1"),
              br(),
              br(),
              br(),
              verbatimTextOutput("selection_1"),
              plotlyOutput("drilldown_bar_chart_1"),
              br(),
              br(),
              br()),
            
            tabPanel( "Data Table Mtd 1",
              h2(textOutput("SalesData_1")),
              br(),
              br(),
              DT::dataTableOutput("MSO_table_1"),
              br()
              ),
            tabPanel('Map Mtd 1',
                     plotlyOutput("map_1")),
            #====================Tab2==========================================
    
         tabPanel("Charts Mtd 2",
                  h2(textOutput("sum_MSO_2")),
                  br(),
                  #Display Browsed Data
                  plotlyOutput("line_chart_2"),
                  br(),
                  br(),
                  plotlyOutput("bar_chart_2"),
                  br(),
                  br(),
                  br(),
                  verbatimTextOutput("selection_2"),
                  plotlyOutput("drilldown_bar_chart_2"),
                  br(),
                  br(),
                  br()),
         
         tabPanel( "Data Table Mtd 2",
                   h2(textOutput("SalesData_2")),
                   br(),
                   br(),
                   DT::dataTableOutput("MSO_table_2")
         ),
         tabPanel('Map Mtd 2',
                  plotlyOutput("map_2")),
         
         #==============Tab3=============================================
         tabPanel("Charts Mtd 3",
                  br(),
                  h2(textOutput("sum_MSO_3")),
                  #Display Browsed Data
                  plotlyOutput("line_chart_3"),
                  br(),
                  plotlyOutput("bar_chart_3"),
                  br(),
                  br(),
                  br(),
                  verbatimTextOutput("selection_3"),
                  plotlyOutput("drilldown_bar_chart_3"),
                  br(),
                  br()),
         
         tabPanel( "Data Table Mtd 3",
                   br(),
                   h2(textOutput("SalesData_3")),
                   br(),
                   br(),
                   DT::dataTableOutput("MSO_table_3")
                   #h2(textOutput("MSO_by_Item"))
         ),
         tabPanel('Map Mtd 3',
                  plotlyOutput("map_3")
         ))
      
      )#end main panel
      
    )
  )

#-------------------