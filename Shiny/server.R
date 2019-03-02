#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#-------------------
#Open R Packages

library(shiny)
library(dfoptim)
library(ggplot2)
library(slam)
library(Rglpk)
library(DT)

library(dplyr)
library(shiftR)
library(readxl)

#-------------------

#-------------------
#Set Seed = 5
set.seed(5)
#-------------------

#-------------------
#Point R to all functions

source("Shiny/Functions/Zero_OH_Qty_Method.R")
source("Shiny/Functions/cleandata.R")

#source("Functions/DescriptiveStats.R")

#-------------------

#-------------------
#Server to output to UI

server = function(input, output){
  
  #Increase max limit on file size to 10KB
  options(shiny.maxRequestSize=30*1024^2) 
  
  #Wait for Submit Button
  observeEvent(input$submit, {
    
    #Validate an input file as been selected
    validate(
      need(input$data.in, "No File Selected")
    )
    req(input$data.in)
    
    withProgress(message = "Starting", value = 0,{

    
       
        
        #Table Title
        output$SalesData <- renderText( 
          paste("Sales Data Table")
        )
        
        #Pass input file to clean.data
        incProgress(1/11, message = "Analizing Zero OH Method")
        MSO = Zero_OH_Qty_Method(input$data.in$datapath)
        output$data_cleaned = renderDataTable({
          
          MSO
          })
        
        
        MSO_sum = sum(MSO$MSO)
        
        output$sum_MSO = renderText({
          
          paste("Missed Sales Opportunities: " , MSO_sum )
        })
  
    })
  })
}
#-------------------