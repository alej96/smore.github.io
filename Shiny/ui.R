#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#-------------------
#User Interface

ui = fluidPage(
  
  #Title of the page
  titlePanel("Hershey Tool"),
  
  #Format Page	
  sidebarLayout(
    
    #Input Area
    sidebarPanel(
      
      #Input .CSV file
      fileInput("data.in", "Read Data (CSV)",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
      ),
      
      #Sliders....
      #User interfce options
      
      #Submit Button
      actionButton("submit", "Submit"),
      
      #Download finished data
      downloadButton("download", "Download"),
      
      #fix width to 4
      width = 4
    ),
    
    #Ouput Area
    mainPanel(
      
      #Display Browsed Data
      h2(textOutput("SalesData")),
      #DT::dataTableOutput("data.out"),
      
      DT::dataTableOutput("data_cleaned"),
      h2(textOutput("sum_MSO"))
      
      
    )
  )
)
#-------------------