#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#-------------------
#Main

#wd = "C:/Users/atorr/Box/Capstone Hershey/Deliverable/hersheyapp_FullSeasonData"
#setwd(wd)
#point R to UI and Server
source("Shiny/server.R")
source("Shiny/ui.R")

#run Shiny Application
runApp(shinyApp(ui,server))

#-------------------

