#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#library(plotly)
#wd = "C:/Users/atorr/Box/Capstone Hershey/Deliverable/hersheyapp/Shiny/Functions/Graphs"
#setwd(wd)

#view_by = "Building State/Prov"
Bar_Chart = function(input_data, view_by){
  
   bar_data = aggregate(list(y = input_data$MSO), by = list(x = input_data[[view_by]]), FUN = sum )
   
   bar_data$x = factor(bar_data$x, levels = unique(bar_data$x)[order(bar_data$y, decreasing = TRUE)]) 
   #combine and create a new category that is "others"
   bar_chart = plot_ly(bar_data, x = ~x, y = ~y, type = "bar", name = 'MSO',
                       source = "bar_chart_click") %>%
     layout(
       
       title = "Missed Sales Opportunities by Selected View",
       xaxis = list(title = view_by), 
       yaxis = list(title = "Missed Sales Opportunities"),
       colors(TRUE)
       ) %>% rangeslider()
   
   
  

  return(bar_chart)  
}