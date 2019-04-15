#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------
Drilldown_Bar_Chart =function(input_data, click_event , drilldown_bar_view_by, barchart_view){
  

  if(length(click_event))
  {
    #call function to make graph
    filter_by = unique(input_data[[barchart_view]])
    data_filter = input_data[  filter_by== click_event[['x']], ]
    
    
   # data_filter = subset(input_data, input_data[[barchart_view]] == s[['x']] )
   # View(filter_by)
   # View(click_event[['x']])
   # View(data_filter)
    
    bar_data = aggregate(list(y = data_filter$MSO), 
                         by = list(x = data_filter[[drilldown_bar_view_by]]),
                         FUN = sum )
    
    
    bar_data$x = factor(bar_data$x, levels = unique(bar_data$x)[order(bar_data$y, decreasing = TRUE)]) 
    
    
    #combine and create a new category that is "others"
    drilldown_bar_chart = plot_ly(bar_data, x = ~x, y = ~y, type = "bar", name = 'Drilldown MSO') %>%
      layout(
        
        title = "MSO Drilled Down",
        xaxis = list(title = drilldown_bar_view_by), 
        yaxis = list(title = "Missed Sales Opportunities")
        
      )%>% rangeslider()
    
    return(drilldown_bar_chart)
  }else{
  
    return(plotly_empty())
  }
  
    
}