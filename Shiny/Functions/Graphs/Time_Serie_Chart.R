library(plotly)
library("zoo")

view_by= input_data$`HSY Item Description`

Time_Series_Chart = function(input_data, view_by ){
  
  #bar_data = tapply(input_data$MSO, input_data$UPC)
  time_value = aggregate(list(MSO = input_data$MSO), by = list(WM.Date = input_data$`WM Date`, View_by = view_by), FUN = sum )
  #Time series of MSO
  time_series <- plot_ly(data = time_value, x= time_value$WM.Date, y= ~MSO , type = 'scatter', mode = 'lines', split = ~View_by) %>%
    layout(title = 'Time Series MSO',showlegend = TRUE)
  
  return(time_series)
}
