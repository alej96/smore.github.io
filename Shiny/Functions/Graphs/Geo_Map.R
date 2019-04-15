#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#library(plotly)

#source: https://plot.ly/r/choropleth-maps/
#other source: https://geocompr.robinlovelace.net/adv-map.html

Geo_Map = function(cleaned_data, MSO_data){

  #make graph for MSO 
  MSO_graph_data = aggregate(list(MSO = round(MSO_data$MSO, 2)),
                         by = list(State = MSO_data$`Building State/Prov`), 
                         FUN = sum )
  
  #Make table to show ALL the POS Sales & Qty
  graph_data = aggregate(list(POS.Sales = round(cleaned_data$`POS Sales`,0),
                              POS.Qty = round(cleaned_data$`POS Qty`,0)), 
                         by = list(State = cleaned_data$`Building State/Prov`), 
                         FUN = sum )
  
  #conbine tables for hover
  
  mso.state = unique(MSO_graph_data$State)
  nbr.mso.state = length(MSO_graph_data$State)
  n.cleaned.state = length(graph_data$State)
  name.cleaned.state = unique(graph_data$State)
  
  graph_data$MSO = 0
  for(i in 1: n.cleaned.state)
  {
    case = which(MSO_graph_data$State == name.cleaned.state[i])
    if( is.integer(case) && length(case) != 0L ){
      MSO_col = MSO_graph_data$MSO[case]         
      graph_data$MSO[i] =  MSO_graph_data$MSO[case] 
    }
    
  }
  
  graph_data$hover <- with(graph_data, paste( graph_data$State,"<br>",
                                              "MSO: $", format(graph_data$MSO, big.mark = ","), "<br>", 
                                              "POS Sales: $",format(graph_data$POS.Sales, big.mark = ","), "<br>",
                                               "POS Qty:",format(graph_data$POS.Qty, big.mark = "," )))
  
  # give state boundaries a white border
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  geo_map <- plot_geo(graph_data, locationmode = 'USA-states') %>%
    add_trace(
      z = ~MSO, text = ~hover, locations = ~State,
      color = ~MSO, colors = 'Blues'
    ) %>%
    colorbar(title = "US Dollars ($)") %>%
    layout(
      title = 'USA Missed Sales Opportunities',
      geo = g
    )
  return(geo_map)
  
}

