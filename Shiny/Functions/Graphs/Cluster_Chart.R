#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

Cluster_Chart = function(mtd_list, view_by){

  bar_data_1 = mtd_list[[1]]
  bar_data_2= mtd_list[[2]]
  bar_data_3 = mtd_list[[3]]
  #Make cluser chart
  cluster_chart <- plot_ly()%>%
    add_trace(x = bar_data_1$x, y = bar_data_1$y, type = 'bar',
              text = bar_data_1$y,textposition = 'auto',
              name = 'Method 1',
              marker = list(color = 'rgba(222,45,38, 0.8)'))%>%
    add_trace(x = bar_data_2$x, y = bar_data_2$y, type = 'bar',
              text = bar_data_2$y, textposition = 'auto',
              name = 'Method 2',
              marker = list(color = 'rgb(158,202,225)'))%>%
    add_trace(x = bar_data_3$x, y = bar_data_3$y, type = 'bar',
              text = bar_data_3$y, textposition = 'auto',
              name = 'Method 3',
              marker = list(color = 'rgb(250,130,50)'))%>%
    layout(title= "MSO Methods Comparison",
           barmode = 'group',
           yaxis = list(title = "MSO"),
           xaxis = list(title = view_by))
    

  return(cluster_chart)
}
