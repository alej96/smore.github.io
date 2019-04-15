#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

Cluster_Grid =  function(bar_data_list, name_x){
  
  #make grid
  cluster_chart_data = do.call(rbind, bar_data_list)

  grid = cluster_chart_data %>% spread(Mtd, y)
  
  colnames(grid)[colnames(grid) == "x"] <- name_x
  
  return(grid)
}