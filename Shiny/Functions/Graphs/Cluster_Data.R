#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

Cluster_Data = function(mtd1, mtd2, mtd3, view_by){
  
  bar_data_1 = aggregate(list(y = round(mtd1$MSO, 0)), by = list(x = mtd1[[view_by]]), FUN = sum )
  bar_data_2 = aggregate(list(y = round(mtd2$MSO,0)), by = list(x = mtd2[[view_by]]), FUN = sum )
  bar_data_3 = aggregate(list(y = round(mtd3$MSO, 0)), by = list(x = mtd3[[view_by]]), FUN = sum )
  bar_data_1["Mtd"] = "Method 1"
  bar_data_2["Mtd"] = "Method 2"
  bar_data_3["Mtd"] = "Method 3"
  
  return(list(bar_data_1, bar_data_2,bar_data_3))
  
  
}