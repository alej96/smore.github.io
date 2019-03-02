Limiter_table = function(store_index)
{
  
  
  
  
  ifelse(store_index > 4, percentage = 0.75, 
         ifelse(store_index <= 4 && store_index >= 1.5), percentage = 0.5,
         ifelse(store_index < 1.5 && store_index >= 1), percentage = 0.25, percentage = 0.1)
    
  
  return(percentage)
  
}
