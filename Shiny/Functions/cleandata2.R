library(readxl)
library(shiftR)


#wd = "~/Box/Capstone Hershey/Deliverable/Draft_Brooke"
#setwd(wd)

#big_file = "VAL2018_Sales_Seasonal Adhoc.xlsx"
#input_file = "VAL2018_Sales_1500rows.xlsx"
two_items = "EAS_2017_TWO_Items.xlsx"

#----------------------
clean.data = function(input_file){
  
  #Read the excel file
  df = read_excel(input_file, col_names = TRUE)
  
  #Delete the first 3 row of the original data
  data = df[-c(0,1,2),]
  
  #add the correct header to the data frame. Ex: UPC or Store name 
  #takes first row and makes it the column headers, but still leaves header info in the first row
  colnames(data) = as.character(unlist(data[1,]))
  
  #deletes first row of header names
  data = data[-1, ]
  
  #cols.num is holding the specific columns to convet to numeric types
  cols.num = c("UPC", "Store Nbr","Building Postal Code","WM Date" , "OH Qty" , "POS Sales" ,"POS Qty", "Unit Retail" )
  
  data[cols.num] = sapply(data[cols.num],as.numeric)
  #sapply(data, class)
  
  #delete all the rows with N/A values
  data = na.omit(data)
  
  #Keep all rows with POS Sales >= 0 (gets rid of negative values)
  data = data[data$`POS Sales` >= 0,]
  
  #Keep all rows with OH Qty >= 0 (gets rid of negative values)
  #data = data[data$`OH Qty` >= 0,]
  
  #Keep all rows with OH Qty >= 0 (gets rid of negative values)
  all_products_data = data[data$`OH Qty` >= 0,]
  
  #******First Shipment Arrival******
 
  product_name = unique(data$UPC)
  #get number of UPCs
  items_length = length(product_name)
  
  list_products = list()
  list_count = 1
  
  for(item_nbr in 1:items_length){
  
  #What does this do  
  data = all_products_data[which(all_products_data$UPC == product_name[item_nbr]),]  
     
  #Get all the store numbers
  store.id = unique(data$`Store Nbr`)
  
  #get the lenght of rows of all the stores
  n.store = length(store.id)
  
  #Create a list to put all the stores starting of the day of "fiest shipment arrival"
  data_by_store_nbr = list()
  
  j = 1
  k = 0
  first_day_store_nbr = list()
  store.count =  0
  item.count = 0
  #Loop over the whole data set and create the cleaned data set
  for (i in 1:n.store){
    
    #identify which rows have the assigned store number. Ex: Store [1] from row from  1 to 60
    lookup_store_nbr = data[which(data$`Store Nbr`== store.id[i]),]
    #lookup_store_nbr = na.omit(lookup_store_nbr)
    
    #sum all OH Qty to verify if products received any products, don't keep stores that sold no products
    judge = (sum(lookup_store_nbr$`OH Qty`) > 0)
    
    if (judge){
      #check where the first arrival happened
      first_arrival = which(diff(lookup_store_nbr$`OH Qty`)>0)[1]+1
      
      if(!is.na(first_arrival)){
        #keep all the values that start from the frist shipment until the store the last row (end of the season) of store[i] 
        
        keep_after_ship = lookup_store_nbr[first_arrival:nrow(lookup_store_nbr),]
        
        #add data to a list
        data_by_store_nbr[[j]] = keep_after_ship
        
        #increment value of the list
        j = j+1
      }
      
      else{
        
        ship_first_season_day = data[which(data$`Store Nbr` == store.id[i]),]
        first_day_store_nbr[[i]] = ship_first_season_day
        
      }
    }
  }
  
  #merge all data and create the big data set
  since_first_ship = do.call(rbind, data_by_store_nbr)
  
  #add list of when shipment arrives on first day to bottom of cleaned list
  arrival_first_season_day = do.call(rbind, first_day_store_nbr)
  
  list_products[[list_count]]= rbind(since_first_ship, arrival_first_season_day)
  
  list_count=list_count+1
  }
  
  combine_cleaned_UPCs = do.call(rbind, list_products)
  
  return(combine_cleaned_UPCs)
}

