#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#library(readxl)
#library(shiftR)


#wd = "~/Box/Capstone Hershey/Deliverable/Draft_Brooke"
#setwd(wd)

#big_file = "VAL2018_Sales_Seasonal Adhoc.xlsx"
#input_file = "VAL2018_Sales_1500rows.xlsx"
#two_items = "EAS_2017_TWO_Items.xlsx"
#input_file = "Bar graph data.xlsx"
#input_file = "HOL2018_One_Item.xlsx"

#----------------------
clean.data = function(input_file, file_type){
  
  clean_time_start = proc.time()
 # incProgress(1/11, message = "Reading Data")
  
  if(file_type == "xlsx"){
  #Read the excel file
      df = read_excel(input_file)
      #incProgress(1/11, message = "Cleaning Data")
      #Delete the first 3 row of the original data
      data = df[-c(0,1,2),]
      
      #add the correct header to the data frame. Ex: UPC or Store name 
      #takes first row and makes it the column headers, but still leaves header info in the first row
      colnames(data) = as.character(unlist(data[1,]))
      #deletes first row of header names
      data = data[-1, ]
      #data["WM Date"] = sapply(data["WM Date"],as.numeric)
  }else if(file_type == "csv"){
    #data =  read.csv2(input_file, quote = "\"", sep = ",", header = T)
    data["WM Date"] <- as.Date(data$`WM Date`, format = "%m/%d/%Y")
  }else if(file_type == "txt"){
    data =  read.table(input_file, quote = "\"", sep = ",", header = T)#,fileEncoding="UTF-16LE")
    
  }
  
  
  #Delete Unnecesary columns!
  data$NetShip_Qty = NULL
  data$NetShip_Cost = NULL
  
  #make sure the DF have the right names
  col_name = c("UPC","HSY Item Description","HSY Seasonal Segmentation","Store Nbr",
               "Store Name","Building City","Building State/Prov","Building Postal Code",
               "Store Type","WM Date","SeasonAndYear","OH Qty","POS Qty","POS Sales")
  
  colnames(data) <- col_name
  #cols.num is holding the specific columns to convert to numeric types
  cols.num = c("UPC", "Store Nbr","Building Postal Code", "OH Qty" , "POS Sales" , "POS Qty")
  #cols.num = colnames(data)[1,3,5]

  data[cols.num] = sapply(data[cols.num],as.numeric)
  data["WM Date"] <- as.Date(data$`WM Date`, format = "%m/%d/%Y")

  
  
  #delete all the rows with N/A values
  data = na.omit(data)
  
  #Keep all rows with POS Sales >= 0 (gets rid of negative values)
  data = data[data$`POS Sales` >= 0, ]
  
  #Keep all rows with OH Qty >= 0 (gets rid of negative values)
  #data = data[data$`OH Qty` >= 0,]
  
  #Keep all rows with OH Qty >= 0 (gets rid of negative values)
  all_products_data = data[data$`OH Qty` >= 0,]
  
  #++++++++++++++++++++++++++++++++
  #          Use multiple cores
  #++++++++++++++++++++++++++++++++
  
  numCores <- detectCores()
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  #******First Shipment Arrival******
 
  product_name = unique(data$UPC)
  #get number of UPCs
  items_length = length(product_name)
  
  list_products = list()
  list_count = 1
  
  dummy = foreach(item_nbr = 1:items_length) %dopar% {
       # for(item_nbr in 1:items_length){
        
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
        
        list_products = rbind(since_first_ship, arrival_first_season_day)
        
      #  list_count=list_count+1
        return(list_products)
  }
  
  combine_cleaned_UPCs = do.call(rbind, dummy)
  
  #calculate total time spend
  clean_time_end = proc.time() - clean_time_start
  print("Total Time for Cleaning data: ")
  print(clean_time_end)
  
  return(combine_cleaned_UPCs)
}

