#-------------------
#Project Members: Bria Garcia,Alejandro Torrico, Brooke Larkin , Hector Ramos, Ty Matindale
#Faculty Advisor: Dr. Ed Polh
#Industry Partners: Raegon Barnes and Willie Nelson
#-------------------

#library(dplyr)
#library(shiftR)
#library(data.table)

#set working directory for personal computer
#wd = "~/Box/Capstone Hershey/Deliverable/Draft_Brooke/Shiny/Functions"
#setwd(wd)

#establish source and store inputs in variable files
#source("cleandata2.R")
#big_file = "VAL2018_Sales_Seasonal Adhoc.xlsx"
#input_file = "VAL2018_Sales_1500rows.xlsx"
#two_items = "EAS_2017_TWO_Items.xlsx"
#method3_test = "Easter 2018 Method 3.xlsx"
#graph_test = "Bar graph data.xlsx"

Not_Enough_OH_Method = function(input_file){
  
  mtd2_time_start = proc.time()
 # incProgress(1/11, message = "Analyzing Not Enough OH MSO")
 # all_products_data = clean.data (graph_test)
  all_products_data = input_file
  product_name = unique(all_products_data$UPC)
  #get number of UPCs
  items_length = length(product_name)
  
  
  #**********Make Index and Day Average Tables!******************
  
  list_products = list()
  j=1
  
  for(item_nbr in 1:items_length){
    
    adhoc_data = all_products_data[which(all_products_data$UPC == product_name[item_nbr]),]
    
    #Add column to track if a store made a sale
    adhoc_data["Store Count"] = ifelse(adhoc_data$`POS Sales` >= 1, 1, 0)
    
    #Sum POS Sales by store number
    Store_POS = aggregate(list(POS_sum = adhoc_data$`POS Sales`), by=list(Store.Nbr=adhoc_data$`Store Nbr`), FUN=sum)
    
    #Take the average of individual store POS totals to get the total average of all stores for the season
    Total_Store_Average = mean(Store_POS$POS_sum)
    
    #Add Store Index calculation to Store_POS table
    Store_POS["Store Index"] = Store_POS$POS_sum / Total_Store_Average
    
    #Create table that sums POS Sales and Stores by weekday
    Weekday_POS = aggregate(list(POS_Qty = adhoc_data$`POS Qty`, Store_Count = adhoc_data$`Store Count` ), by=list(Weekday=adhoc_data$`WM Date`), FUN=sum)
    
    #Add column to calculate the average POS Sales per day of the week
    Weekday_POS['POS Average per Day'] = Weekday_POS$POS_Qty / Weekday_POS$Store_Count
    
    
    #**********Find Missed Sales Opportunities!*********************
    
    #Get the length of stores by their id numbers
    n.store = length(unique(adhoc_data$`Store Nbr`))
    
    #Get all ID store numbers
    store.id = unique(adhoc_data$`Store Nbr`)
    
    #Get the length of WM Dates by their id numbers
    n.date = length(unique(adhoc_data$`WM Date`))
    
    #Get all ID WM Date numbers
    date.id = unique(adhoc_data$`WM Date`)
    
    #Initialize 2D array 
    output = array(0, dim=c(n.store, n.date))
    
    #Iterates on all stores and create array of missed opportunities
    for (i in 1:n.store){
      
      #Identify which rows have 
      case = which(Store_POS$Store.Nbr == store.id[i])
      
      #Get Index of of Store POS
      index.extract = Store_POS$`Store Index`[case]
      
      
      for (j in 1:n.date){
        #Locate the respective date from the Week Average table 
        case = which(Weekday_POS$Weekday == date.id[j])
        
        #Get the respective value
        Week_avg = Weekday_POS$`POS Average per Day`[case]
        
        #2D array of Missed sales Opportunities
        output[i,j] = Week_avg * index.extract
      }

    }
    
    #Create a new column for missed sales opportunities (MSO)
    Expected_Performance = array(0, dim=c(n.store, n.date))
    adhoc_data$Expected_Performance = 0
    adhoc_data$MSO_Qty = 0
    
    for (i in 1:nrow(adhoc_data)){
      
      #Get the desired store numb abd date
      store.target = adhoc_data$`Store Nbr`[i]
      date.target = adhoc_data$`WM Date`[i]
      
      #Find the desired vlue of store numb and date
      case1 = which(store.id == store.target)
      case2 = which(date.id == date.target)
      
      #Populate the MSO colum with the respective value
      adhoc_data$Expected_Performance[i] = output[case1, case2]
      
    }
    

    adhoc_data$MSO_Qty= (adhoc_data$Expected_Performance - adhoc_data$`OH Qty`)
    
    #Get rid of rows from adhoc_data that have enough OH Qty to meet Expected Performance
    miss_op_table = adhoc_data[adhoc_data$MSO_Qty > 0 , ]
    miss_op_table = na.omit(miss_op_table)

    #Assign MSO Dollars column to be filled with zero
    miss_op_table$MSO = 0
    
    #Calculate MSO Dollars
    miss_op_table$MSO = miss_op_table$MSO_Qty * miss_op_table$`Unit Retail`
    
    miss_op_table$'Store Count' = NULL
    list_products[[j]] = miss_op_table
    j = j + 1
    
  }
  
  
  combined_products = do.call(rbind, list_products )
  combined_products = na.omit(combined_products)
  
  #Calculate calcualtion time Mtd2
  mtd2_time_end = proc.time() - mtd2_time_start
  print("Total time to Calculate Method 2: ")
  print(mtd2_time_end)
  #Return only the missed sales opportunities table
  return(combined_products)
  
}
