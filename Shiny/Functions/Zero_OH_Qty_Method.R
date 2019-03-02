library(dplyr)
library(shiftR)


#wd = "C:/Users/atorr/Box/Capstone Hershey/Deliverable/Draft_Alejandro/Shiny/Functions"

#setwd(wd)

#source("cleandata.R")
#input_file = "Val2018_Sales_1500rows.xlsx"
#big_file = "VAL2018_Sales_Seasonal Adhoc.xlsx"




Zero_OH_Qty_Method= function(input_file){
  
    adhoc_data = clean.data (input_file )
    
    
    #**********Make Index and Day Average Tables!******************
    
    
    #Add column to track if a store made a sale
    adhoc_data["Store Count"] = ifelse(adhoc_data$`POS Sales` >= 1, 1, 0)
    
    #Sum POS Sales by store number
    Store_POS = aggregate(list(POS_sum = adhoc_data$`POS Sales`), by=list(Store.Nbr=adhoc_data$`Store Nbr`), FUN=sum)
    
    
    #Take the average of individual store POS totals to get the total average of all stores for the season
    Total_Store_Average = mean(Store_POS$POS_sum)
    
    #Add Store Index calculation to Store_POS table
    Store_POS["Store Index"] = Store_POS$POS_sum / Total_Store_Average
    
    #Create table that sums POS Sales and Stores by weekday
    Weekday_POS = aggregate(list(POS_Sales = adhoc_data$`POS Sales`, Store_Count = adhoc_data$`Store Count` ), by=list(Weekday=adhoc_data$`WM Date`), FUN=sum)
    
    #Add column to calculate the average POS Sales per day of the week
    Weekday_POS['POS Average per Day'] = Weekday_POS$POS_Sales / Weekday_POS$Store_Count
    
    #New table that only shows missed sales opportunities
    miss_op_table = adhoc_data[adhoc_data$`OH Qty` == 0, ]
    
    
    
    #**********Find Missed Sales Opportunities!*********************
    
    #Get the length of stores by their id numbers
    n.store = length(unique(miss_op_table$`Store Nbr`))
    
    #Get all ID store numbers
    store.id = unique(miss_op_table$`Store Nbr`)
    
    #Get the length of WM Dates by their id numbers
    n.date = length(unique(miss_op_table$`WM Date`))
    
    #Get all ID WM Date numbers
    date.id = unique(miss_op_table$`WM Date`)
    
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
      #print(i)
    }
    
    #Create a new column for missed sales opportunities (MSO)
    miss_op_table$MSO = 0
    
    for (i in 1:nrow(miss_op_table)){
      
      #Get the desired store numb abd date
      store.target = miss_op_table$`Store Nbr`[i]
      date.target = miss_op_table$`WM Date`[i]
      
      #Find the desired vlue of store numb and date
      case1 = which(store.id == store.target)
      case2 = which(date.id == date.target)
      
      #Populate the MSO colum with the respective value
      miss_op_table$MSO[i] = output[case1, case2]
    }

    
    #Return only the missed sales opportunities table
   return(miss_op_table)
}