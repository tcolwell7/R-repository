# loops & list script. 

#' The following script is to highlight and demonstrate
#' uses of lists and loops within R. 
#' I have read loops are generally discouraged within R
#' However, for general data analysis where repeated process are required
#' I frequently use loops in combination of lists successful
#' To meet requests and varied business problems. 
#' The combination of looping through data inserting data into a list
#' is simple and easy
#' by utilising this functionality I have reduced significant tasks
#' by writing more efficient code
#' and applying this within a loop
#' opposed to writing it all out manually

#' I will highlight how loops and lists
#' can be utilised when working with trade data
#' across hudreds of countries. 


# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)

options(scipen=999)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

#upload data

trade_data <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()
tariff_data <- read_excel("..\\data\\tariff_data.xlsx")


# 1. lists ---------------------------------------

#' R list is the object which contains elements of different types 
#' like strings, vectors, dataframes
#' R list can also contain a matrix or a function as its elements. 
#' The list is created using the list() 
#' 

empty_list <- list()

# create values to insert into list:
value <- "test"
value2 <- 10
value3 <- 10*30

empty_list[1] <- value
empty_list[2] <- value2
empty_list[3] <- value3

# insert an array into a list:
array <- c(value,value2,value3)

# note outcome using this syntax:
empty_list[4] <- array

# inserting objects like arrays or dataframes require double square brackets [[x]]
empty_list[[4]] <- array

print(empty_list)

# insert dfs:
df_list <- list()
df_list[["tariff_data"]] <- tariff_data
df_list[["trade_data"]] <- trade_data


print(df_list)

# extract data from list:

df <- df_list[[1]] # first list element
df2 <- df_list[[2]] # second list element

# name elements in list:

nme <- c("data1", "data2")

print(names(df_list))

names(df_list) <- nme
print(names(df_list))

# 2. loops ---------------------------------------
--------------------------------------------------
  

#' Utilising loops when applying a repeat process 
#' Is helpful to write less code
#' Speed up the process
#' and provides a quick way to run through data
#'
#' Creating and running a simple loops is easy!


# Example: run through and identify each individual country in the trade data

# first create a basic loop using pre-defined array:
# you need to define what your variable is you wish to run through the loop. 
# this can be a simple item from an array or df. 

for(name in c("name1","name2","name3")){
  print(name)
}

array = c("name1","name2","name3")

for(name in array){
  print(name)
}

for(name in trade_data$country_name){
  print(name)
}

#' *NOTE:* 
#' when running the above - each individual row country name
#' is run through the loop. Combine this using unique:
#' 

for(name in unique(trade_data$country_name)){
  print(name)
}

#' ofcourse we could simple use 
#' print(unique(trade_data$country_name))) 
#' to acheive the same result
#' however this first code snippet is to 
#' demonstrate how to construct a simple loop. 
#' 

------------------------------------
## 2.1 loop df length --------------
------------------------------------
  

df <- head(trade_data,10)

# nrow for number of rows in df
for(i in 1:nrow(df)){
  print(i)
}

# length for number of rows in df
for(i in 1:length(df)){
  print(i)
}

# alternatively you can use dim:

for(i in 1:dim(df)[1]){
  print(i)
  print(df$commodity_code[i])
}

# 2 for column length:
for(i in 1:dim(df)[2]){
  print(i)
  print(colnames(df)[i])
}

--------------------------------------------------
### 2.2 nested loop ------------------------------
--------------------------------------------------
  
#' a loop within a loop. 

  
for(i in 1:3){
  for(j in 4:6){
    print(i)
    #print(j)
  }
}  

#' there are 3 iterations per loop. 
#' The first loop starts i = 1. 
#' There for 1 is iterated three times for 4, 5 and 6 etc. 
#' If you print the multiple of each loop iteration
#' 1 x 4 =4, 1x5 , 1x6..
#' Then 2 x4, 2x5 etc...

for(i in 1:3){
  for(j in 4:6){
    print(i*j)
  }
}
    
for(i in 1:3){
 # print(i)
  for(j in 4:6){
    print(i*j)
  }
}

#' The reason I highlight nested loops
#' Is I needed to utilise this for a work project
#' I had to filter data by a specific aggregation level of data
#' Then group this data based on 3 individual groupings.
#' I was creating a function for this
#' The best way I could think to do this was to create a nested loop
#' If there are better ways - please let me know. 
#' 

# example:
# group the data by each column (3 in total)
# for each individual unique region (5 in total)
# 3x5 loop iterations:

grouped_data <- read_excel("..\\data\\grouped_data.xlsx")

# manual list first, following by each individual unique region. 

for(group in c("region","grouping","country")){
  print(group)
  for(reg in unique(grouped_data$region)){
    
    #print(reg)
    
    df <- grouped_data %>% filter(region == reg)
    
    df_agg <- 
      df %>%
      group_by(!!as.symbol(group)) %>%
      summarise(value = sum(value))
    
    print(nrow(df_agg))
  }
}

---------------------------------------------------
# 3. loops & lists --------------------------------
---------------------------------------------------

  
#' I first started using loops when I didn't want to filter 
#' an excel spreadsheet 20 times for 20 different data cuts
#' I looped through this data filtering each cut
#' And saved each cut into a single excel output
#' saving me that new hassle
#' and learning a new skill in the process. 
#' This skill of combining loops and storing the data within lists
#' I use all the time still to this day.
#' *If there are better ways to achieve the same result please get in touch!* 
#'
#' Combining loops with lists are simple and easy. 
#' For example - loop through a country level dataset:

df <- trade_data %>% group_by(country_name) %>% summarise(value = sum(value_gbp))

# create empty list:
empty_list <- list()

for(country in df$country_name){
  
  df2 <- df %>% filter(country_name == country)
  
  empty_list[[country]] <- df2
  
}

#' as simple as that. 
#' You can create your own naming convetion by adding in an array. 
#' Rather than using the loop variable (in this exmaple country)

#' the code within the loop can be complicated as desired. 
#' Although this is generally discouraged as run time is slower. 
#' The purr package allows for transforming data 
#' within lists through the use of functions 
#' 

#however a further example. Aggregate each country by year and flow

list <- list()

for(country in unique(trade_data$country_name)){
  
  # filter data for country
  df <- trade_data %>% filter(country_name == country)
  
  # aggregate filtered data
  df2 <- df %>%
    group_by(
      country_name,
      year,
      flow
      ) %>%
    summarise(
      value = sum(value_gbp),
      avg = mean(value_gbp)
     
     #,.groups = "drop" # unfilter .groups option to see what this does when re-ran
    )
  # customer name for list:
  nm <- paste0("df_",country)
  
  list[[nm]] <- df2
  
}

#' this was a straight forward data transformation. 
#' However if the code is more complex is is advisable 
#' To apply a function to the data within a list.
#' So you would only filter and export the data to the list. 
#' Then apply the function etc.
#' 


## 3.1 extract list data ----------------------------
-----------------------------------------------------
  
#' As shown in section 1. 
#' it is simple to individually extract data
#' utilising a single funciton you can extract all dfs
#' into the global environment
#'


list2env(list,globalenv())


### 3.2 combine list data ---------------------------
-----------------------------------------------------

#' You can individually extract each df
#' Then combine or manioulate data in however you see fit
#' A common use of these method is 
#' I bind the df rwos together into a single df. 
#' 

# using bind_rows. 

df_combined <- bind_rows(list)

# using do.call

df_combined2 <- do.call(rbind, list)

# 4. list mapping --------------------------------

#' You can manipulate data within lists
#' You can iterate through each data item using Map
#' You can apply functions to data stored in lists
#' To help avoid over use of complicated loops. 
#' lapply
#' mapply
#' sapply
#' purr
#' *TBC*


Map(function(data,name){
  
  print(name)
 #print(nrow(df))
}, list, names(list))


# 5. mapping and openxlsx ------------------------

#' combining Map and openxlsx to automate spreadsheet creation.

#' This combination of using Map, lists and spreadsheet automation
#' was my first real experience of applying these methods
#' to solve and produce outputs
#' used by other teams
#' created using automation
#' 



#' Task: create a spreadsheet for all country data formatted. 
#' This can be done manually ofcourse. But it is much easier in the long run
#' to learn a method to automate this process. 


# create filtered data and store within list

list <- list()

for(country in unique(trade_data$country_name)){
  
  df <- trade_data %>% filter(country_name == country)
  
  list[[country]] <- df
  
}

# all country data has been filtered for each unique country
# and stored within a list

#' for openxlsx tip - please see appropriate workbook stored in folder 2.
 
wb <- createWorkbook()

# combine Map with user defined Function:
  
  Map(function(data, name){
    
    # strip name down to less strings
    
    name <- str_sub(name,1,30)
    
    addWorksheet(wb, name)
    
    rowNo <- nrow(data)
    colNo <- ncol(data)
    
    writeData(wb, 
              sheet = name, 
              data, 
              withFilter = TRUE,
              startRow = 2, 
              startCol = 1) # set row to 2 to insert merged cell in row 1 for header title. 
    
    # 0. set column widths:
    setColWidths(wb, sheet = name, cols = 1:colNo, width = 15)
    
    # 1. create border style:
    borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD")
    
    # 2. create headerStyle:
    headerStyle <- createStyle(fontSize = 12, 
                               fontColour = "#FFFFFF", 
                               halign = "center",
                               fgFill = "#4F81BD", 
                               border="TopBottom", 
                               borderColour = "#4F81BD", 
                               wrapText = TRUE, 
                               textDecoration = "bold")
    
    # 3. Add merged cell header: (one row merged across all columns 1:7). 
    # first write header title
    
    headerTitle <- paste0("Total trade between the UK and ", name)
    
    writeData(wb, name, headerTitle, 
              startCol = 1, 
              startRow = 1, 
              borders="surrounding", 
              borderColour = "black")
    
    mergeCells(wb, name, cols = 1:colNo, rows = 1)
    
    firstRow <- createStyle(fontSize = 14, 
                            halign = "center", 
                            border = "TopBottomLeftRight", 
                            textDecoration = "bold", 
                            borderStyle = "thick")
    
    # 4. Create number formatting:
    
    # the same border styles are required, otherwise the cells this style applies to resets to default. 
    numStyle <- createStyle(numFmt = "#,##0", 
                            border = "TopBottom", 
                            borderColour = "#4F81BD")
    
  
    # 5. freezepane:
    
    freezePane(wb, name, firstActiveRow = 3, firstActiveCol = 1)
    
    # Add styles:
    addStyle(wb, sheet = name, borderStyle, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
    addStyle(wb, sheet = name, headerStyle, rows = 2, cols = 1:colNo)
    addStyle(wb, sheet = name, numStyle, rows = 3:rowNo, cols = 6, gridExpand = T)
    addStyle(wb, sheet = name, firstRow, rows = 1, cols = 1:colNo, gridExpand = T)
    
    
  }, list, names(list))
  
  
  saveWorkbook(wb, file = "..\\outputs\\mapped_country_xl.xlsx", overwrite = TRUE)
  
  
  
# 6. functions usage -----------------------------

#' 
#' Loops and lists can be utilised within functions. 
#' Loops may be needed to iterate through data
#' Lists may be needed if there are multiple outputs
#' created within the function whcih need calling. 
#' 


  
#' There have been multiple instances where I have created functions
#' where I require multiple outputs
#' when returning one dataframe you can easily use "return"
#' for multiple items to be returned I am unaware how to do this
#' outside of storing items in a list - then calling them outside of the function
#' 

  
#' Example: 
#' filter data in loop and store in list
#' aggregate data and store in list

list <- list()
  
func <- function(country){
  

   df <- trade_data %>% filter(country_name == country)
   
   list[[country]] <- df
   
   df_agg <- trade_data %>% 
     filter(country_name == country) %>%
     group_by(country_name, year, flow) %>%
     summarise(value = sum(value_gbp))
   
   list[["country_agg"]] <- df_agg
   
   return(list) # return list where items are stored
   
 }
  
output_list <- func("Thailand")
  
# returns two elements which you cna then call:

df <- output_list[["Thailand"]]
df_agg <- output_list[["country_agg"]]


# Example 2:
#' create function to store looped trade data and return list of stored items
#' funciton filter is for trade flow:

func2 <- function(.flow){
  
  list <- list()
  
  for(country in unique(trade_data$country_name)){
    
    df <- trade_data %>% filter(country_name == country) %>%
      filter(flow == .flow)
    
    list[[country]] <- df
    
  }
  
  return(list)
  
}

output_list2 <- func2("Exports")

#' output is all countries filtered from main trade data set. 
#' you can then call on specific countries
#' or export all into global environment etc. 
#' this method is to demonstrate
#' use of a loop within a function to store multiple data items
#' 
#' 

# end

