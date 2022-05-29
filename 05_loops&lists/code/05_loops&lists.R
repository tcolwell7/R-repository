# loops & list script. 

#' The following script is to highlight and demonstrate
#' uses of lists and loops within R. 
#' I have read loops are generally discouraged wihin R
#' However, for general data analysis where repeated process are required
#' I frequently use loops in combination of lists sucessful
#' To meet frequest and varied business problems. 
#' The cobination of looping through data inserting data into a list
#' is simple and easy
#' by utilising this functionality I have reduced significant tasks
#' by writing more efficent code
#' and applying this within a loop
#' opposed to writing it all out manually

#' I will highlight how loops and lists
#' can be utilised when working with trade data
#' acorss hudreds of countries. 


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





# 3. loops & lists -------------------------------



# 4. combine list data ---------------------------



# 5. list mapping --------------------------------



# 6. mapping and openxlsx ------------------------


# 7. functions usage -----------------------------
