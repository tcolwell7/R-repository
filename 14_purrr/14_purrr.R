
# purrr functional prgramming script

#' I have been learning new techniques in R and it had often been discussed
#' to look into the purrr package to reduce the need to use loops
#' I have read using lapply/purrr style mapping opposed to loops
#' is far more efficient 
#' I have tested this out using lapply/mapply etc and now
#' I want to test this using the popular purr package
#' and become more comfortable coding in a more functional programming way
#' 
#' 

#' https://purrr.tidyverse.org/
#' "purrr enhances R’s functional programming (FP) 
#' toolkit by providing a complete and consistent 
#' set of tools for working with functions and vectors. 
#' If you’ve never heard of FP before, 
#' the best place to start is the family of map() 
#' functions which allow you to replace many for loops 
#' with code that is both more succinct and easier to read. T
#' he best place to learn about the map() functions is the iteration chapter
#'  in R for data science.
#' https://r4ds.had.co.nz/iteration.html#the-map-functions




# set up -----------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)
library(ggplot2)
library(purrr)
library(tictoc)
library(data.table)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # 


# load data

load("data/Trade_datasets.RData")
agg_data <- read_excel("data/agg_data.xlsx")



# map functions ----------------

#' Apply functions using purrrs map 
#' Map functions apply a function to each element 
#' within a list or vector 
#' 


# Example: iterate through trade data set for all countries and sum total trade:

country_list <- unique(trade_annual$country)

# function to filter four country and sum total trade for all years
sumFunc = function(.country){
  sum = trade_annual %>% filter(country == .country) %>% summarise(value=sum(total))
}

# using map function apply the sum to all countries in unique list
sum_list <- purrr::map(country_list, .f = sumFunc)

# output is a list. 

# use the names function to re-name elements within the list

names(sum_list) <- country_list

# to create a df output you can use the map_dfr function

sum_df <- purrr::map_dfr(country_list, .f = sumFunc)
sum_df_wide <- purrr::map_dfc(country_list, .f = sumFunc) # creates df with wide format of elements


# map multiple lists to a function

# map2
#' note the two input vectors need to be the same length. 

#' create function to sum data and create column

country_list <- unique(trade_annual$country)
country_list_iso <- unique(trade_annual$iso2)

sumFunc2 <- function(.country, .iso){
  
  sum = trade_annual %>% 
    filter(country == .country) %>% 
    summarise(value=sum(total)) %>%
    mutate(iso = .iso)
  
}

sum_list2 <- purrr::map2(country_list, country_list_iso, .f = sumFunc2)
sum_df2 <- purrr::map2_dfr(country_list, country_list_iso, .f = sumFunc2)

#' you can create a multi input function and apply one input list 
#' 

test_function <- function(.country, .data, .year){
  
  sum = .data %>% 
    filter(year == .year) %>%
    filter(country == .country) %>% 
    summarise(value=sum(total)) %>%
    mutate(Year = .year)
  
}

test_multi_input <-
  purrr::map(
    country_list, 
    test_function, 
    .data = services_annual, 
    .year = 2018
    )
  

## speed test vs loop ----------

#' same process of summing df ran through loop:

tic()
list <- list()
for(.country in country_list){
  
  .iso = trade_annual %>% 
    distinct(country, .keep_all = TRUE) %>% 
    filter(country==.country) %>% pull()

  sum = trade_annual %>% 
    filter(country == .country) %>% 
    summarise(value=sum(total)) %>%
    mutate(iso = .iso)
  
  list[[.country]] <- sum
  
}

x = list %>% bind_rows
toc()


tic()
iso_list <- unique(trade_annual$iso2)
sumFunc2 <- function(.country, .iso){
  
  sum = trade_annual %>% 
    filter(country == .country) %>% 
    summarise(value=sum(total)) %>%
    mutate(iso = .iso)
  
}
xx <- purrr::map2_dfr(country_list, iso_list, .f = sumFunc2)
toc()


#' *faster when using two input lists. When apply a simple function to iterate through the speed was around the same*
#' 


## apply function to list ----------


#' the previous example was applying a function to a list of characters applying this to a df
#' equally you can use the map functions to apply a function to elements within a list
#' for example a list containing dataframes, then apply an aggregation function to this data
#' 

#' *example* list containing all trade data for individual countries - then aggregate this data and create an output
#' 

trade_data_list <- map(country_list,function(x){trade_annual %>% filter(country==x)})

# list of each country within the trade dataset

#' aggregate all data within the list 
#' 

trade_data_list2 <- map(trade_data_list,function(x){x %>% group_by(country) %>% summarise(value=sum(total))})


# nested data ------------


tbc

#' other function within purrr tbc when I explore mroe of this package. 

