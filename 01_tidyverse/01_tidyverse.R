## 02 Tidyverse function examples. 

## /// Day to day functions I use using tidyverse for data wrangling, transformaitons, cleaning and aggregaiton to produce useable outputs. 

## function areas to cover: Selecting columns, creating columns, filtering, aggregations, duplicates, seperating delimiters. 

# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

#upload data

trade_data <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()
tariff_data <- read_excel("..\\data\\tariff_data.xlsx")

## 1.  Select -------

# /// There are multiple ways to select columns within a dataframe:
##    1. Base R
##    2. Select using column Index
##    3. Select using column names
##    4. Select using combination 

# //// Select the commodity code, MFN and preferential tariff columns. 

# 1. Using Base R and Index numbers (time consuming searching for each number manually

print(colnames(tariff_data))

df <- tariff_data[,c(2,4,5,6,7,8,9,18)]
# OR //
df <- tariff_data[,c("commodity_code",
                     "commodity_code_description","mfn_applied_duty_rate",
                     "preferential_applied_duty_rate_2021",
                     "preferential_applied_duty_rate_2022",
                     "preferential_applied_duty_rate_2023",
                     "preferential_applied_duty_rate_2024",
                     "mfn_applied_rate_ukgt")]


# Select function:

df <- tariff_data %>% select(2,4,5,6:9,18)


df <- tariff_data %>% select(starts_with("commodity_code"),
                             starts_with("preferential"), # This captures an unwanted column "preferential_applied_duty_rate_excluded"
                          #  starts_with(c("commodity_code","preferential")), 
                             contains("mfn"))


df <- tariff_data %>% select(starts_with("commodity_code"),
                             contains("mfn"),
                             num_range("preferential_applied_duty_rate_",2021:2024))

# remove MFN columns

df <- df %>% select(-contains("mfn"))
# OR
df <- df %>% select(!contains("mfn"))
# select 2022 columns:
df <- tariff_data %>% select(commodity_code, ends_with("2022"))

# select numerical columns:
df <- tariff_data %>% select(commodity_code,where(is.numeric))

# select non-numerical columns:
df <- tariff_data %>% select(commodity_code, !where(is.numeric))

### 1i. Relocate ------

# relocate select column positioning based on other column location. For example you want to relocate a column before or after another. 
print(colnames(tariff_data))


# You can relocate columns manually using select: i.e. select(1,2,4,3,5,6... ) etc. 
# This can be laborious and inefficient (typing each column out) 
# and prone to error if the df order changes if using column indexing. 


df <- tariff_data %>% select(starts_with("commodity_code"),
                             contains("mfn"),
                             num_range("preferential_applied_duty_rate_",2021:2024),
                             where(where(is.numeric)))
  
  
df2 <- df  %>% relocate(x8_digit_or_10_digit, .after = "commodity_code")
df2 <- df2 %>% relocate(cn8_count, .after = "x8_digit_or_10_digit")
df2 <- df2 %>% relocate(value_usd, .before = "commodity_code_description")
  
df2 <- 
  df %>%
  relocate(c("x8_digit_or_10_digit","cn8_count"), .after = "commodity_code")

# relocate numerical only columns:

df2 <-
  df %>%
  relocate(where(is.numeric), .before = "commodity_code_description")


## 2. Mutate ---------

# create and update columns using mutate

df2 <- df  %>% mutate(value_gbp = value_gbp / 1000000)
df2 <- df  %>% mutate(value_gbp2 = value_gbp / 1000)
df2 <- df2 %>% mutate(newCol = paste0(commodity_code,"_",year))


# select specific column and drop non-numeric characters 
df3 <- df2 %>% mutate(commodity_code = parse_number(commodity_code))

df3 <- df2 %>% mutate_at(c("commodity_code", "newCol"), .funs = parse_number) 

# INSTEAD OF:

df3 <- df2 %>% mutate(commodity_code = parse_number(commodity_code),
                      newCol         = parse_number(newCOl))

# convert numeric columns to character
df3 <- df2 %>% mutate_if(is.numeric, .funs = as.character)
# OR
df3 <- df2 %>% mutate(across(where(is.numeric), ~ as.character(.x)))
               
df3 <- df2 %>% mutate(across(where(is.numeric), ~ .x * 10000))


### 2i. Mutate across variables -----
## combining mutate > across > where is very useful and efficient when applying functions to multiple columns.  
 

df <- 
  tariff_data %>% 
  select(starts_with("commodity_code"),
         mfn_applied_duty_rate,
         num_range("preferential_applied_duty_rate_",2021:2024))

# want to convert tariffs to numeric. Need to remove '%'
# This can be done using str_remove in the stringr package. 

df2 <-
  df %>%
  mutate(
    preferential_applied_duty_rate_2021 = 
      str_remove(preferential_applied_duty_rate_2021, "%"),
    preferential_applied_duty_rate_2022 = 
      str_remove(preferential_applied_duty_rate_2022, "%"),
    preferential_applied_duty_rate_2023 = 
      str_remove(preferential_applied_duty_rate_2023, "%"),
    preferential_applied_duty_rate_2024 = 
      str_remove(preferential_applied_duty_rate_2024, "%"))

# single string function to remove "%"
stringRemove <- function(.x)(str_remove_all(.x,"%"))

df2 <- df %>% mutate_at(c("preferential_applied_duty_rate_2021",
                          "preferential_applied_duty_rate_2022"), .funs = stringRemove)

df2 <- df %>% mutate_if(is.character, .funs = stringRemove)

# combine mutate at (select specific columns) with pattern recognition:
df2 <- df %>% mutate_at(vars(contains("pref")), .funs = stringRemove)
df2 <- df %>% mutate_at(vars(contains("mfn")),  .funs = stringRemove)

# alternatively if columns don't have a single patterns:

df2 <- 
  df %>% 
  mutate(
    across(.cols = 
      c(preferential_applied_duty_rate_2021:preferential_applied_duty_rate_2024), 
    ~ str_remove_all(.x, "%")))

## reduce notation using column index: (.cols is optional, function works without specifying)

df2 <-
  df %>%
  mutate(
    across(.cols = c(4:7), ~ str_remove_all(.x, "%"))
  )

## remove % from all tariff columns and covert to numeric and calculate average tariff. 

df2 <-
  df %>%
  mutate(
    across(.cols = -c(1:2), ~ str_remove_all(.x, "%"))
  ) %>%
  mutate(
    across(-c(1:2), ~ as.numeric(.x))
  ) %>%
  mutate(
    average_tariff = rowMeans(across(-c(1:2)), na.rm = T))
                      
  

## 3. Filter -----

# Filter data sets referring to pre-calculated values, arrays and text. 

filterValue = 1000000

df <- trade_data %>% filter(value_gbp >= 200000)
df <- trade_data %>% filter(value_gbp >= mean(value_gbp, na.rm = T))
df <- trade_data %>% filter(value_gbp < filterValue)


# filter using AND | OR opoerators. 





# filter df based on commodity code list. 

codeList <- c("01012100", "02031913","02032290","03071190","04063010","11071099")

df <- trade_data %>% filter(commodity_code %in% c("01012100","48084000"))
df <- trade_data %>% filter(commodity_code %in% c("01012100","48084000"))

# filter based on codes *not* being in list:

#custom negate function(
  
`%notin%` <- Negate(`%in%`) # Custom negate function

df2 <- trade_data %>% filter(commodity_code %notin% codeList)
df2 <- trade_data %>% filter(year %notin% c("2020"))

# you cna use not in / in base don another dataframes column:

df <-  head(trade_data,10)
df2 <- head(trade_data,30)

# filter df2 if codes in df1 
df3 <- df2 %>% filter(commodity_code %in% df$commodity_code)
df3 <- df2 %>% filter(commodity_code %notin% df$commodity_code)

# logical filtering:



# conditional formatting:
# filter data based on select input:
# For example, if a user whats to filter based on hs2, hs4 etc. 

# create hs columns:
df <- trade_data %>% 
  mutate(hs2 = str_sub(commodity_code,1,2),
         hs4 = str_sub(commodity_code, 1, 4), 
         hs6 = str_sub(commodity_code, 1, 6))

### 3i. Conditional filtering ----

input <- "hs2"
code <- "02"
  
  
df4 <- df %>%
  {if(input == "hs2") filter(., hs2 == code)
    else if(input == "hs4") filter(., hs4 == code)
    else if(input == "hs6") filter(., hs6 == code) else .}

## I previously used:

if(input == "hs2"){
df4 <- df %>% filter(hs2 == code)
} else if(input == "hs4"){
  df4 <- df %>% filter(hs4 == code)
} else if(input == "hs6"){
  df4 <- df %>% filter(hs6 == code)
} else{
  df
}

## \\ outcome with method one cleaner and more concise code! 

## 4. Aggregations ------

## // You can easily aggregate and group data for data transformations and calculations. 

# group_by. 
# and summarise combined. 

# The trade dataset is a commodity code level dataset and has a hierarchy of:
# Commodity code > country >  flow > year

# You can chose the hierarcy level within the dataframe you want to perform calculations on.
## For example: year level calculations - country - flow. 

### 4i. Group_by/summarise ----
# year level df:
df <- trade_data %>% group_by(year)

# grouping doesn't change how the data looks (apart from listing
# how it's grouped):
# need to perform other dplry functions like summarise:
df <- 
  trade_data %>% 
  group_by(year) %>%
  summarise(total_value = sum(value_gbp), # sum total column
            mean_value  = mean(value_gbp),
            count_code  = length(commodity_code),
            max_value   = max(value_gbp),
            min_value   = min(value_gbp))

# You can easily concatenate multiple rows into one cell using group_by and summarise:

# alternatively if you want the whole dataframe values you can use summarise to calculate this:

df <- trade_data %>% 
  summarise(total_value = sum(value_gbp), 
            mean_value = mean(value_gbp))


### 4i. "Sumifs" method ----

# Calculate year values for "Taiwan" using base R filtering. 

df <- 
  trade_data %>% 
  group_by(year) %>%
  summarise(
    total_value = sum(value_gbp[country_name == "Taiwan"]), # sum total column for Taiwan. 
    mean_value  = mean(value_gbp[country_name == "Taiwan"])
    )
           

#### 4ii. Concatenate cells ------

# example data:

df <- tariff_data %>% mutate(hs4 = str_sub(commodity_code, 1,4)) %>% head(30)

df2 <- df %>% 
  group_by(hs4) %>%
  summarise(combined_mfn_tariff = paste0(mfn_applied_duty_rate, collapse = " ; "))
  

##### 4iii. Unnest -----------
## ungroup a concatonated cell: -


df3 <- df3 %>% 
  mutate(
    combined_mfn_tariff = 
      strsplit(
        as.character(combined_mfn_tariff), ";")
    ) %>% 
  unnest(combined_mfn_tariff)

# seperate_rows:





###### 4iii. Ungroup ----

# When working with group dataframes - when attempting to create dataset level calculations the grouping can skew this. 
# To remove the grouping in any data set use ungroup()
#
# When working with a multi-grouped data frame - when trying to calculate a new measure using an entire columns values - results incorrect calculations.
# 
# For example if you wanted to find the top ranked countries in a multi-goruped df it will produce incorrect rank porducing a single value. 
#
# ungrouping after the aggregation fixes this.
# For example // aggregated the dataframe to highlight each year and countries total trade values. 


df <- trade_data %>%
  group_by(country_name, country_code) %>%
  summarise(total_value = sum(value_gbp)) %>%
  mutate(rank_country = rank(total_value))

##  using upgroup:

df <- trade_data %>%
  group_by(country_name, country_code) %>%
  summarise(total_value = sum(value_gbp)) %>%
  ungroup() %>%
  mutate(rank_country = rank(-total_value))



## 5. Left join / binding ------

### 5i.  Joins ----

# left join: two dfs x and y:
# i.e. match everything from y to x - using unique identifier(s). 
df <- head(trade_data,50)
df2 <- head(trade_data,50)


df3 <- df %>% left_join(df2, by = "commodity_code")
df3 <- df %>% left_join(df2, by = "year")
df3 <- df %>% left_join(df2, by = "country_name")


df3 <- df %>% left_join(df2, by = c("commodity_code" = "commodity_code",
                                    "year" = "year",
                                    "country_name" = "country_name"))


# inner join: 
# i.e. match and return everything between x and y using using unique identifier

df <- head(trade_data,50)
df2 <- head(trade_data,20)

df3 <- df %>% inner_join(df2, by = "commodity_code")

# full join:
# i.e. combine everything in x and y into one df

df3 <- df %>% full_join(df2, "commodity_code")

#### 5ii. rbind/cbind ------
#
# Binding dataframes together whther by rows
# i.e. append two dataframes on top of each other - OR
#      append two dataframes side by side
#
#

# r bind (row bind)
# r bind will only work if each dataframe has the same named columns. 
# if there are additional columns or they are named differently - rbind will not work. 


df3 <- rbind(df,df2)

# c bind (column bind)
# c bind will only work if each dataframe has the same number of rows 
# if there are additional or less rows - cbind will not work. 

df2 <- tail(trade_data,50)

df3 <- cbind(df,df2)

# df3 now combined the top 50 and bottom 50 rows side by side.

##### 5iii. bind_rows/bind_cols -------


#' bind rows:
#' Similar to rbind the purpose is to append rows of multiple dfs
#' bind rows output will contain any column which appears in any input
#' eg. If a column exists in df2 but not df1 the output
#'  from bind_rows will contain the extra column from df2
#'  This is how and why I first discovered this function and continue to use
#'  bind_rows over rbind
#'  I often use rbind if I want that extra assurance check that column names are the same
#'  and all column names are the same between dfs. 
#'

df  <- head(trade_data,50)
df2 <- tail(trade_data,50)

df3 <- bind_rows(df,df2)

# extra column:
df2 <- mutate(df2, value_gbp2 = value_gbp * 10)

df3 <- bind_rows(df,df2)
print(colnames(df3))

#' bind_rows when working with lists is very useful and something I use alot
#' 

list <- list()
list[["df"]] <- df
list[["df2"]] <- df2

df3 <- bind_rows(list)

#' I often insert multiple dataframes into a list 
#' with the purpose of binding the rows together
#' so use this method alot
#' This can quickly be combined with pipes and other functions:


df3 <- bind_rows(list) %>% filter(flow == "Exports") # etc. 

## 6. Other operators ------------
### 6i. distinct --------------


#### 6ii. arrange --------


##### 6iii. pull --------


## 7. Pivot data ----



## 8. Split and combine cells ----

# unite

# separate






