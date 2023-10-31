## 01 Tidyverse function examples. 

## /// Day to day functions I use using tidyverse for data wrangling, transformaitons, cleaning and aggregaiton to produce useable outputs. 

## function areas to cover: Selecting columns, creating columns, filtering, aggregations, duplicates, seperating delimiters. 

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


df <- tariff_data %>% 
  select(
    starts_with("commodity_code"),
    starts_with("preferential"), # This captures an unwanted column "preferential_applied_duty_rate_excluded"
    #starts_with(c("commodity_code","preferential")), 
     contains("mfn")
    )


df <- tariff_data %>% 
  select(
    starts_with("commodity_code"),
    contains("mfn"),
    num_range(
      "preferential_applied_duty_rate_",
      2021:2024
      )
    )

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


### 1i. select_if ------------------

# select df columns based on input function or condition, i.e. contian no NAs.

print(print(colnames(tariff_data)))

#' supression_notes has NAs. 
#' want a dynamic way to remove columns with NAs
#' if for example there were numerous columns
#' so not having to type each individually out
#' 

#' combine ! > any > function (is.na()) 

df <-
  trade_data %>%
  select_if(~ !any(is.na(.)))


#### 1ii. Relocate ------

# relocate select column positioning based on other column location. For example you want to relocate a column before or after another. 
print(colnames(tariff_data))


# You can relocate columns manually using select: i.e. select(1,2,4,3,5,6... ) etc. 
# This can be laborious and inefficient (typing each column out) 
# and prone to error if the df order changes if using column indexing. 


df <- tariff_data %>% 
  select(
    starts_with("commodity_code"),
    contains("mfn"),
    num_range(
      "preferential_applied_duty_rate_",
      2021:2024
      ),
  where(where(is.numeric))
  )
  
  
df2 <- df  %>% relocate(x8_digit_or_10_digit, .after = "commodity_code")
df2 <- df2 %>% relocate(cn8_count, .after = "x8_digit_or_10_digit")
df2 <- df2 %>% relocate(value_usd, .before = "commodity_code_description")
  
df2 <- 
  df %>%
  relocate(
    .,
    c(
      "x8_digit_or_10_digit",
      "cn8_count"
      ), 
    .after = "commodity_code"
    )

# relocate numerical only columns:

df2 <-
  df %>%
  relocate(where(is.numeric), .before = "commodity_code_description")


## 2. Mutate ------------------------------------------------------------------

# create and update columns using mutate

df2 <- df  %>% mutate(value_gbp = value_gbp / 1000000)
df2 <- df  %>% mutate(value_gbp2 = value_gbp / 1000)
df2 <- df2 %>% mutate(newCol = paste0(commodity_code,"_",year))


# select specific column and drop non-numeric characters 
df3 <- df2 %>% mutate(commodity_code = parse_number(commodity_code))

df3 <- df2 %>% 
  mutate_at(
    c("commodity_code", "newCol"), 
    .funs = parse_number
    ) 

# INSTEAD OF:

df3 <- df2 %>% 
  mutate(
    commodity_code = parse_number(commodity_code),
    newCol = parse_number(newCol)
    )

# convert numeric columns to character
df3 <- df2 %>% mutate_if(is.numeric, .funs = as.character)
# OR
df3 <- df2 %>% mutate(across(where(is.numeric), ~ as.character(.x)))

# multiply all numerical values by 1000             
df3 <- df2 %>% mutate(across(where(is.numeric), ~ .x * 1000))


### 2i. Mutate across variables -----
## combining mutate > across > where is very useful and efficient when applying functions to multiple columns.  
 

df <- 
  tariff_data %>% 
  select(
    starts_with("commodity_code"),
    mfn_applied_duty_rate,
    num_range(
      "preferential_applied_duty_rate_",
      2021:2024
      )
    )

# want to convert tariffs to numeric. Need to remove '%'
# This can be done using str_remove in the stringr package. 

df2 <-
  df %>%
  mutate(
    preferential_applied_duty_rate_2021 = str_remove(preferential_applied_duty_rate_2021, "%"),
    preferential_applied_duty_rate_2022 = str_remove(preferential_applied_duty_rate_2022, "%"),
    preferential_applied_duty_rate_2023 = str_remove(preferential_applied_duty_rate_2023, "%"),
    preferential_applied_duty_rate_2024 = str_remove(preferential_applied_duty_rate_2024, "%")
    )

# single string function to remove "%"
stringRemove <- function(.x)(str_remove_all(.x,"%"))

df2 <- df %>% 
  mutate_at(
    c("preferential_applied_duty_rate_2021",
     "preferential_applied_duty_rate_2022"
     ), 
    .funs = stringRemove
    )

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
    across(c(4:7), ~ str_remove_all(.x, "%"))
  )

#' remove % from all tariff columns and covert to numeric and calculate average tariff. 
#' -c(1:2) all columns except the first two
#' 

df2 <-
  df %>%
  mutate(
    across(
        -c(1:2), ~ str_remove_all(.x, "%")
      )
  ) %>%
  mutate(
    across(
      -c(1:2), ~ as.numeric(.x)
      )
  ) %>%
  mutate(
    average_tariff = rowMeans(across(-c(1:2)), na.rm = T)
    )
                      
#### 2ii. Mutate across new names ---------

#' you can instead of updating the column names
#' create new columns using across /names function!
#' Nice and simple

df2 <- 
  df %>% 
  mutate(
    across(
      .cols = c(preferential_applied_duty_rate_2021:preferential_applied_duty_rate_2024), 
      .fns =  str_remove_all(.x, "%"),
      .names = "{.col}_new"
    )
  )



## 3. Filter -----

# Filter data sets referring to pre-calculated values, arrays and text. 

filterValue = 1000000

df <- trade_data %>% filter(value_gbp >= 200000)
df <- trade_data %>% filter(value_gbp >= mean(value_gbp, na.rm = T))
df <- trade_data %>% filter(value_gbp < filterValue)


# filter using AND | OR opoerators. 

# & for and | for or
# filter for Taiwan or Thailand
df <- trade_data %>% filter(country_name == "Taiwan" | country_name == "Thailand")

# filer for exports and Taiwan

df <- trade_data %>% filter(country_name == "Taiwan" & flow == "Exports")

# + filter if value is greater than 10,000,000

df <- trade_data %>% 
  filter(
    country_name == "Taiwan" & 
    flow == "Exports" & 
    value_gbp >= 10000000
  )

# combine multiple filtering conditions using and / or. 


df <- trade_data %>% 
  filter(
    (country_name == "Taiwan" & 
      flow == "Exports" & 
      value_gbp >= 10000000)
     |
     (country_name == "Thailand" & 
        flow == "Imports" & 
        value_gbp >= 10000000)
  )


### 3i. Filter by array -----------------------------
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


#### 3ii. Conditional filtering --------

# conditional formatting:
# filter data based on select input:
# For example, if a user whats to filter based on hs2, hs4 etc. 

# create hs columns:
df <- trade_data %>% 
  mutate(
    hs2 = str_sub(commodity_code,1,2),
    hs4 = str_sub(commodity_code, 1, 4), 
    hs6 = str_sub(commodity_code, 1, 6)
  )

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


##### 3iii. Filter at --------------------------------------------

# filter any trade value is less than 100
df <- trade_data %>%
  mutate(value_gbp2 = value_gbp / 10) %>%
  filter_at(
    vars(value_gbp,value_gbp2), 
    any_vars(. < 10)
    ) 

# filter for NAs
df <- trade_data %>%
  mutate(value_gbp2 = value_gbp / 10) %>%
  filter_at(
    vars(value_gbp,value_gbp2), 
    any_vars(is.na(.))
  ) 


# filter for MFN rate not equal to 0
df <-  tariff_data %>% 
  select(
    starts_with("commodity_code"),
    mfn_applied_duty_rate,
    num_range(
      "preferential_applied_duty_rate_",
      2021:2024
    )
  ) %>%
  filter_at(
    vars(contains("mfn")),
    any_vars(. != "0%")
  )
  
# filter df using tidy select of colums using if any and starts with
df <- tariff_data %>% 
  filter(
    if_any(
      starts_with("pref"), ~ (.x == "0%")
      )
    )


#### 3iv. filter across ---------------------------------

#' filter across all columns, 
#' filter across df to find nas or remove nas
#' 



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
  summarise(
    total_value = sum(value_gbp), # sum total column
    mean_value  = mean(value_gbp),
    count_code  = length(commodity_code),
    max_value   = max(value_gbp),
    min_value   = min(value_gbp)
    )

# You can easily concatenate multiple rows into one cell using group_by and summarise:

# alternatively if you want the whole dataframe values you can use summarise to calculate this:

df <- trade_data %>% 
  summarise(
    total_value = sum(value_gbp), 
    mean_value = mean(value_gbp)
    )


### 4i. "Sumifs" method ----

# Calculate year values for "Taiwan" using base R filtering. 

df <- 
  trade_data %>% 
  group_by(year) %>%
  summarise(
    total_value = sum(value_gbp[country_name == "Taiwan"]), # sum total column for Taiwan. 
    mean_value  = mean(value_gbp[country_name == "Taiwan"])
    )
           
### 4i. tally ------------

#' tally() allows a quick and easy way to count the number of occurances per grouping
#' 

df <- trade_data %>%
  group_by(country_name, country_code, flow, year) %>%
  tally()

df <- trade_data %>%
  group_by(year) %>%
  tally()


#### 4ii. Concatenate cells ------

# example data:

df <- tariff_data %>% mutate(hs4 = str_sub(commodity_code, 1,4)) %>% head(30)

df2 <- df %>% 
  group_by(hs4) %>%
  summarise(combined_mfn_tariff = paste0(mfn_applied_duty_rate, collapse = " ; "))
  

##### 4iii. Unnest -----------
## ungroup a concatonated cell: -


df3 <- df2 %>% 
  mutate(
    combined_mfn_tariff = 
      strsplit(
        as.character(combined_mfn_tariff), ";")
    ) %>% 
  unnest(combined_mfn_tariff)

# separate_rows:

df3 <- df2 %>% separate_rows(., combined_mfn_tariff, sep = ";")

###### 4.iv Ungroup ----

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
  summarise(
    total_value = sum(value_gbp)
    ) %>%
  mutate(rank_country = rank(total_value))

##  using upgroup:

df <- trade_data %>%
  group_by(country_name, country_code) %>%
  summarise(
    total_value = sum(value_gbp)
    ) %>%
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


df3 <- df %>% 
  left_join(df2, 
            by = 
              c("commodity_code" = "commodity_code",
                "year" = "year",
                "country_name" = "country_name"
                )
            )


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

## bind_cols
df3 <- bind_cols(df,df2)

#' notice the column names are duplicated
#' and are renamed if you bind together the dfs. 
#' 

df2 <- tail(trade_data,25)

#' number of rows must be the same in bind_cols. 
df3 <- bind_cols(df,df2)



# dynamically bind dfs together. 
# you can create columns/filter data and bind them together
# without having to create new dfs or objects. 


df_bind <- bind_rows({trade_data %>% filter(year == "2020") %>% filter(country_code == "US")},
                     {trade_data %>% mutate(value2 = value_gbp*2)},
                     {trade_data %>% filter(country_code == "US")}
)


## 6. Other operators -----------------------
#' selection of other tidyverse operators I continually use:
### 6i. distinct ----------------------------
#'
#' Remove duplicate rows in a dataframe
#' Using specific fields to determine unique rows in the df
#'
#'

# data frame with repeated rows. 
df <- rbind(trade_data,trade_data)

# using distinct() where all rows are duplicated they will be deleted. 

df2 <- df %>% distinct()

#' remove duplicate rows using the country name as unique identifier:
#' This results in a single column dataframe with only single entires of the country name
#' This can be quite useful
#' but the function needs adding to depending on what the
#' desired outcome is
df2 <- df %>% distinct(., country_name)

#' .keep_names
#' input to keep the df columns once duplicate rows are removed
df2 <- df %>% distinct(., country_name, .keep_all = TRUE)

#' issue: keeping columns with this distinct dataframe using one field returns the first ordered row. 
#' 

#' Output: unique df for acountries and specific commodity codes they trade
#'         unique df for countries, specific commodity codes and years they trade
#'         unique df for countries and years they trade

df3 <- df %>% distinct(., country_name, commodity_code, .keep_all = TRUE)


df3 <- df %>% distinct(., country_name, commodity_code, year, .keep_all = TRUE)


df3 <- df %>% distinct(., country_name, year)

#### 6ii. arrange --------
#'
#' Re-order / sort dataframe based on specific group/variables.
#'
#'

df <- head(trade_data,100)

# re-roder based on smallest value
df2 <- df %>% arrange(value_gbp)

# largest:
df2 <- df %>% arrange(-value_gbp)
# or
df2 <- df %>% arrange(desc(value_gbp))

# arrnage df for largest values - but specfiic to each year

df2 <- df %>% arrange(desc(value_gbp), year)

#' Common output:
#' I freqeuntly aggregate data based on simple groupings, i.e country and year
#' I then want to reorder the df by highest trading country by value
#' and create a ranking i.e top trade = rank 1, 2 etc. 
#' I use arrnage to do this t re-order my df


df2 <- trade_data %>%
  group_by(year, country_name) %>%
  summarise(
    total_value = sum(value_gbp)
  )  %>% # df now in group by order i.e. year then country name in ascending order. 
  arrange(
    desc(total_value)
    ) # the group by order is removed. 

# you can re-spell out the grouping you desire for the outcome:
df2 <- trade_data %>%
  group_by(year, country_name) %>%
  summarise(
    total_value = sum(value_gbp)
  )  %>% # df now in group by order i.e. year then country name in ascending order. 
  arrange(
    year,
    desc(total_value),
    country_name
  ) 


  
# using arrnage .by_group input this keep the grouping intact. 
df2 <- trade_data %>%
  group_by(year, country_name) %>%
  summarise(
    total_value = sum(value_gbp)
  )  %>% # df now in group by order i.e. year then country name in ascending order. 
  arrange(
    desc(total_value),
    .by_group = TRUE
  ) %>%  # add rnaking
  mutate(
    rank = rank(desc(total_value))
  )



##### 6iii. pull --------
#'
#' I use pull to convert columns from dfs into a character vector for further use
#' I commonly use the character vector for an array to filter against
#' or loop through
#' or act as an array for a QA check:
#' 
#' Output: using distinct list of country names - pull into an array:
#' 

arr <- trade_data %>% pull(., country_name)
arr <- tail(trade_data,1000) %>% distinct(., country_name) %>% pull()

# filter data based on array:
df <- trade_data %>% filter(country_name %in% arr) 
df <- trade_data %>% filter(country_name %notin% arr) 

## 7. Pivot data ----

#' Function to turn long data forma tinto wide and visa versa.  
#' I use this function alot fo general data wrangling / transformations. 
#'

### 7i. pivot longer -------------------------------------------

# create a df multiple value columns:

df <- 
  head(trade_data,100) %>% 
  select(
    country_name, 
    commodity_code, 
    value_gbp
    ) %>%
  mutate(
    value_gbp2 = value_gbp*2,
    value_gbp3 = value_gbp*3
    )

# convert wide format into long format: i.e. one df row per value per grouping. 

df_long <- df %>%
  pivot_longer(
    ., 
    cols = value_gbp:value_gbp3, # columns to pivot to long format
    names_to = "value_category" # column name for new column
  )

#### 7ii. pivot wider -----------------------------------------

# trade data is in long format (i.e year column)
# pivot into wide format - i.e. one column per value per year


df_wide <- 
  trade_data %>%
  group_by(country_name,year) %>%
  summarise(
    value_gbp = sum(value_gbp)) %>% # aggregate to simple df - one observation per country per year
  pivot_wider(
    .,
    names_from = year,
    values_from = value_gbp
  ) # widens data using year columns



#' When there are duplicate rows based on the data grouping
#'  (i.e. commodity code - multiple instances across different countries)
#'  The function doesn't work
#'  Create unique grouping and row identifier 
#'  And add to col_id
#'  Pivot wider then will make data wider based on the id (year)

df_wide2 <- 
  trade_data %>%
  group_by(year, commodity_code, country_name) %>%
  select(
    commodity_code, 
    country_name, 
    year, 
    value_gbp
    ) %>%
  mutate(row = row_number()) %>% # unique row number
  pivot_wider(
    .,
    id_cols = c(row, year, commodity_code, country_name),
    names_from = year,
    values_from = value_gbp
  ) %>%
  select(-row)



## 8. Split and combine cells ----

### 8i. unite --------------------

#' function to combine multiple columns together

df <- head(tariff_data,50) %>% select(1:10)

# combine all preferential duty columns into one:

df2 <- df %>% 
  unite(
    .,
    "pref_combined", 
    preferential_applied_duty_rate_2021:preferential_applied_duty_rate_2024, # column index can be used instead of col names
    sep = " ; ",
    remove = FALSE ## change to TRUE to remove input columns
  )


##### 8ii. separate -------------------
#'
#' separate 
#'

## separate combined column created in section 8i using unite:

df3 <- df2 %>%
  separate(
    .,
    pref_combined, # column to separate
    into = paste0("col",1:4), # new column names to split column into
    sep = " ; ",
    remove = TRUE # default is TRUE - remove column being seperated 
  )


# 0. as.symbol -----------------------


#' I utilise input text to create and manipulate data frequently. 
#' I use loop or function inputs to filter data or create column names
#' This is a very useful thing to know and use. 
#' 

# mutate


# filter



# group_by & summarise


# using a function


