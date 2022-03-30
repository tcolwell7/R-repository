## functions in Janitor package used day to day

# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

#upload data

trade_data <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()
tariff_data <- read_excel("..\\data\\tariff_data.xlsx")


# 1. clean names ---------------------------
#'
#' clean names makes column names cleaner, more easy to type and easier to write code using. 
#' I use this function in every script I write.
#' Every dataframe I upload into R I follow with clean_names()
#' Through my R experience the number of times I have not used a capital letter
#' or not typed a column correctly within `` 
#' I have lost count. It has been the cause of so many errors, issues and time wasted error checking
#' clean names is the simplest trick I have done to improve code writing and efficiency 
#' It is a shame it took me 4 years before I discovered the function!!!
#' 


#' Examples: load in using various methods with varying column names. compare outputs:
#' 
df <- read.csv("..\\data\\cleanNames.csv")
print(colnames(df))

df <- read_csv("..\\data\\cleanNames.csv")
print(colnames(df))

df <- read.xlsx("..\\data\\cleanNames.xlsx")
print(colnames(df))

df <- read_excel("..\\data\\cleanNames.xlsx")
print(colnames(df))


df2 <- read_excel("..\\data\\cleanNames.xlsx") %>% clean_names()
print(colnames(df2))


## 1.1 make clean names ----------------------

# clean a vector of text:

#' Example - extracting a unique name from a dataframe you want to chnage to lower case and remove spaces:
#' Filter trade data for country name:

string <- tail(trade_data,50) %>% clean_names() %>% distinct(.,country_name) %>% pull()

string2 <- make_clean_names(string)

# clean an array 

array <- trade_data %>% clean_names() %>% distinct(.,country_name) %>% pull()

array2 <- make_clean_names(array)

print(array)
print(array2)

#' I use this alot when working with vectors/array for filtering or creating IF/Else logical statements. +
#' When I create functions for user inputs, this helps nullify the issue of spelling inputs using capital letters 
#' 

# 2. compare dataframe cols ----------------

#' compare_df_cols is a very useful function to QA data before manipulating it further.
#' A common practice is binding dataframes together. I do this all the time. 
#' I have come across the issue where the number of columns or column spellings don't match (and hence don't rbind)
#' This function within the janitor package provides a quick, easy way to write simple QA checks before binding data. 
#' This can be used injunction with error messages to help for error checking and audting code. 
#' For example - if I have written a lengthy and complicated function
#' I can place this compare_df_cols QA check in and if a cusotm error message occurs I immeidately no where to check
#' This trick would have saved me ALOT of time over the years error chekcing my own work!
#'
#' 

## 2.1 examples -----------

df <- trade_data
df2 <- trade_data
df3 <- trade_data

df_qa = compare_df_cols(
  df,
  df2,
  df3,
  return = c("all"),
  bind_method = c("bind_rows", "rbind"),
  strict_description = FALSE
) 


#' output: summary of the column names between each data frame and column types (very useful!)


### example 2: --------


df2 <- df %>%
  mutate(
    across(value_gbp, as.character),
     value_gbp2 = value_gbp
  ) 

#df2 <- df %>%
 # mutate_at(vars(value_gbp), as.character
  #)

df_qa = compare_df_cols(
  df,
  df2,
  return = c("all"),
  bind_method = c("bind_rows", "rbind"),
  strict_description = FALSE
) 


# Now we have dataframes which have different cols/column types we can create an in-built QA check

### 2.2 QA check example -------------

#' create a QA dataframe using compare_df_cols
#' filter for any NA values (if no na vlaues all columns match)
#' secondary comparison of column types
#' output: create warning message or abort action if this check is failed
#' Using inbuilt R functions message/stop/abort can easily create custom error messages
#' 

df_qa = compare_df_cols(
  df,
  df2,
  return = c("all"),
  bind_method = c("bind_rows", "rbind"),
  strict_description = FALSE
) %>%
  filter_at(vars(df:df2), any_vars(is.na(.))) 

# using nrow

# message:
if(nrow(df_qa) > 0){print("Error message")
  }else{print("QA check passged") }

if(nrow(df_qa) > 0){
  message("Error message")
   }else{
     
         print("QA check passged") 
         rbind(df,df2)
   }

# stop:
if(nrow(df_qa) > 0){
   stop("Error message")
}else{
  
  print("QA check passged") 
  rbind(df,df2)
}



# abort:
if(nrow(df_qa) > 0){
  rlang::abort("Error message")
}else{
  
  print("QA check passged") 
  rbind(df,df2)
}


# secondary QA check:

df2$year <- as.numeric(df2$year)

df_qa2 <- compare_df_cols(
  df,
  df2,
  return = c("all"),
  bind_method = c("bind_rows", "rbind"),
  strict_description = FALSE
) %>%
  filter(across(c(df,df2), ~ !is.na(.))) %>% # ignore NA vlaues for this check
  mutate(col_check = df == df2)

colTypeCheck <- df_qa2 %>% filter(col_check == FALSE)


if(nrow(colTypeCheck) > 0){
  abort(c("Col types are different for ",unique(colTypeCheck$column_name)))
} else{
  rbind(df,df2)
}

if(nrow(colTypeCheck) > 0){
  stop(c("Col types are different for ", unique(colTypeCheck$column_name)))
} else{
  rbind(df,df2)
}

# abort is preferbale whne outputting specific fields from dataframes in error message

#### 2.3 alternative ----------------------------------------------

#' janitor has an in built function to achieve
#' a check of column names opposed to creating a df:
#' 
compare_df_cols_same(df,df2)

#' while very useful and beneficial - 
#' if you want to know which columns are different 
#' and to create a unique error message a more custom function is required

#### 2.3 automated QA check --------------------------------------





# 3. Get duplicates ----------------------------------------------

#' get_dupes - identify duplicates within dataframe
#' 

df <- head(tariff_data,200) %>% 
  select(
    starts_with("commodity"),
    contains("mfn"),
    num_range(
      "preferential_applied_duty_rate_",
      2021:2024),
    contains("hs")
  )

# get duplicate tariff rates:
df2 <- df %>% get_dupes(c(mfn_applied_duty_rate))

df <- rbind(head(trade_data,100),tail(trade_data,100))

# get duplicate commodity code rows:

df2 <- df %>% get_dupes(commodity_code)

# 4. remove empty --------------------------

# remove empty rows or columns from dataframe:
# practical exmaple of how I used this function within a larger piece of automation:

# The staging dataframe has multiple countries data combined. Not all countries have data in each column.
# To clean this simply in an automated fashion I used remove_empty within a cusotm function

df <- read_excel("..\\data\\staging_data.xlsx") %>% clean_names() %>% select(-staging_2021)

for(c in unique(df$country)){
   
  print(c)
  # filter data for country
  df2 <- df %>% filter(country == c)

  # remove columns with no data in:

  df2 <- 
    df2 %>%
    remove_empty(
      .,
      which = "cols",
      quiet = FALSE # message which columns have been removed. (To remove use TRUE)
    )
  
}

# 5. adorn functions -------------------------------------

## 5.1 adorn total ---------------------------------------

#' adorn totals is a quick, simple and easy way to add totals to a dataframe.
#' For creating summary data tables used in reports to to present
#' This is a great function to have. 
#' 

df <- head(trade_data,10) %>%
  select(-suppression_notes) %>%
  mutate(
     value_gbp2 = value_gbp / 100,
     value_gbp3 = value_gbp / 1000
     )

df2 <- df %>% adorn_totals()

# create summary column adding up each row for a new column:
df2 <- df %>% adorn_totals(where = "col")

# for both summary row and column:
df2 <- df %>% adorn_totals(where = c("col","row"))

# Example - summaries trade data to yearly values in wide format: then summarise totals.

df <- trade_data %>%
  group_by(country_name) %>%
  summarise(
    value19 = sum(value_gbp[year == "2019"]),
    value20 = sum(value_gbp[year == "2020"])
    ) %>%
  adorn_totals(where = c("col","row"), name = "Total Imports")

          

### 5. adorn_pct_formatting -----------------

# format percentage from decemial:
# using function you can name, specific col to use using the "..." argument

df <- trade_data %>%
  group_by(country_name) %>%
  summarise(
    value19 = sum(value_gbp[year == "2019"]),
    value20 = sum(value_gbp[year == "2020"])
  ) %>%
  mutate(pc = value19/value20) %>%
  adorn_pct_formatting(
    .,
    digits = 1,
    rounding = "half up",
    affix_sign = TRUE,
    pc
    )

# alternatively you can use adorn_pct_formatting(,,,pc)

# adorn_rounding
# round numerical values

df <- head(trade_data,10) %>%
  select(-suppression_notes) %>%
  mutate(
    value_gbp2 = value_gbp / 100,
    value_gbp3 = value_gbp / 1000
  ) 

df2 <- df %>% adorn_rounding()

