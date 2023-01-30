# 04 - stringr - package to help aid in string manipulations and transformations. Very helpful package. 

# set up ----------------------------------------------------------------------------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)

options(scipen=999)

# set working directory
path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

#upload data

uk_trqs <- read_csv("..\\data\\uk_trqs.csv") %>% clean_names()
tariff_data <- read_excel("..\\data\\tariff_data.xlsx")
trade_data <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()



# 1. string extraction ------------------------------------------
## str_sub --------------------------------------------------

#' 
#' Extract string from field/column:
#' I use this function all the time. 
#' It's base R equivalent was one of the first functions I learnt within R 
#' I use this stringr equivalent all the time in my day to day work
#' I first learnt this while trying to achieve a "LEFT" function in excel
#' a very useful function to have!
#' 


# trim column length i.e. reduce the string of commodity codes:

df <- trade_data %>% 
  mutate(
    hs2 = str_sub(commodity_code,1,2), # extract 2 characters from string starting from the first string position
    hs4 = str_sub(commodity_code,1,4), # extract 4 chracters ..
    hs6 = str_sub(commodity_code,1,6)  # extract 6 chracters and so on...
  )


#' same can be applied to create a "MID" or "RIGHT" function
#' chnaging the start character
#' 

df <- 
  df %>% 
  mutate(
    right = str_sub(commodity_code,7,8), # start at the 7th character string and extract the 8th (2 characters)
    mid = str_sub(commodity_code,4,6) # start at the 4th character string and extract up to the 6th (3 characterS)
  )


#' you can substitue vectors within the str_sub function
#' to automate the actions depending on the string length

df <- select(tariff_data,1:4)

# extract the last two digits of a string
# the commodity code column has varying lenght strings

df2 <- df %>% 
  mutate(
    start_string = x8_digit_or_10_digit -1, # if we wanted the last 3 characters we would chnage to this 2, 4 characters chnage to 3 etc. 
    new_string = str_sub(commodity_code,start_string,x8_digit_or_10_digit)
  )

#' In excel I have used the combination of the length of string
#' with the right function frequently
#' Thsi can easily be replicated in R:
#' 
#

### str_length -------------------------------------------------

#' use str_len to find the length of any chracter string within a df

#( ignoring the fact we already have the string length in the df ..)
df2 <-
  df %>% 
  mutate(
    string_length = str_length(commodity_code),
    start_string = x8_digit_or_10_digit -1,
    new_string = 
      str_sub(
        commodity_code,
        start_string,
        string_length
      )
  )
  



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

# 2. string search and extract -----------------------------------------------------
## str_count (& grepl) -------------

#' count the instances of a pattern within a string.
#' For example - count the number of zeros within a character string:
#' Example 2 - search string for the word "horses" within commodity code description

df2 <- df %>% mutate(string_count = str_count(commodity_code,"0"))

df2 <- df %>% mutate(string_count = str_count(commodity_code_description,"horses"))
# alternatively "grepl" can be used to return boolean output (TRUE/FALSE)
df2 <- df %>% mutate(string_search = grepl("horses",commodity_code_description))

# str_count can be useful to identified duplicate strings, wordings and in text searches. 


### str_extract ----------------------------------

# extract the word "bovine" from the commodity code description columns
df2 <- df %>% 
  mutate(
    str_extract1 = str_extract(commodity_code_description,"bovine"),
    str_extract2 = str_extract(commodity_code_description,"eggs"),
    str_extract3 = str_extract(commodity_code,"0")
  )

#' notice with string extract 3 - there are multiple instances of "0"
#' all occurances of a string cna be extracted with str_extract_all:


df2 <- df2 %>% mutate(str_extract4 = str_extract_all(commodity_code, "0"))



# 3 string removal -------------------------------------------------

#' There are many instacnes of when strings need to be trimmed, edited or parts removed. 
#' string provides a very easy to use funciton to achieve this:
#' 

## str_remove ----------------------------------------

#' the UK TRQ data set is an unclean web extracted spreadsheet which is an SQL output. 
#' This data needs to be cleaned before it can be be more usable. 
#' eg. there are square brackets within multiple fields. Names are within quotas etc. 
#' This is a perfect data set to apply the removal functions for.
#' 

df <- uk_trqs %>% 
  mutate(
    geographical_areas = str_remove(geographical_areas,"\\["), # (regex to required to use \\)
    geographical_areas = str_remove(geographical_areas,"\\]")
  )

# remove ''

df2 <- df %>% mutate(geographical_areas = str_remove(geographical_areas,"\\'"))

#' *NOTICE* that only one ' was revmoed. 
#' when there are repeated occurances of a string
#' You can remove all of them using str_remove_all

df2 <- df %>% mutate(geographical_areas = str_remove_all(geographical_areas,"\\'"))

# remove all '' from neccessary columns:

df3 <- df2 %>% 
  mutate(
    headings = str_remove_all(headings, "\\'"),
    commodities = str_remove_all(commodities, "\\'")
  )

# replicating the same process across multiple columns can be simplifed
# by using tidyverse functions such as mutate_at / mutate_if

stringRemove <- function(.x)(str_remove_all(.x,"\\'"))

df3 <- df2 %>% 
  mutate_if(is.character, .funs = stringRemove)

# apply same process to remove brackets:

stringRemove2 <- function(.x)(str_remove_all(.x,"\\[|\\]")) # regex character removal

df3 <- df2 %>% 
  mutate_if(is.character, .funs = stringRemove2)

# alternatively:

df3 <- df2 %>%
  mutate(
    across(
      everything(), ~ str_remove_all(.x,"\\[|\\]")
      )
    )

# remove all strings at once:

df3 <-
  df %>%
  mutate(
    across(
      everything(), ~ str_remove_all(.x, "\\[|\\]|\\'")
    )
  ) %>%
  mutate(validity = str_remove(validity,"\\)")) # remove outliter ) bracket
                

    
### digits ------------

# you can use remove_all or replace_all to acheive same thing
df <- 
  uk_trqs %>% 
  mutate(headings = str_remove_all(headings, "[:digit:]"))

### non-digits --------


df <- 
  uk_trqs %>% 
  mutate(headings = str_replace_all(headings, "[a-zA-Z]", ""))


### special characters

df <- 
  uk_trqs %>% 
  mutate(headings = str_replace_all(headings, "[[:punct:]]", ""))

### alphanumerical characters -------

df <- 
  uk_trqs %>% 
  mutate(headings = str_replace_all(headings,"[^[:alnum:]]", ""))


# 4. remove string whitespace ----------------------------------------

#' 
#' when I have worked with strings and pattern recognition 
#' I have often come across issues of the pattern functions not work
#' They wouldn't work which didnt make sense
#' but these issues stemmed from hidden "whitespace" within the strings
#' and hence the pattern recogition not working. 
#' The following functions I use to trim/remove whitespace. 
#' 
#' when working with SQL database outputs
#' I often revert to removing whitespace
#' using these functions
#' 

## str_trim ---------------------------------------------------------

#' str_trim you can select whice side of the stirng you want to remove space from:
#' 


df <- uk_trqs %>%
  mutate(
    commodities = str_trim(commodities, side = "left"), # remove whitespace from left handside of string
    commodiies2 = str_trim(commodities, side = "right"), # remove whitespace from right handside of string
    commodiies3 = str_trim(commodities, side = "both") # remove whitespace from left and right handside of string
  )


### str_squish -------------------------------------------------------

#' removes white space form withinside a sttring


df <- uk_trqs %>%
  mutate(
    commodities = str_squish(commodities),
    validity = str_squish(validity)
  )

# 5. string detection -----------------------------------------------

#' There are functions which are helpful
#' to detect patterns or strings within columns
#' This is combination can be very helpful
#' To automate string pattenr recongniation
#' and creation of new / updated fields
#' 

## str_detect -------------------------------------------------------

#' function to return TRUE/FALSE if pattern is detected within a string.
#' Example: detect "ERGA" within quota origin within uk trq data and various other flags:

df <-
  uk_trqs %>%
  mutate(
    erga_flag = str_detect(geographical_areas,"ERGA"),
    kg_flag = str_detect(quota_unit,"kg"),
    hl_flag = str_detect(quota_unit,"hl")
  ) %>%
  mutate( # convert to tonnes: (1000 kg = 1tn)
    volume =
      ifelse(
        kg_flag == TRUE,
        initial_volume / 1000,
        initial_volume
      )
    )


### str_count -------------------------------------------------------

#' count the number of matches of a pattenr within a string:
#' 

#' count the number of "0" within commodity code string:

df <- trade_data %>% mutate(string_count = str_count(commodity_code,"0"))

#### str_locate -----------------------------------------------------

#' locate the position of patterns within a string.
#' *Example* this can be used to remove non numeric character strings for example:
#'           may look to extract the first part of a combined tariff
#'           i.e. 0.00% + £10 / 1000 tonne. - want to extract the 0.00% -
#'           but in an automated fasion the length of the first string varies
#'           i.e. 15.25% , 1%, 0.1% etc. 
#'           using str_locate the position of the start of the second string can be located
#'           and then isolated and converted to a numerical value

df <- 
  tariff_data %>%
  select(1:6) %>%
  mutate(string_locate = str_locate(mfn_applied_duty_rate,"\\+")[,1]) %>% # include indexing to return single return, rather than multiple dimensions
  mutate(string_locate2 = str_locate(mfn_applied_duty_rate,"0")) %>%
  mutate(string_locate3 = str_locate(preferential_applied_duty_rate_2021,"0"))

## compare environment with the created fileds to check the aatributes created when using str_locate:


# combine string location with str_sub:

df <- df %>% 
   mutate(
     ave_tariff = 
       ifelse(
         is.na(string_locate), # string_locate has nas. Add logic to equation and stR_sub works
         mfn_applied_duty_rate,
         str_sub(mfn_applied_duty_rate,1,string_locate)
       )
   )
        
# function doesn't work perfect as it includes the "+" - 
# a formula cna be added to reduce the string length

# if the str_locate has used an index ([,1]) - there is no need for the ifelse statement
# as follows:

df <- df %>% mutate(ave_tariff =  str_sub(mfn_applied_duty_rate,1,string_locate))

#() chnage last string to extract from:)
df <- df %>% mutate(ave_tariff =  str_sub(mfn_applied_duty_rate,1,(string_locate-1)))



# can now convert to a numerical for example:

df2 <- df %>% mutate(ave_tariff = as.numeric(ave_tariff))

# NAs returned as non-numeric character in string. 
# Need to remove the "%". 

df2 <- df %>%
  mutate(ave_tariff = str_remove(ave_tariff, "%"),
         ave_tariff = as.numeric(ave_tariff))






# 6. string formatting -------------------------------------------------------------

#' string case formatting

df <- trade_data %>%
  mutate(
    country_name_lower = str_to_lower(country_name), # lower case formatting
    country_name_upper = str_to_upper(country_name) # upper case formatting
  ) %>%
  mutate(
    country_name_title = str_to_title(country_name) # format as title
  )


## str_glue --------

#' These functions are wrappers around glue::glue() 
#' and glue::glue_data(), 
#' which provide a powerful and elegant 
#' syntax for interpolating strings. 
#' These wrappers provide a small set of the full options. 
#' Use the functions directly from glue for more control.


# create column using glu_str - essentially pasting together columns within table
# more effective than using paste0

df <- tariff_data %>%
  mutate(
    paste_col = 
      str_glue("Pasting together commodity code heading '{commodity_heading}'",
               "And the commodity code '{commodity_code}'")
  )


## str_glue_data --------

# outside of mutate it creates a text object
df <- tariff_data %>%
  str_glue_data(
    "MFN duty rate {mfn_applied_duty_rate}",
    " and Pref rate {preferential_applied_duty_rate_2021}"
    )



## str_flatten ------

#' str_flatten() reduces a character vector to a single string. 
#' This is a summary function because regardless of the length of the input x, 
#' it always returns a single string.

#str_flatten_comma() is a variation designed specifically for flattening with commas. It automatically recognises if last uses the Oxford comma and handles the special case of 2 elements.


str_flatten(
  string = letters[1:10], 
  collapse = ", ",
  last = " and ",
  na.rm = FALSE
)

# useful if you have an array and you cant to combine into a single string 



#

