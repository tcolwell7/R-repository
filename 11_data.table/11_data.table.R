# 11 data.table

#' the data.table package is a powerful alternative to base R (more concise code) and 
#' tidyverse (much quicker) and enables users to handle data on much larger scales. 
#' the purpose of this script is to highlight the processing power and 
#' difference between methods (base, data.table, tidyverse) for computing and wrangling data. 
#' 
#' This script also serves as a repository of basic data.table functions
#' and uses to wrangle and transform data for basic data analysis and exploration. 

#' helpful resource: <https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/>
#' further inspiration for exploring data.table: https://eliocamp.github.io/codigo-r/en/2019/07/why-i-love-data-table/
#' 

# Set up -------------------------

rm(list=ls()) # remove everything form global environment.

library(tidyverse)
library(openxlsx) 
library(readxl)
library(janitor)
library(stringr)
library(tictoc) # simple function to monitor code chunk run time
library(data.table) # data.table pakcage for high-speed data processing. 

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]"))# set working directory to where your file is saved. 

`%notin%` <- Negate(`%in%`)


# data.table basic functions ----------------------------------

# simple examples using UK TRQ data. 

## 1. upload data ------------------

#' *fread()* uploads csv files but not xlsx. 

uk_trqs <- fread("data/uk_trqs.csv") # uploads as data.table format. 

## 2. select columns -------------------------------------------

# ensure columns are within brackets combined with '.'

df <- uk_trqs[, .(quota_number,quota_origin,year,quota_fill_rate)]

#' multiple columns can be selected using a character array

cols <-c("quota_number","quota_origin","quota_year","fill_rate")
df <- uk_trqs[, cols, with = FALSE]

# drop columns
# define dropped columns within an array and denoted using ! to drop the columns. 
drop_cols <-c("year","fill_rate")
df2 <- df[, !(drop_cols), with = FALSE]

#rename columns you can use base are setnames etc. I will compare this for speed vs rename in dplyr later. 


## 3. filter data -------------------------------------------------------------


#' you can simply filter data by naming the specific columns and conditions

df <- uk_trqs[quota_origin == "Turkey"]
df <- uk_trqs[quota_origin == "Turkey" & quota_year == 2021]
df <- uk_trqs[(quota_origin == "Turkey" | quota_origin == "Iceland") & quota_year == 2021]
df <- uk_trqs[fill_rate >= 0.99]
# etc.

# filter using array

filt = c("Iceland","Turkey","Norway","Vietnam")
df <- uk_trqs[quota_origin %in% filt]
df <- uk_trqs[quota_origin %notin% filt]


## 4. create columns -------------------------------------

# the syntax to create columns is simple, ensure ',' is defined to begin with
# and the equals sign is ' := '.

# select columns for smaller df:

df <- uk_trqs[, .(quota_origin,trq_dsc,volume,quota_usage,fill_rate)]

df2 <- df[, fill_rate2 := quota_usage / volume]

# multiple columns:

df3 <- df2[, `:=`(fill_rate3 = fill_rate / 10,
                  fill_rate4 = fill_rate / 100)
           ]
           
# select only created columns:

df3 <- df2[, .(fill_rate3 = fill_rate / 10,
               fill_rate4 = fill_rate / 100)
]

# convert to character
df3 <- df2[, `:=`(fill_rate2 = as.character(fill_rate2),
                  fill_rate3 = as.character(fill_rate3))
                  ]

# further way to update/create columns. 
df4 <- df3[, c("fill_rate5", "fill_rate6") := list(as.numeric(fill_rate2), as.numeric(fill_rate3))]
df5 <- df4[, c("fill_rate5", "fill_rate6") := list(fill_rate5*10, fill_rate6*100)]
df6 <- df5[, c("fill_rate5", "fill_rate6") := list(0)]

# apply function across multiple columns:

rm(df2,df3,df4,df5,df6) # remove example dfs. 

cols = c("volume","quota_usage","fill_rate")
df2 <- df[, (cols) := lapply(.SD, as.character), .SDcols = (cols)]
df3 <- df2[,(4:5) := lapply(.SD, as.numeric), .SDcols = (4:5)]

# apply function

df3 <- df2[, fill_rate := stringr::str_remove(fill_rate,"[.]")] # works fine. 

# apply function across multiple columns:
func = function(.x){str_remove(.x, "S")}
df4 <- df3[, (1:2) := lapply(.SD, FUN=func), .SDcols = (1:2)]


## 5. aggregations -------------------------------------------

# load trade data for aggregate calculations. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# simple on col group calculation. # total imports by reporting country
tic()
df_agg <- df[, .(total_imports = sum(value_in_euro)), by = declarant_iso]
toc()
# multi-group, total imports by reporting country broken down by partner country
tic()
df_agg <- df[, 
             .(total_imports = sum(value_in_euro)), 
             by = .(declarant_iso, partner_iso)]
toc()

# multiple agg, columns:
tic()
df_agg <- df[, .(total_imports = sum(value_in_euro),
                 mean_imports = mean(value_in_euro),
                 total_imports_weight = sum(quantity_kg),
                 count = length(declarant_iso),
                 stndv = sd(value_in_euro),
                 max_val = max(value_in_euro)),
             by = declarant_iso]
toc()

## 6. keys -------------------------------------------------

#' "Setting one or more keys on a data.table 
#' enables it to perform binary search, 
#' which is many order of magnitudes faster than linear search, 
#' especially for large data.
#' As a result, the filtering operations are super fast
#' after setting the keys. 
#' There is a side effect though.
#' By setting a key, the `data.table` gets sorted by that key."
#' 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

#' filter example. Filter the data based on selection of countries
#' compare when setkey is applied. 

tic()
df_filt <- df[declarant_iso %in% c("FR","DE","GB")]
toc()

# set key to declarant country (i,e. reporting country). 


tic()
setkey(df,declarant_iso)
df_filt <- df[declarant_iso %in% c("FR","DE","GB")]
toc()

#' *by setting the key, the speed to filter is 60+% quicker*


# setting multiple keys. 
# highlight using a multi-group aggregation example for speed. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()


# filter example: filter selection of declarant and partner countries

tic()
df_filt <- df[declarant_iso %in% c("FR","DE","GB") &
              partner_iso %in% c("US","JP","VN")]
toc()

setkey(df,declarant_iso,partner_iso)
tic()
df_filt <- df[declarant_iso %in% c("FR","DE","GB") &
                partner_iso %in% c("US","JP","VN")]
toc()

#' *filter using multi-key is ~ 30% quicker!* 


# group_by example
tic()
df_agg <- df[, 
             .(total_imports = sum(value_in_euro),
               mean_imports = mean(value_in_euro),
               sd_imports = mean(value_in_euro)), 
             by = .(declarant_iso, partner_iso)]
toc()

setkey(df, declarant_iso,partner_iso)

tic()
df_agg <- df[, 
             .(total_imports = sum(value_in_euro),
               mean_imports = mean(value_in_euro),
               sd_imports = mean(value_in_euro)), 
             by = .(declarant_iso, partner_iso)]
toc()

#' *slower in the groupby!* *much faster for filtering*


# remove key:

setkey(df, NULL)



## 7. joins -------------------------------------------------


tbc. 


## 8. chaining (%>%) -----------------------------------------

#' Data.Table offers unique features there makes it even more effiecient. 
#' by chaining, this results in you not having to store your results
#' as an object and create a new command from this object
#' For example saving a dataframe - to then update this df
#' this can be done in one code chunk. This makes the speed of running the code mroe quickly. 
#' It is simple: you just ensure the commands (i.e. create new column) square brackets
#' comes at the end of the previous commends closed square bracket. 
#' *Example*
#' Highlight chaining by selecting columns, filter data, then create columns
#' and highlighting how this is done and speed efficency. 
#


df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

tic()
# select columns
df2 <- df[,.(declarant_iso,partner_iso,product,period,value_in_euro,quantity_kg)]
# filter data
df2 <- df2[declarant_iso %in% c("GB","IE")]
# create columns
df2 <- df2[, `:=`(value_in_euro_mil = value_in_euro / 1000,
             quantity_kg_tonne = quantity_kg /1000)]
toc()


# chained:

tic()
df2 <-  df[,.(declarant_iso,partner_iso,product,period,value_in_euro,quantity_kg)
           ][declarant_iso %in% c("GB","IE")
           ][, `:=`(value_in_euro_mil = value_in_euro / 1000,
                  quantity_kg_tonne = quantity_kg /1000)]
toc()


#' *chaining increases this run time slighlty, but provides a cleaner way to write the code* 



#' with using piping %>%
#' which makes writing code in data.table as easy and neat. 
#' similar with tidyverse you use the %>% to reference the next command
#' you do this too, however, before the next command
#' you need to ensure a '.' is placed before. 
#' 


tic()
df2 <-  
  df[,.(declarant_iso,partner_iso,product,period,value_in_euro,quantity_kg)] %>%
  .[declarant_iso %in% c("GB","IE")] %>%
  .[, `:=`(value_in_euro_mil = value_in_euro / 1000,
         quantity_kg_tonne = quantity_kg /1000)]
toc()


#' *combining piping with data.table is much slower than chaining.*



# data.table comparisons ----------------------------------------
## 0. data upload -----------------------------------------------

#' the first example is to highlight the speed uploading a 1 mn row csv file. 
#' data.table has the fread function
#' 


tic()
# compare the urn time of .csv vs_csv. 
#df <- read.csv("data/uk-tariff-measures-on-declarable-commodities.csv") # compare the urn time of .csv vs_csv. 
df <- readr::read_csv("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()
  
# using system.time to time code run time
system.time({df <- read_csv("data/uk-tariff-measures-on-declarable-commodities.csv")})
system.time({df <- fread("data/uk-tariff-measures-on-declarable-commodities.csv")})

# fread can read in .dat files too. 10mn row dataset. 
tic()
df2 <- data.table::fread("data/prefs2019.dat")
toc()

# compared with read.delim
tic()
df2 <- read.delim("data/prefs2019.dat", header = TRUE, dec = ",")
toc()

# compared with read_del in the readr package
tic()
df2 <- readr::read_delim("data/prefs2019.dat", ",", col_names = TRUE)
toc()

# readr read_delim is slightly quicker than data.table! Only slighlty so use at your own discretion. 

# running vs read_csv (which is relatively quick) the run time using fread is ~ 40-60% quicker
# after a few test runs. 


## 1. columns -------------------------------------

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

df <- bind_rows(df,df,df) # increase size of data for speed test. 
#### create columns -------------------------------

#' this section will compare creating columns and updating several at once. 
# create 3 character columns:
# base R
tic()
df$hs2 <- substr(df$commodity__code,1,2)
df$hs4 <- substr(df$commodity__code,1,4)
df$hs6 <- substr(df$commodity__code,1,6)
toc()

# tidyverse

tic()
df2 <- df %>%
  mutate(
    hs2 = str_sub(commodity__code,1,2),
    hs4 = str_sub(commodity__code,1,4),
    hs6 = str_sub(commodity__code,1,6)
  )
toc()

tic()
df2 <- df[, 
          c("hs2", "hs4","hs6") := 
          list(
            str_sub(commodity__code,1,2),
            str_sub(commodity__code,1,4),
            str_sub(commodity__code,1,6)
          )]

toc()

#' *data.table IS slightly quicker creating 3 new columns than tidyverse.* some runs tidyverse is faster however!
#' 

#

### update columns -------------------------

# example1 - update numeric columns to character

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

# tidyverse

tic()
df2 <- df %>%
  mutate_if(is.numeric,as.character)
toc()

# data.table
tic()
cols <- names(Filter(is.numeric, df)) # identify column names which are numeric. 
df3 <- df[, (cols) := lapply(.SD, FUN = as.character), .SDcols = (cols)]
toc()

#' *data.table is faster*

# example2 - update columns to remove a string

# tidyverse

tic()
df2 <- df %>%
  mutate(
    across(
      c(
        measure__type__description,
        measure__geographical_area__description,
        commodity__description
        ),
      ~ stringr::str_remove_all(.x,"S")
     )
    )
toc()


# data.table

tic()
func <- function(.x){stringr::str_remove_all(.x,"S")}
cols = c("measure__type__description",
         "measure__geographical_area__description",
          "commodity__description")
df2 <- df[, (cols) := lapply(.SD, FUN = func), .SDcols = (cols)]
toc()


#' *data.table is faster*. The run time varies between 5-20% faster. 

#### remove columns -------------


## 2. filter data -----------------------------------------

#' filter large preferntial imports data from the EU
#' by selection of countries. 
#' compare tidyverse against data.table + with keys set. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# tidyverse

tic()
df_filt <- df %>% 
  filter(declarant_iso %in% c("GB","IE","DE","FR") &
         partner_iso %in% c("US","CA","MX"))
toc()

# data.table

tic()
df_filt <- df[declarant_iso %in% c("GB","IE","DE","FR") &
                partner_iso %in% c("US","CA","MX")]
toc()


#' *data.table without keys up to 90% faster for filtering large data!* 

setkey(df,declarant_iso,partner_iso)
tic()
df_filt <- df[declarant_iso %in% c("GB","IE","DE","FR") &
                partner_iso %in% c("US","CA","MX")]
toc()

#' *even faster with keys set!*

setkey(df,NULL)

# second exmaple using numerical filtering. 

# tidyverse
tic()
df_filt <- df %>% filter(value_in_euro >= 1000000 & quantity_kg >= 10000)
toc()

tic()
df_filt <- df[value_in_euro >= 1000000 & quantity_kg >= 10000]
toc()

#' *40-50% faster*        

## 3. aggregations ----------------------------------------

#' aggregations using tidyverse can sometimes be slow. It will be interesting
#' to see the compairson manioulating a large data set. 


df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# example: aggregation multiple values using large EU preferntial import data

# tidyverse
tic()
df_agg <- df %>%
  group_by(declarant_iso, partner_iso) %>%
  summarise(
    total_imports = sum(value_in_euro),
    mean_imports = mean(value_in_euro),
    total_imports_weight = sum(quantity_kg),
    count = length(declarant_iso),
    stndv = sd(value_in_euro),
    max_val = max(value_in_euro)
  )
toc()

# data.table
tic()
df_agg <- df[, .(total_imports = sum(value_in_euro),
                 mean_imports = mean(value_in_euro),
                 total_imports_weight = sum(quantity_kg),
                 count = length(declarant_iso),
                 stndv = sd(value_in_euro),
                 max_val = max(value_in_euro)),
             by = .(declarant_iso,partner_iso)]
toc()


#' *data.table is  faster* following a few runs, this varied between 5%-50% faster. 

## 4. combined query -----------------------

#' Example: extract pref imports data and filter for certain type. 
#' # create columns and aggregate the data to EU level. 
#' (All declarant countries are EU28 states)

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

tic()
df2 <- df %>% tibble() %>%
  filter(use %notin% c("U30", "U31")) %>% 
  mutate(year = str_sub(period, 1, 4)) %>% 
  filter(declarant_iso != "GB") # filter out uk
toc()
 
## data for total EU
tic()
df_eu <- 
  group_by(df2,
    partner_iso,
    product,
    year
    ) %>%
  summarise(
    value = sum(value_in_euro),
    weight = sum(quantity_kg)
  ) %>%
  mutate(declarant_iso = "EU")
toc()

#' this large aggregation takes a very long time!

# data.table

tic()
df2 <- df[use %notin% c("U30","U31")] %>%
  .[, year := str_sub(period,1,4)] %>%
  .[declarant_iso != "GB"]


setkey(df2, partner_iso,product,year)
df_eu <- df2[, .(value = sum(value_in_euro),
                 weight = sum(quantity_kg)), by = .(partner_iso,product,year)] %>%
  .[!is.na(partner_iso)]
toc()


#' * 100x faster using data.table to aggregate large amount of data!*         




# End. 