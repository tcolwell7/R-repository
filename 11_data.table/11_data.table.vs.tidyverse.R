#' data.table vs tidyverse operation speed testing
#' I am exploring different data.table operations to
#' see how efficient they are when transforming large datasets
#' I am very comfortable in tidyverse and love it syntax for ease of reading
#' and following through to review code
#' I want to see how well data.table performs vs tidyverse
#' combined with the ease and efficiency of using either syntax. 
#' 


rm(list=ls()) # remove everything form global environment.

library(tidyverse)
library(openxlsx) 
library(readxl)
library(janitor)
library(stringr)
library(tictoc) # simple function to monitor code chunk run time
library(data.table) # data.table package for high-speed data processing. 
library(vroom)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]"))# set working directory to where your file is saved. 

`%notin%` <- Negate(`%in%`)



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

# vroom:

tic()
df <- vroom::vroom("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

#' *data/table is 2x quicker than readr. Vroom is 0.5-1x faster than data/table*


# fread can read in .dat files too. 10mn row dataset. 
tic()
df2 <- data.table::fread("data/prefs2019.dat",sep=",")
toc()

# compared with read.delim
tic()
df2 <- read.delim("data/prefs2019.dat", header = TRUE, dec = ",")
toc()

# read.delim is slow in comparison. Do not use. 

# compared with read_del in the readr package
tic()
df2 <- readr::read_delim("data/prefs2019.dat", ",", col_names = TRUE)
toc()

# vroom:

tic()
df2 <- vroom::vroom("data/prefs2019.dat",delim=",")
toc()

#' *readr read_delim is slightly quicker than data.table! Only slighlty so use at your own discretion.*
#' *vroom is 1 to 2x faster than both rad_delim and fread*


## 1. columns -------------------------------------

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

df <- bind_rows(df,df,df) # increase size of data for speed test. 


#### create columns -------------------------------

#' this section will compare creating columns and updating several at once. 

# tidyverse
tic()
df2 <- df %>%
  mutate(
    hs2 = str_sub(commodity__code,1,2),
    hs4 = str_sub(commodity__code,1,4),
    hs6 = str_sub(commodity__code,1,6)
  )
toc()

# data.table 1:
tic()
df2 <- df[, 
          c("hs2", "hs4","hs6") := 
            list(
              str_sub(commodity__code,1,2),
              str_sub(commodity__code,1,4),
              str_sub(commodity__code,1,6)
            )]

toc()

# data.table2:

tic()
df2 <-
  df[,
     `:=`(hs2 =  str_sub(commodity__code,1,2),
          hs4 =  str_sub(commodity__code,1,4),
          hs6 =  str_sub(commodity__code,1,6)
          )
     ]
toc()

#' *data.table IS slightly quicker creating 3 new columns than tidyverse.* some runs tidyverse is faster however!
#' * the second data.table method appears slighlty quicker than the first* will need to test this further on a larger dataset. 


### ifelse -------------------------------------

#' Creating columns based on conditions is very common. 
#' I often use mutate combined with ifelse. case_when is also very popular.
#' These commands can be done in data.tables purposely built functions. 
#' How do they compare?
#' Example: create a conditional column on 10mn row data frame:

tic()
df <- vroom::vroom("data/prefs2019.dat",delim=",") %>% clean_names()
toc()

# tidyverse:
tic()
df2 <- df %>% 
  mutate(
    val = ifelse(value_in_euro <= 10000, "Small",
          ifelse(value_in_euro <= 100000, "Medium", 
          ifelse(value_in_euro <= 1000000, "Medium-Large",
          ifelse(value_in_euro <= 10000000, "Large","Very-Large")
          )
        )
      )
    )
toc() 


dt <- setDT(df)

tic()
dt2 <- 
  dt[, 
     val := fifelse(value_in_euro <= 10000, "Small",
            fifelse(value_in_euro <= 100000, "Medium", 
            fifelse(value_in_euro <= 1000000, "Medium-Large",
            fifelse(value_in_euro <= 10000000, "Large","Very-Large")
          )
         )
       )
  ]

toc() 

#' * data.table & fifelse are x20 faster than tidyverse*

### case_when -------------------------------------------

# expanding on the above example using more conditions:

tic()
df <- vroom::vroom("data/prefs2019.dat",delim=",") %>% clean_names() %>%mutate(month = str_sub(period,-2,-1))
toc()

# tidyverse:

tic()
df2 <- df %>%
  mutate(
    monthName = 
      case_when(
        month == "01" ~ "Jan", month == "02" ~ "Feb",
        month == "03" ~ "Mar", month == "04" ~ "Apr",
        month == "05" ~ "May", month == "06" ~ "Jun",
        month == "07" ~ "Jul", month == "08" ~ "Aug",
        month == "09" ~ "Sep", month == "10" ~ "Oct",
        month == "11" ~ "Nov", month == "12" ~ "Dec",
        month == "52" ~ "JanToDec"
        )
    ) 
toc()


# data.table:

dt <- setDT(df)


tic()
dt2 <- 
     dt[, 
      monthName := fcase(
               month == "01","Jan",month == "02","Feb",
               month == "03","Mar",month == "04","Apr",
               month == "05","May",month == "06","Jun",
               month == "07","Jul",month == "08","Aug",
               month == "09","Sep",month == "10","Oct",
               month == "11","Nov",month == "12","Dec",
               month == "52","JanToDec"
             )
     ]
toc()


#' *fcase and data.table are 10x faster!*

### update columns ------------------------------

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


#### remove columns --------------------------------------

#' simple ease of dropping columns in tidyverse vs data.table

tic()
dt <- data.table::fread("data/prefs2019.dat",sep=",") %>% clean_names()
toc()


# tidyverse:

df <- tibble(dt)

tic()
df2 <- df %>% select(-sup_quantity,-quantity_kg,-declarant)
toc()

# data.table 1:
tic()
drop_cols<-c("sup_quantity","quantity_kg","declarant")
dt2 <- dt[, !(drop_cols), with = FALSE]
toc()

# data.table 2:
tic()
dt2 <- dt[, c("sup_quantity","quantity_kg","declarant") := NULL]
toc()


dt2 <- dt %>% select(-sup_quantity,-quantity_kg,-declarant)
toc()

#' * using tidyverse to drop columns is quicker than data.table method1*
#' * data.table method 2 is faster than tidyverse method*
#' 


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

#' *even faster with keys set ~ 10x faster!*

setkey(df,NULL)

# second example using numerical filtering. 

# tidyverse
tic()
df_filt <- df %>% filter(value_in_euro >= 1000000 & quantity_kg >= 10000)
toc()

tic()
df_filt <- df[value_in_euro >= 1000000 & quantity_kg >= 10000]
toc()

#' *close to 4x faster using data.table*        

## 3. aggregations ----------------------------------------

#' aggregations using tidyverse can sometimes be slow. It will be interesting
#' to see the comparison manipulating a large data set. 


dt <- data.table::fread("data/prefs2019.dat") %>% clean_names()
df <- tibble(dt)
# example: aggregation multiple values using large EU preferential import data

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
setkey(dt,declarant_iso,partner_iso)
dt_agg <- dt[, .(total_imports = sum(value_in_euro),
                 mean_imports = mean(value_in_euro),
                 total_imports_weight = sum(quantity_kg),
                 count = length(declarant_iso),
                 stndv = sd(value_in_euro),
                 max_val = max(value_in_euro)),
             by = .(declarant_iso,partner_iso)]
toc()


#' *data.table is  faster by up to 2x for this example*

## 4. combined queries -----------------------

#' Example: extract pref imports data and filter for certain type. 
#' # create columns and aggregate the data to EU level. 
#' (All declarant countries are EU28 states)

dt <- data.table::fread("data/prefs2019.dat") %>% clean_names()
df <- tibble(df)

tic()
df2 <- df %>%
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
dt2 <- dt[use %notin% c("U30","U31")] %>%
  .[, year := str_sub(period,1,4)] %>%
  .[declarant_iso != "GB"]


setkey(dt2, partner_iso,product,year)
dt_eu <- dt2[, .(value = sum(value_in_euro),
                 weight = sum(quantity_kg)), by = .(partner_iso,product,year)] %>%
  .[!is.na(partner_iso)]
toc()


#' * 100x faster using data.table to aggregate large amount of data!*         


# End. 