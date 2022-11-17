# 11 data.table

#' the data.table package is a powerful alternative to base R (more concise code) and 
#' tidyverse (much quicker) and enables users to handle data on much larger scales. 
#' the purpose of this script is to highlight the processing power and 
#' difference between methods (base, data.table, tidyverse) for computing and wrangling data. 
#' 
#' This script also serves as a repository of basic data.table functions
#' and uses to wrangle and transform data for basic data analysis and exploration. 
#' 
#' A large part of my reasoning for learning data.table is 
#' to find ways to speed up the increasingly complex code
#' and larger data sets  I work with. 
#' Further expanding on basic data wrangling I have explored the package further
#' and will continue to add to this script as I progress. 
#' 

#' helpful resource: <https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/>
#' further inspiration for exploring data.table: https://eliocamp.github.io/codigo-r/en/2019/07/why-i-love-data-table/
#' great blog with examples: http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#1-data-structures--assignment
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

df <- uk_trqs[, .(quota_number,quota_origin,quota_year,fill_rate)]

#' multiple columns can be selected using a character array

cols <-c("quota_number","quota_origin","quota_year","fill_rate")
df <- uk_trqs[, cols, with = FALSE]

# drop columns
# define dropped columns within an array and denoted using ! to drop the columns. 
drop_cols <-c("year","fill_rate")
df2 <- df[, !(drop_cols), with = FALSE]

#rename columns you can use base are setnames etc. I will compare this for speed vs rename in dplyr later. 

setnames(df,c("fill_rate","quota_year"),c("quota_fill_rate","year"))

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


### filter na ----------------

#' data.table has its own na.omit function. 
#' 

# create data with NAs from large dataset for comparison. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()
df <- df %>% mutate(declarant_iso = ifelse(declarant_iso=="GB",NA,declarant_iso),
                    partner_iso = ifelse(partner_iso=="JP",NA,partner_iso))

tic()
df2 <- df[!is.na(declarant_iso)]
toc()
#' ~ 2-3seconds. 

tic()
df3 <- na.omit(df,cols="declarant_iso")
toc()

#' *after a few runs it appears na.omit is faster ~ 20-40%* 


# filter NA with multiple columns:

tic()
df2 <- df[(!is.na(declarant_iso) & !is.na(partner_iso))]
toc()


tic()
df2 <- na.omit(df,cols=c("declarant_iso","partner_iso"))
toc()

#' *na.omit is quicker by aorund 40% filtering on two columns for NAs* 


### subset ------------------------------------------------------



### between() ---------------------------------------------------

#' Convenience functions for range subsets
#' you can more simply filter a dt by a value range

tic()
df2 <- df[value_in_euro > 100000 & value_in_euro <= 2000000]
toc()

tic()
df2 <- df[between(value_in_euro,100000,2000000)]
toc()

#' *between 10-30% faster following a few runs*

### chmatch() --------------------------------------------

#' chmatch returns a vector of the positions of 
#' (first) matches of its first argument in its second. Both
#' arguments must be character vectors.
#' %chin% is like %in%, but for character vectors.
#' 
#' faster replacement than using %in%. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

partner_list <- c("US","JP","CN","VN","CA","MX")

tic()
df2 <- df[partner_iso %in% partner_list]
toc()

tic()
df2 <- df[partner_iso %chin% partner_list]
toc()

#' *faster using %chin%, sometimes significantly depending on the run*


### %like% ------------------------------------------------

#' filtering data using pattern recognition/search
#' is a very common command using grepl. 
#' data.table you can perform similar command using %like%
#' equiv. of the LIKE command in SQL. 


tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

df <- rbind(df,df,df) # make larger df to test speed

tic()
df2 <- df[grepl("Eggs",commodity__description)]
toc()

tic()
df3 <- df[commodity__description %like% "Eggs"]
toc()

#' *speed execution is essentially the same as using grepl* 


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

### SDcols --------------------------------------------------

#' SD cols allows you to perform operations on a subset of rows within the datatable. 
#' you can use to to summarise values, such as a mean/sum
#' and apply functions to multiple columns at once - very useful!

df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")

# apply function across multiple columns:
tic()
func = function(.x){str_remove(.x, "S")}
df2 <- df[, (5:8) := lapply(.SD, FUN=func), .SDcols = (5:8)]
toc()

cols = c("commodity__description","measure__type__description","measure__geographical_area__description")
df2 <- 
  df[, 
      (cols) :=
      lapply(.SD, FUN=func), .SDcols =  (cols)
]

### fifelse ----------------------------------------------------------

#' fast if else. a fast way to perform ifelse statements using data.table
#' 

tbc. 



### case when (fcase) ------------------------------------------------

#' case when is a useful function to create a column using conditional logic. 
#' commonly used case_when from dyplr (equiv. to CASE WHEN in SQL) 
#' and commonly used ifelse statements to create columns
#' data.table has its own unique function fcase. 
#' 

#' simple example create a new column based on the value_in_euro column. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

tic()
df2 <- 
  df[, 
     val := 
       fcase(
       value_in_euro <= 10000, "Small",
       value_in_euro <= 100000, "Medium",
       value_in_euro > 1000000, "Large"
        )
      ]
toc()

# using dplyr case_when:
tic()
df3 <- 
  df[, 
     val := 
       case_when(
         value_in_euro <= 10000 ~ "Small",
         value_in_euro <= 100000 ~ "Medium",
         value_in_euro > 1000000~ "Large"
       )
  ]
toc()

#' *fcase is much much faster!*



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


### SDcols -----------------------------------------------

# you can aggregate using .SD and SDcols arugment

tic()
df_agg <- df[, 
             .(total_imports = sum(value_in_euro),
               total_weight = sum(quantity_kg)), 
             by = .(declarant_iso, partner_iso)]
toc()

tic()
df_agg <- df[, lapply(.SD, sum), 
             .SDcols = c("value_in_euro",
                         "quantity_kg"),
             by = .(declarant_iso,partner_iso)
           ]
toc()

#' *grouping using SD cols or with the normal syntax, run times are very similar* 



###  .N operator --------------------------------------------

#' .N operator allows of quick and simple use of counting number of occurneces
#' across variables and groups. 
#' tidyverse equiv would be tally() which is simple and effective. 

#' Example: count the total number of quotas in a year for each country:
df <- uk_trqs %>% filter(quota_year == 2022)
df[, .N, by = quota_origin][order(-N)] # chain with order function to arrnage output


df <- uk_trqs
df[, .N, by = c("quota_origin","quota_year")][order(-N)] # chain with order function to arrnage output

df2 <- df[, .N, by = c("quota_origin","quota_year")][order(-N)] 

# .N using large dataset vs tally():

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

tic()
count = df[, .N, by = "measure__geographical_area__description"][order(-N)] # chain with order function to arrnage output
toc()

tic()
count <- df %>% group_by(measure__geographical_area__description) %>% tally()
toc()

#' *using .N to count is ~ 10x quicker than using tidyverse tally() !*



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


# setting multiple keys:

# highlight using a multi-group aggregation example for speed. 

df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# filter example: filter selection of declaration and partner countries

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


### remove key ---------------------------------------------

setkey(df, NULL)


## 7. joins -------------------------------------------------

#' joining data tables together people are familiar with base R merge
#' and tidyverse' left_join. 
#' data.table has a specifically designed merge, with simple syntax and inputs. 


# upload example data:
df <- data.table::fread("data/uk_trqs.csv") %>% clean_names()
# create two distinct data frames to match against. 

df <- df %>% filter(quota_year == 2021) %>% distinct(quota_number, .keep_all = TRUE) %>% select(1:4,10:13)
df1 <- setDT(head(df,100))
df2 <- setDT(head(df,250))

#' two dataframes with no duplicate keys (quota_number). 
#' which can be simply matched against to demonstrate joins. 
#' There are 50 records in df2 which are not in df1. All records in df2 are in df1. 
#' 

# left join

df3 <- merge.data.table(df1,df2, by = "quota_number")

# full join 
df3 <- merge.data.table(df1,df2, by = "quota_number", all.y = TRUE) # include all in y data table. 
df3 <- merge.data.table(df2,df1, by = "quota_number", all = TRUE)

# using data.table key

setkey(df1,quota_number)
setkey(df2,quota_number)

df3 <- merge.data.table(df1,df2) # default is left join (i.e. all in x which match y). 

# bind rows/cols you can use rbind/cbind/bind_rows/bind_cols as with normal dataframes. 
# speeds tests to follwing in second seperate of this script. 


# joins using multi-keys. i.e. multiple by arguments. 

df <- data.table::fread("data/uk_trqs.csv") %>% clean_names()
# create two distinct data frames to match against. 
# here there are two rows per quota number, one per year. 
df <- df %>% distinct(quota_number,quota_year, .keep_all = TRUE) %>% select(1:5,7,13)
df1 <- setDT(head(df,100))
df2 <- setDT(head(df,250))


df3 <- merge.data.table(df1,df2, by = c("quota_number"="quota_number",
                                        "quota_year"="quota_year"))

# full join
df3 <- merge.data.table(df1,df2, by = c("quota_number"="quota_number",
                                        "quota_year"="quota_year"), all = TRUE)

# setkeys
setkey(df1,quota_number,quota_year)
setkey(df2,quota_number,quota_year)

df3 <- merge.data.table(df1,df2)
df3 <- merge.data.table(df1,df2, all = TRUE)


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


#' *combining piping with data.table is much slower than chaining but can be easier to read and follow*

## 9. pivot data -----------------------------------------------

tbc. 

### melt (wide/long) -------------------------------------------



### dcast (long/wide) ------------------------------------------




## 10. other operators ------------------------------------------

### setorder() --------------------------------------------------

#' re-order data. Similar to tidyverse arrange. 
#'
df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# re-order based on size of import value
tic()
df2 <- setorder(df,-value_in_euro) # default is lowest to highest (to change denote using '-')
toc()

tic()
df2 <- setorder(df, declarant_iso,partner_iso)
toc()

tic()
df2 <- df %>% arrange(declarant_iso,partner_iso)
toc()

#' *setorder is 90%+ quicker than tidyverse arrange equiv!* 


### duplicates ------------------------------------------------


#' you can remove duplicates using data.tables unique function
#' for fast removal of duplicate rows or based on specific columns. 
#' data.table has two function to help identfiy and revmoe duplicates
#' unique() and duplicates()

tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

# identify all duplicate commodity codes

# duplicated:
#' duplicated with highlight the first row or duplicating data/grouping as FALSE
#' TRUE is denoted for all further duplicate rows/groupings. 

# creates logical vector determining if any cell is duplicated
tic()
df2 <- duplicated(df, by = "commodity__code")
toc()
# create vector as a column:
tic()
df[, dup := duplicated(df,by="commodity__code")]
toc()

# multi column duplication:
tic()
df[, dup := duplicated(df,by=c("commodity__code","measure__duty_expression"))]
toc()



# unique removes duplicate rows defined by the grouping/columns selected. 

tic()
df2 <- unique(df, by = "commodity__code")
toc()

tic()
df2 <- unique(df, by = c("commodity__code","measure__duty_expression"))
toc()

### frank() ----------------------------------------------------

#' fast ranking function 
#' 

# Example rank highest value imports and by grouping(s):
df <- data.table::fread("data/prefs2019.dat") %>% clean_names()

# create rank columns:
df2 <- df[, rank := frank(df,-value_in_euro)]
setorder(df2,rank)  

# create rank within groups:
tic()
df2 <- df[, rank := frank(-value_in_euro), by =declarant_iso]
setorder(df2,declarant_iso,-value_in_euro)
toc()

# creates ordered dt of declaring countries top imports from each partner country:
tic()
df2 <- df[, rank := frank(-value_in_euro), by = .(declarant_iso,partner_iso)]
setorder(df2,declarant_iso,partner_iso,-value_in_euro)
toc()

#' *4 seconds to group-rank and re-order 10mil rwo dt*

# example with small dataset:
df <- uk_trqs
df2<-df[,therank := frank(-fill_rate, ties.method ="first"),by=.(quota_origin,quota_year)]
setorder(df2,quota_origin,quota_year,-fill_rate)



### fwrite (save csv) -------------------------------------------

#' save csv files quickly. like write.csv but much faster. 
#' 
tic()
df <- data.table::fread("data/uk-tariff-measures-on-declarable-commodities.csv")
toc()

tic()
write.csv(df,"outputs/csv_output1.csv")
toc()
#' run1: 88 seconds
#' 

tic()
fwrite(df,"outputs/csv_output2.csv")
toc()

#' *run1: close to 100x faster than write.csv* 

### nafill ------------------------------------------------------

#' fill in blank cells with value, 
#' whether constant value or values form cells above or below. 
#'


x = 1:10
x[c(1:2, 5:6, 9:10)] = NA
dt=data.table(col=x)
dt[,col:=nafill(dt,"locf")] # fill values from cell above. 
dt[,col:=nafill(dt,"nocb")] # fill values from cell below
dt[,col:=nafill(dt,"const",fill=0)] # fill values with constant value and set value


## 11. colnames function args -----------------------------------

#' a common practice I have used is inserting column names
#' as function arguments using tidyverse. 
#' I use !!as.symbol(x) to extract a funciton input
#'  as a character to create columns for example. 
#'  This can be done different;y using data.table
#'  

tbc.

http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#3-functions
  



# End. 


