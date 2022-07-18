# 07 officer - package to scrape data from word documents and power point slides. Combining with tidyverse makes it nice and straight forward. 


#' *For commentary please refer to makrdown file* 

# Set up ---------------------------

rm(list = ls())

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(ggplot2)
library(officer)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

# Scrape word docs --------------------------------------------------------------
## Example 1 --------------------------------------------------------------------
### scrape doc ------------------------------------------------------------------
# scrape the doc using read_docx and docx_summary. 

doc_text <- read_docx("inputs/example_word_doc.docx") # read doc. info. 
data <- docx_summary(doc_text) # extract docu data. 


#### compile table1 data --------------------------------------------------------


header1 <-
  data %>%
  filter(is_header) %>% # default filter is_header = TRUE
  filter(doc_index == 5) %>%
  select(text) %>%
  pull()

# headers are stored as an array. 
# this can easily be combined with the data to create a df. 

# filter data for table doc_index
# data is in long format - we need to select necessary data and format as wide
df <- data %>%
  filter(!is_header) %>% # filter non header data
  filter(doc_index == 5) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>%
  set_names(header1) # set df column names as header data scraped 

# data is now scraped!
# further steps may be required or desired depending on your data table
# and how you want it formatted. 
# but in general these are the neccessary steps to scrape and transform the data
# into a wide datatable format. 

# further steps to clean data

df <- df %>%
  mutate(across(.cols = -1, .fns = parse_number)) %>% # remove nont numeric values. 
  rowid_to_column() # I like having a row id. This is handy incase you want to match two data tbales together for QA. 



#### compile table2 data ----------------------------------------------------

header2 <-
  data %>%
  filter(is_header) %>% # default filter is_header = TRUE
  filter(doc_index == 7) %>%
  select(text) %>%
  pull()


df2 <- data %>%
  filter(!is_header) %>% # filter non header data
  filter(doc_index == 7) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>%
  set_names(header2) 

# Same process has been followed but changing doc_index. 
# This can be stored as a function 
# so to help not repeat code snippets if scrpaing multiple tables. 


word_doc_scrape <- function(.doc_index, .data){
  
  header <-  
    .data %>%
    filter(is_header) %>% # default filter is_header = TRUE
    filter(doc_index == .doc_index) %>%
    select(text) %>%
    pull()
  
  
  df <- 
    .data %>%
    filter(!is_header) %>% # filter non header data
    filter(doc_index == .doc_index) %>% # filter first table
    select(text, row_id, cell_id) %>% # select necessary columns
    pivot_wider(names_from = cell_id, values_from = text) %>% 
    select(-row_id) %>%
    set_names(header)
  
  return(df)
}

df1 <- word_doc_scrape(docIndex = 5, .data = data)
df2 <- word_doc_scrape(docIndex = 7, .data = data)


## Example 2 -------------------------------------------------------------

# TRQ reference document contains 3 tables required to be scraped and compiled into useable format. 

doc     <- read_docx("inputs/uk_trq_ref_doc.docx")
data <- docx_summary(doc) %>% clean_names()

### identify doc_index ---------------------------------------------------

doc_ind <- 
  doc2 %>%
  filter(content_type == "table cell") %>%
  distinct(doc_index) %>%
  select(doc_index) %>%
  pull()

print(doc_ind)

#table doc_indexes are 18. 35 and 66. 

# extract table column headers
# browsing the document and data
# column headers are the same across all tables
# so need to remove duplicates

### scrape headers ---------------------------------------

headers <- data %>% 
  filter(is_header) %>% 
  distinct(text, .keep_all = TRUE) %>%
  mutate(across(everything(), ~ str_remove_all(.x, "\\(|\\)"))) %>% # remove uneccessary brackets
  mutate(across(everything(), ~ gsub("[/0-9]", "", .x))) %>% # and numerical text
  pull(text)



### scrape data tables -----------------------------------
# extract data for each table. 

df1 <-
  data %>%
  filter(!is_header) %>% # filter non header data
  filter(doc_index == 18) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>%
  set_names(headers) # set df column names as header data scraped 

  
df2 <-
  data %>%
  filter(!is_header) %>% # filter non header data
  filter(doc_index == 35) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>%
  set_names(headers) 

df3 <-
  data %>%
  filter(!is_header) %>% # filter non header data
  filter(doc_index == 66) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>% 
  set_names(headers) 

# note df3 doens't run. The funciton scrapes an empty column
# This is due to the text column having NAs
# where the function has scraped columns which are merged into two columns
# (notice the col_span column = 2 for quoat number). 
# We can avoid any issues by removing NAs form the text. 


df3 <-
  data %>%
  filter(!is_header) %>% # filter non header data
  filter(!is.na(text)) %>% # remove NAs. 
  filter(doc_index == 66) %>% # filter first table
  select(text, row_id, cell_id) %>% # select necessary columns
  pivot_wider(names_from = cell_id, values_from = text) %>% 
  select(-row_id) %>% 
  set_names(headers)


# The word_doc_scrape function won't work fully for all tables in this document. 
# The removing NA form text step is required. 

word_doc_scrape2 <- function(.doc_index, .data){
  
  header <-  
    .data %>%
    filter(is_header) %>% # default filter is_header = TRUE
    filter(doc_index == .doc_index) %>%
    select(text) %>%
    pull()
  
  
  df <- 
    .data %>%
    filter(!is_header) %>% # filter non header data
    filter(!is.na(text)) %>% #### new step in function
    filter(doc_index == .doc_index) %>% # filter first table
    select(text, row_id, cell_id) %>% # select necessary columns
    pivot_wider(names_from = cell_id, values_from = text) %>% 
    select(-row_id) %>%
    set_names(header)
  
  return(df)
}

df1 <- word_doc_scrape2(.doc_index = 18, .data = data)
df2 <- word_doc_scrape2(.doc_index = 35, .data = data)
df3 <- word_doc_scrape2(.doc_index = 66, .data = data)

# Automated function --------------------------------------------------------

# If you have a large document and don't want to browse through and find the correct
# doc_index - you cna automate this process through the use of a loop.
# you can extract the doc_index numbers and then loop through them creating dfs. 


auto_word_doc_scrape <- function(.data){

  # identify doc_index
  doc_ind <- 
    data %>%
    filter(content_type == "table cell") %>%
    distinct(doc_index) %>%
    select(doc_index) %>%
    pull()
 
  
  list <- list() # empty list to store dfs.
  for(i in doc_ind){
  
    print(i)
    
  nm <- paste0("df_",i)  
  
  header <-  
    .data %>%
    filter(is_header) %>% # default filter is_header = TRUE
    filter(doc_index == i) %>%
    select(text) %>%
    pull()
  
  
  df <- 
    .data %>%
    filter(!is_header) %>% # filter non header data
    filter(!is.na(text)) %>% #### new step in function
    filter(doc_index == i) %>% # filter first table
    select(text, row_id, cell_id) %>% # select necessary columns
    pivot_wider(names_from = cell_id, values_from = text) %>% 
    select(-row_id) %>%
    set_names(header)
  
  list[[nm]]<- df # store scraped data table in list
  
  }
 
  list[["doc_index_arr"]] <- doc_ind # store doc indexes in list
  list2env(list,globalenv()) # return list items to global environment

}

auto_word_doc_scrape(data)

# to further illustrate the usefulness of automating this process.
# This function will work for the first example document
# I have compiled a further example with more data tables to
# further illustrate the usefulness of automation. 

#' *IMPORTANT NOTE ABOUT FUNCTION* 
#' The header aspect of the function assumes there are no merged cells. 
#' It implies there is one cell per column with text within. 
#' depending on the ata contents scrapped through. 
#' The step-by-step process to pull in and cimpile the data table
#' may not be universal. 
#' It is advised to analyses and text code which is applicble to the
#' tables you are trying to scrape
#' and if there are multiple tables to scrape I hope code can be utilised
#' in a more automated fashion combining functions and extracting the `doc_index`.   
#' 


# demo. with initial example document. 
doc_text <- read_docx("inputs/example_word_doc.docx") # read doc. info. 
data <- docx_summary(doc_text) # extract data

auto_word_doc_scrape(data)

doc_text <- read_docx("inputs/example_word_doc_2.docx") # read doc. info. 
data <- docx_summary(doc_text) # extract data

auto_word_doc_scrape(data)



# Scrape PowerPoint docs ------------------------------------------------

#' *TBC* 
#' 
#' 
#' 


# End.


