# Functions:


#' utilising functions within R helps write clean, concise and more powerful code. 
#' You can reduce repetitiveness within scripts by creating a function
#' Then repeating this throughout the script. 
#' When working with lots of code instead of everything being in a single script
#' If sections can be broken down and created using a function
#' This helps break up code effectively and helps with error checking when issues arise
#' Functions can be simple and complicated
#' they are accessible to all levels of user
#' I would strongly promote their use
#' And practicing creating functions for your own work
#' This is something I use in pretty much all scripts I now write
#' 

#' quoting hadley wickham
#'  "You should consider writing a function
#'  whenever you’ve copied and pasted a block of code more than twice
#'  (i.e. you now have three copies of the same code). 
#'

#' *recommended reading*
#' https://r4ds.had.co.nz/functions.html#introduction-12
#' 

#' in general this script will cover creating basic, multi-variable functions
#' utilising logic and null values
#' and creating custom error checking in-built into your functions. 



# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)

options(scipen=999)

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function

#upload data

trade_data <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()
tariff_data <- read_excel("..\\data\\tariff_data.xlsx")


# 1. simple function -----------------

# simple value input and print value:
func1 <- function(x){
  print(x)
}

func1(10)


# function to input countyr name to filter and aggregate data:

func2 <- function(c){
  
  data <- trade_data %>%
    filter(country_name == c) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
}

# ensure to define object output from function:
func2("Thailand")

df <- func2("Thailand")

## 1i. multiple variables -------------

# adding multiple variables to functions are simple and easy.
# just ensure if they aren't set as a default NULL
# they must be defined when applying the function

# create a function to filter and aggregate data where the data is defined:

trade_data2 <- trade_data

func3 <- function(c, # country filter
                  data # data input
                  ){
  
  df <- data %>%
    filter(country_name == c) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
}


df3 <- func3("Thailand")

# error output as data function input has not been defined.

df3 <- func3("Thailand", trade_data)
df3 <- func3("Thailand", trade_data2)


#' Default value as NULL
#' For example if you wanted to enter an additional argument
#' to aggregate the data
#' i.e flow/year/commodity code
#' you provide users the option by setting
#' NULL as the default


func4 <-function(c, # country filter
                     data, # data input
                     additional_arg = NULL){
  
  df <- data %>%
    filter(country_name == c) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
}

df4 <- func4("Thailand", trade_data)

#' NOTE: function runs without issue. 
#' However if you wanted to add additional text arguments
#' for code to interpret and run
#' you need an additional bit of code
#' so R looks at the string text

### 1.ii symbol arguments ---------------

#' continuing with the simple example of
#' filtering and aggregating trade data
#' based on user input of country and aggregation argument


#' *!!as.symbol** 
#' to be used to wrap to argument within

func5 <-function(c, # country filter
                 data, # data input
                 additional_arg = NULL){
  
  df <- data %>%
    filter(country_name == c) %>%
    group_by(country_name, !!as.symbol(additional_arg)) %>%
    summarise(value = sum(value_gbp))
  
}


df5 <- func5("Thailand", trade_data, "flow")
df5 <- func5("Thailand", trade_data, "year")
df5 <- func5("Thailand", trade_data, c("year","flow"))



# 2. conditional logic ---------------

#' combining functions and conditional logic
#' provides a useful way to create user-centered functions
#' and more dynamic functions
#' enabling more complicated functions
#' 

# function using conditional logic within

#' Example: function with inputs which determine code outcome
#' eg. input hs2/hs4/hs6 - to determine code sample.
#' 

func_con <- function(hs_chapter){
  
    if(hs_chapter == "hs2"){
    
      df <- trade_data %>% mutate(hs2 = str_sub(commodity_code,1,2))
    
  } else if(hs_chapter == "hs4"){
    
      df <- trade_data %>% mutate(hs4 = str_sub(commodity_code,1,2))
      
  } else if(hs_chapter == "hs6"){
    
     df <- trade_data %>% mutate(hs4 = str_sub(commodity_code,1,2))
    
  } else{
    
    print("Please input hs2, hs4 of hs6")
  }
  
} # end function


output  <- func_con("hs2")
output2 <- func_con("hs4")
output3 <- func_con("hs6")
output4 <- func_con("h6")

#' creating a simple function with conditional logic is as simple as that. 
#' It is important to practice and learn how to implement this
#' As when creating more complicated functions
#' It can be very useful to implemnt this method
#' I have created an example of a more complicated example
#' towards the end of this script. 


## 2i. NULL conditional logic -------------

#' You can pre-define your funciton inputs. 
#' Pre-defining variables as NULL
#' Results in no error when running a function if
#' a user hasn't put in a value for that variable. 
#' It also provides the benefit of being able to
#' Easily connect the functions input to the underlying code.
#' By setting a variable as null this makes it
#' easy to implement conditional logic
#' by using is.null(). 
#'

#' Example: create a function to filter data and aggregate
#' providing the option to aggregate data by flow
#' 

func_null <- function(country, flow_flag = NULL){ # simple set to NULL when defining the function
  
  if(is.null(flow_flag)){
    
  df <- trade_data %>% 
    filter(country_name == country) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
  } else{
    
    df <- trade_data %>% 
      filter(country_name == country) %>%
      group_by(country_name, flow) %>%
      summarise(value = sum(value_gbp))
    
  }
  
  return(df) # ensure return is used otherwise the function won't return an outptu when run. 
}

df_agg <- func_null(country = "Thailand",flow_flag = 1)
df_agg <- func_null(country = "Thailand",flow_flag = "Y") # note any non-null value will work
df_agg <- func_null(country = "Thailand")


## 2i. conditional execution -------------------------------------------------

#' There may be instances when writing a longer script where the
#' execution of a function is dependent on a variable
#' or output within the larger script. 
#' Regardless of the trigger
#' It is simple and easy to use logic to then execute a function. 

#' 

trigger_value <- "Y"

if(trigger_value == "Y"){
  
  df_agg <- func_null(country = "Thailand",flow_flag = "Y")
  
} else{
  
  output  <- func_con("hs2")
}

#' simple and easy to implement. 



# 3. error checking -------------------------------------------------------------

#' error checking within functions can be very helpful
#' especially when you are creating functions for others to use. 
#' If you have multiple function inputs
#' and spelling or the validity of inputs are important 
#' having a custom error message which helps
#' explain the issue and how to fix it
#' can help users solve the issue for themselves
#' save time
#' and ensure confidence of independent use of functions you create
#' 
#' an additional benefit can be
#' if you have written a relatively long or complicated function
#' or script for that matter
#' inserting snippets of code
#' which test logic or expected outcomes within the script
#' if these expectations aren't met
#' you can insert a custom error message which helps you
#' when error checking and you know which snippet or
#' chunk of code is the cause of the error. 
#' 
#'
#' This is something I'm incorporating into my codes more and more. 
#' It is very helpful and not as daunting as first thought. 
#'


#' *Example*: create a function to filter and aggregate data
#' build in an error check message to determine if the spelling
#' or input name is valid. 



# Simple check:
  
  # create a unique list of country names from.
  
  country_list <- unique(trade_data$country_name)
  
  # use logic to determine if input exisits in list:
  
  if(country %notin% country_list){
    message("ERROR: country does not exisit in trade data set. Please check spelling")
  }

#' *NOTE:* 
#' there are functions using rlang 
#' that stop the code running and are mroe helpful for error checking
#' For more informaiton please read through
#' https://adv-r.hadley.nz/conditions.html
#' 
  
# compare output differences:
#' message
#' warning
#' stop
#' abort
#' 

# message does not stop execution of remaining code
if(country %notin% country_list){
  message("ERROR: country does not exisit in trade data set. Please check spelling")
}

# message does not stop execution of remaining code
if(country %notin% country_list){
  warning("ERROR: country does not exisit in trade data set. Please check spelling")
}

if(country %notin% country_list){
  stop("ERROR: country does not exisit in trade data set. Please check spelling")
}

if(country %notin% country_list){
  abort("ERROR: country does not exisit in trade data set. Please check spelling")
}

#' all error functions are helpful. Depending on your output message it may 
#' be advantageous to use specific ones. 
#' I tend to chose abort for the output and how it displays highlighting
#' an list in the error. 

#' For example you can input arryas into a funciton argument. 
#' I may input three countries into the function argument ot filter the data
#' If multiple doen't exisit or are spelt wrong
#' It is helpful to identify these in the error message
#' 


country <- c("Thaland","Unitd States", "Taiwan","Vietnam")


  df <- data.frame(col = country)
  df2 <- trade_data %>% distinct(., country_name) 
  n <- df %>% filter(country %notin% df2$country_name)
  
  for(i in seq_len(nrow(n))){warning("Country name not valid for: ",n[i,1])}
  

## 3i. custom error check ------------------------------------------
  
# creating an example using a function:
  
# define error function with message:
errorCheck <- function(country){
  
  df <- data.frame(col = country)
  df2 <- trade_data %>% distinct(., country_name) 
  n <- df %>% filter(country %notin% df2$country_name)
  
  for(i in seq_len(nrow(n))){warning("Country name not valid for: ",n[i,1])}
}

# insert into function for seperate purpose (to filter dtaa in this case)
  
func_test <- function(country_input){
  
  errorCheck(country_input)
  
  df <- trade_data %>% filter(country_code %in% country_input)
  
}

func_test(country_input = c("Thailand","Taiwan","Unitd States","Vitnam"))


### 3ii. abort code ------------------------------------------------------

#' if an error is too great
#' you can stop or abort the code
#' You can write a logical test
#' and if this lgoic fails you can abort the code
#' with a custom error message
#' instead of the R generated error which would occur
#' if the code is fully ran. 
#' This is helpful to a) erorr check and know where the error occurs
#' and b) write a custom error message to help users 
#' 

#' *Example*: simple function to input country and year to then filter
#' and aggregate the data. 
#' The start of the function will have a simple error check 
#' as created in the above sections
#' if this check fails - the code stops before the rest is ran. 
#' 


func_abort <- function(country, yr){
  
  # error checks:
  # country
  
  if(country %notin% unique(trade_data$country_name)){
    abort("Country name input is not valid. Please check spelling.")
  }
  
  # year
  
  if(yr %notin% unique(trade_data$year)){
    
    # this output is cusotm to display the valid years to chose from within the data
    abort(
      c("Please enter a valid year:",
        unique(trade_data$year)
      ))
  }
  
  # if the checks pass the remainign code is ran:
  
  df <- trade_data %>%
    filter(country_name == country) %>%
    filter(year == yr) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
}


df2 <- func_abort("Thailand",2020)
df2 <- func_abort("Thailad",2020)
df2 <- func_abort("Thailand",2021)


#' compare abort output with stop. The same function, replaced with "stop". 
#' note abort is part of the rlang package. 

func_stop <- function(country, yr){
  
  # error checks:
  # country
  
  if(country %notin% unique(trade_data$country_name)){
    stop("Country name input is not valid. Please check spelling.")
  }
  
  # year
  
  if(yr %notin% unique(trade_data$year)){
    
    # this output is cusotm to display the valid years to chose from within the data
    stop(
      c("Please enter a valid year:",
        unique(trade_data$year)
      ))
  }
  
  # if the checks pass the remainign code is ran:
  
  df <- trade_data %>%
    filter(country_name == country) %>%
    filter(year == yr) %>%
    group_by(country_name) %>%
    summarise(value = sum(value_gbp))
  
}


df2 <- func_stop("Thailand",2020)
df2 <- func_stop("Thailad",2020)
df2 <- func_stop("Thailand",2021)

#' I find abort much more helpful if creating custom error messages with multiple outputs. 
#' for simple error messages and code abort - stop is also great. 


# 4. full function example -----------------------------------------------------

#' I have utilised all of the above methods
#' When creating mroe complicated, multi-input
#' functions designed for use by others
#' learning these techniques and functions
#' has been very helpful
#' and beneficial where I've been able to
#' create improved usable code across different
#' R capabilities. 
#' 

## missing -------------------------

#' missing can be used to test whether a value was specified
#'  as an argument to a function.

#' simple and effective use of base R function to test inputs have been entered. 

example <- function(x,y,z){
  
  if(missing(x)){
    print("Missing x argument")
  } else if(missing(y)){
    print("Missing y argument")
  } else if(missing(z)){
    print("Missing z argument")
  }
  
}

example(y=10,z=10)


#' *Example* 
#' I created a function which enables users
#' to make custom plots
#' selecting specific countries
#' and sector
#' to plot UK Tariff Rate Quota data. 
#' Error checks were built in
#' to help users error check and ensure
#' they could run the functions indepdently
#' to then update slide packs for further dissmeniation 
#' 

## 4i. error checks ------------------------------------------------

#' I createdseparate in-built checks to:
#' check quota units and regional names were valid
#' check if country inputs were valid
#' 

trqPlotErrorHandling <- function(region_arg, quota_unit_arg){
  
  #' Error handling function
  #' Identify where main inputs:
  #' Region & Quota Unit
  #' Which do not exist or misspelled
  #' Print list of valid options 
  #' 
  #'
  #' Inputs are consistent across all TRQ functions
  #' Region is the main filter to produce charts for
  #' TRQ quota unit is second main filter for user selection
  #' 
  
  # Logic if only region exists (default input for Quota Unit is NULL)
  if(is.null(quota_unit_arg)){
    
    if(region_arg %notin% unique(sectors_grouping$region)){
      abort(
        c("Please enter valid *.region* name from the available list",
          unique(sectors_grouping$region)
        ))
    }
    
  } else{
    
    
    if(region_arg %notin% unique(sectors_grouping$region)){
      abort(
        c("Please enter valid *.region* name from the available list",
          unique(sectors_grouping$region)
        ))}
    
    if(quota_unit_arg %notin% unique(sectors_region$quota_unit_final)){
      abort(
        c("Please enter valid Quota Unit name from the available list",
          unique(sectors_grouping$quota_unit_final)
          
        ))
    }
    
  } # end else bracket
  
} # end function bracket


sectorCheck <- function(sectorInput){
  
  #' convert character vector to data frame
  #' identify entries which don't exist in sectors list
  #' print warning message for each
  #' 
  
  df <- data.frame(col = sectorInput)
  dfs <- sectors_region %>% distinct(., trq_dsc) 
  n <- df %>% filter(col %notin% dfs$trq_dsc)
  
  for(i in seq_len(nrow(n))){warning("Sector name not valid for: ",n[i,1])}
  
}


countryCheck <- function(countryInput = NULL, groupingInput = NULL){
  
  #' convert character vector to data frame
  #' identify entries which don't exist in country or grouping list
  #' print warning message for each
  #' 
  
  if(!is.null(countryInput) & !is.null(groupingInput)){stop("Please select one input")}
  
  if(!is.null(countryInput)){
    
    df <- data.frame(col = countryInput)
    dfs <- sectors_country %>% distinct(., quota_origin) 
    n <- df %>% filter(col %notin% dfs$quota_origin)
    
  } else{
    
    df <- data.frame(col = groupingInput)
    dfs <- sectors_country %>% distinct(., grouping) 
    n <- df %>% filter(col %notin% dfs$grouping)
  }
  
  for(i in seq_len(nrow(n))){warning("Name not valid for: ",n[i,1])}
  
}

trqSectorCountryPlot <- function(chartType,
                                 .region,
                                 quotaUnit = NULL,
                                 countrySelect = NULL,
                                 countryRemove = NULL,
                                 groupSelect = NULL,
                                 groupRemove = NULL,
                                 sectorSelect = NULL,
                                 sectorRemove = NULL){
  
  
  # Stop function if no sector selected as facet plot is unreadable. 
  
  if(is.null(sectorSelect) & is.null(sectorRemove)){
    rlang::abort(c("Please select specific TRQ sectors for facet plot using sectorSelect or sectorRemove"))}
  
  #' error handling for 
  #' correct region and @region
  #' quota unit inputs  @quotaUnit
  #' 
  
  trqPlotErrorHandling(region_arg = .region, quota_unit_arg = quotaUnit)
  
  #' chart type 
  #' and selection filters
  #' filter for country or grouping inputs
  #' 
  
  cs <- c(countrySelect)
  cr <- c(countryRemove) # create arrays
  gs <- c(groupSelect)
  gr <- c(groupRemove)
  
  if(chartType == "country"){  
    
    # abort function if no country input selected:
    
    if(!is.null(countrySelect) & !is.null(countryRemove)){
      
      message("Please select single country input")
      
    } else{
      
      # QA check for country:
      if(is.null(countryRemove) & is.null(countrySelect)){
        message("No country input selected - are you sure?")
      }else if(is.null(countryRemove)){
        countryCheck(countryInput = countrySelect)
      }else{countryCheck(countryInput = countrySelect)}
      
      # filter data based on user input:
      df <- sectors_country %>% filter(region == .region) %>%
        {if(!is.null(countrySelect))  filter(.,quota_origin %in% cs)
          else if(!is.null(countryRemove)) filter(.,quota_origin %notin% cr)
          else .}
      
    }
    
    
  } else if(chartType == "grouping"){
    
    if(!is.null(groupSelect) & !is.null(groupRemove)){
      
      message("Please select single grouping input")
      
    } else{
      
      # QA check for grouping:
      if(is.null(groupRemove) & is.null(groupSelect)){
        message("No grouping input selected - are you sure?")
      } else if(is.null(groupRemove)){
        countryCheck(groupingInput = groupSelect) # check grouping inputs are valid. 
      } else{countryCheck(groupingInput = groupRemove)} # if not output warning message
      
      
      # filter data based on user input
      df <- sectors_grouping %>% filter(region == .region) %>%
        {if(!is.null(groupSelect))  filter(.,grouping %in% gs)
          else if(!is.null(groupRemove)) filter(.,grouping %notin% gr)
          else .}
      
    }
    
  } else{stop("Please select valid chartType input: 'country' or 'grouping' ")}
  
  
  df <- df %>% filter(source == "UK") # filter uk data
  
  # quota unit filter:
  
  if(is.null(quotaUnit)){ 
    
    quotaFilt <- "Tonnes" # for chart label
    
    df <- df %>% 
      mutate(across(total_quota_volume:total_quota_usage, ~ ./1000)) %>%    # convert data to Tonnes
      filter(quota_unit_final == "Kilograms")
    
  } else {
    
    quotaFilt <- quotaUnit
    df <- df %>% filter(quota_unit_final == quotaFilt)
  }
  
  # sector selection filters --
  
  sect <- c(sectorSelect)
  sectrm <- c(sectorRemove)
  
  # sector QA check:
  
  if(!is.null(sectorSelect)){sectorCheck(sect)}
  else if(!is.null(sectorRemove)){sectorCheck(sectrm)}
  
  #  logic for one select sector input:
  if(!is.null(sectorSelect) & !is.null(sectorRemove)){
    
    message("Please select single sector input")
    
  } else{
    
    df <- df %>% 
      {if(!is.null(sectorSelect))filter(., trq_dsc %in% sectorSelect)
        else if(!is.null(sectorRemove)) filter(.,trq_dsc %notin% sectorRemove)
        else .}
    
  }
  
  # create factor levels for plot: i.e. plot largest values at bottom of chart
  
  myLevels <- df %>% 
    group_by(trq_dsc) %>% 
    summarise(value = sum(total_quota_volume)) %>%
    arrange(-value)
  
  df$trq_dsc <- factor(df$trq_dsc , levels=myLevels$trq_dsc)
  
  
  t <- 
    ggplot()+
    geom_bar(
      data = df, 
      aes(
        x = grouping,
        y = total_quota_volume, 
        fill = "blue"
      ),
      stat = "identity")+
    geom_bar(
      data = df, 
      aes(
        x = grouping, 
        y = total_quota_usage, 
        fill = "red"
      ),
      stat = "identity"
    ) +
    geom_text(
      data = df,
      aes(
        x = grouping, 
        y = total_quota_volume
      ),
      label = paste0(round((df$total_allocation_fill_rate*100),1),"%"), # fill rate % as text
      size = 3
    )+
    scale_y_continuous(
      quotaFilt, 
      labels = scales::comma,
      n.breaks=6
    )+
    scale_x_discrete(name="")+
    scale_fill_manual(
      values=c("#3c8dbc","#cf102d"),
      labels = c("Quota volume", "Quota usage")
    )+
    coord_flip()+
    theme(
      panel.background = element_blank(),
      panel.grid.major.x = element_line(colour="lightgrey", size=0.5),
      axis.text.y  = element_text(size = 11),
      legend.title=element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 9,angle = 60, vjust = 0.5, hjust=1)
    )+
    facet_grid(. ~ trq_dsc, scales = "free_x")+
    theme(
      strip.text.x = element_text(size=11),
      strip.background = element_rect(colour="cornflowerblue", fill="aliceblue")
    )
  
  
  t
  
}

### 4ii. test function --------------------------------------------------

#' test the trqSectorCountryPlot function with how it runs
#' and various error messaging when
#' incorrect inputs are inserted. 
#' 

# load example data for function

load("..\\data\\function_example_data.Rdata")


trqSectorCountryPlot(chartType = "country",
                     .region = "Africa",
                     countrySelect = c("Morocco","Egypt","Tunisia"),
                     sectorSelect = c("Fruits and vegetables","Cereals")
                      )

# compare outputs and message in console
trqSectorCountryPlot(chartType = "country",
                     .region = "Africa",
                     #countrySelect = c("Morocco","Egypt","Tunisia"),
                     sectorSelect = c("Fruits and vegetables","Cereals")
)


trqSectorCountryPlot(chartType = "count", # country spelt wrong
                     .region = "Africa",
                     countrySelect = c("Morocco","Egypt","Tunisia"),
                     sectorSelect = c("Fruits and vegetables","Cereals")
)


trqSectorCountryPlot(chartType = "country", 
                     .region = "Africa",
                     countrySelect = c("Morocco","Eypt","Tuniia"), # Egypt and Tunisia spelt wrong
                     sectorSelect = c("Fruits and vegetables","Cereals")
)


# example with abort error output message:
trqSectorCountryPlot(chartType = "country",
                     .region = "Afria", # Africa spelt wrong
                     countrySelect = c("Morocco","Egypt","Tunisia"),
                     sectorSelect = c("Fruits and vegetables","Cereals")
)


trqSectorCountryPlot(chartType = "country",
                     .region = "Africa", 
                     countrySelect = c("Morocco","Egypt","Tunisia"),
                     sectorSelect = c("Fruts and vegetables","Cereals","Sugar","Base metals") # mis-spelled sectors. 
)

# end. 