# 08 ggplot2 script. 

#' The following script will go through how to create basic and general plots using ggplot2. 
#' How to approach plotting by considering the data
#' what exactly you want to plot
#' plot formatting
#' and various plot types to make powerful charts for analysis, presentations and dashboards
#' 
#' this script will be utilising ONS trade data. 
#' Trade data time series are a perfect dataset to demonstrate the effectiveness of plotting data
#' 

' Very helpful resources:'
# https://ggplot2-book.org/index.html
# https://r-graph-gallery.com/



# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)
library(ggplot2)
library(plotly) # plotly library for different plocking package

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function


load("data/Trade_datasets.RData")



# 0. data set up -----------------------------------


#' It's very important to analyse your data
#' and envision your plot before writing any code. 
#' I have created plots which run into issues
#' which would not have occurred if I had correctly set my data up
#' to best be in a format for that specific plot. 
#' This is especially important if you want to plot grouped data. 
#' 
#' *Things to consider:*
#' what plot do you want? 
#' E.g. Bar plot? Stacked bar plot?
#' Is it grouped data?
#' Is the data long or wide format
#' and which formats are best for the plot you want?
#' 


df1 <- trade_annual %>% filter(iso2 %in% c("W1")) # filter for world trade
country_filt <- c("US","DE","IE","FR","JP")
df2 <- trade_annual %>% filter(iso2 %in% country_filt)


# 1. basic plots ---------------

#' basic plots using the plotly library
#' for simple trade data time series
#' and grouped data

## line plot -----------------

plot <-
  plot_ly(
    data = df1, 
    x = ~year, 
    y = ~total,
    color = I("darkred"),
    size = I(4),
    type = "scatter", 
    #linetype = I("dot"), # options : solid, dot, dash, longdash, dashdotr
    #mode = "markers" ,# mode  = markers creates scatter plot
    mode = "lines" # lines creates line plot
  ) %>% # this input create line plot
  layout(
    title = "Line Plot of Trade ", 
    xaxis = list(title = "Year"), 
    yaxis = list(title = "Total Trade £mn.")
    )
  


## scatter plot -------


plot <-
  plot_ly(
    data = df1, 
    x = ~year, 
    y = ~total,
    color = I("darkblue"),
    fill = I("white"),
    type = "scatter", 
    mode = "markers",
    symbol = I("square") # symbol options: circle, square, diamond, cross, triangle-up, triangle-left, triangle-right, triangle-down
  ) %>% # this input create line plot
  layout(
    title = "Line Plot of Trade ", 
    xaxis = list(title = "Year"), 
    yaxis = list(title = "Total TRade £mn.")
  )


# scatter plot with different fill colour using marker


plot_ly(
  data = df1,
  x = ~year,
  y = ~total,
  type = "scatter",
  mode = "markers",
  symbol=  I("diamond"),
  marker = list(color = "orange", line = list(color = "black", width = 1.5))
)

# scatter plot formatting and add line using line argument

plot_ly(
  data = df1,
  x = ~year,
  y = ~total,
  type = "scatter",
  mode = "markers",
  symbol=  I("diamond"),
  marker = list(color = "orange", line = list(color = "black", width = 1.5)),
  line = list(color = "darkblue", linetype = "dot")
)

## bar chart -----------------

  plot_ly(
    data = df1, 
    x = ~year, 
    y = ~export, 
    type = "bar",
    marker = list(color = "orange", line = list(color = "black", width = 1)),
    text = ~export, # adds text values ontop of bars
    textposition = "outside",
    opacity = 0.7 # like alpha - make bars more faint
    ) %>%
    layout(
      title = "Bar Plot of Exports", 
      xaxis = list(title = "X"), 
      yaxis = list(title = "Y"),
      font = list(size = 15) # increases axis / title font size
    )


# 2. grouped plots -------------


## line plot -----------



## bar chart -----------