# 09 dygraphs - interactive time series charts. 

# set up ---------------------------------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)
library(lubridate) # best package to handle dates working in R. 
library(ggplot2)
library(dygraphs) # dygraph package for interactive time series plots
library(xts) # time-series extention https://www.datacamp.com/cheat-sheet/xts-cheat-sheet-time-series-in-r

# https://r-graph-gallery.com/317-time-series-with-the-dygraphs-library.html
# https://rstudio.github.io/dygraphs/
# https://search.r-project.org/CRAN/refmans/dygraphs/html/dyOptions.html


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`)

#' The dygraphs package is an R interface to the dygraphs JavaScript charting library. 
#' It provides rich facilities for charting time-series data in R


#' dygraphs provide a creative and impact full way to plot
#'  interactive time-series within shiny app
#' and markdown documents. 

#' dygraphs work with dates. The difficult part is ensuring the 
#' date field is converted correctly. Then plotting is simple. 
#' You can amend dygraphs limitlessly if you're able to embed custom JaveScript code. 
#' even without knowing JS - you can create unique colorful time series. 
#' I know the basics of creating dyGraphs - nothing complicated
#' and I see how dygraphs enhance apps and documents I create 
#' So I highly recommend to ultilise this in your own work. 
#' 

# Example date is daily, monthly and quarterly. 

# upload data 
load("data/Trade_datasets.RData")
monthly <- read_excel("data/scores_data.xlsx", sheet = "monthly", col_types = "text") %>% clean_names()
qrt <- read_excel("data/scores_data.xlsx", sheet = "quarterly", col_types = "text") %>% clean_names()

# data converted to text so to demonstrate data conversions. 

# convert to dates --------------------------------

#' x-axis time data needs to be in a date format. 
#' Ensure you check your data before hand
#' If the date/time column isn't in a date format 
#' make sure you convert to this data type. 
# lubridate is a useful package plus base R functions
# stack overflow will have answers for any data-date conversions you need. 
#' https://www.r-bloggers.com/2013/08/date-formats-in-r/
#' https://r4ds.had.co.nz/dates-and-times.html
#' 


# convert character data, monthly-year, quarter:

  
monthly$month_year <- as.POSIXct(as.Date(monthly$month_year))

monthly$month_year <- 
  excel_numeric_to_date( # janitor package function to convert excel date serial number to date. 
    as.numeric(as.character(monthly$month_year), 
               date_system = "modern")
  )

# convert quarter date (yy q) to date          
qrt$yr_qrt <- as.Date(as.yearqtr(qrt$yr_qrt))

month_data <- xts(x = monthly$score, order.by = monthly$month_year)
daily_data <- xts(x = data_daily$score, order.by = data_monthly$month_year)
qrt_data <- xts(x = qrt$value, order.by = qrt$yr_qrt)



# 1. basic dygraph plot ---------------------------------------

#' the dygrpah function will plot an interactive time-series plot
#' assuming the data inputted is in the correct format 
#' (i.e x = data values, order.by = date within the xts function)
#' 


dygraph(data) # plot chart

# this is the default simplest plot. 

# you can create objects using dygraph and add piping. %>%. 


dy <- dygraph(data) %>%
  dyAxis("x", label = "Year-date") %>%
  dyAxis("y", label = "Score-value")


# dygraph arguments


dy <- 
  dygraph(
    data = data,
    main = "Time-series using dyGraph",
    xlab = "Year-axis-title",
    ylab = "Score-value"
  ) %>%
  dyAxis("y", valueRange = c(3, 10)) # add custom y-axis range
 

dy



## 1i. dy options --------------------------------------------------

# there are numerous add-ins for dygraphs. 
# using dyOptions there are several inputs
# you can use to aestehtically chnage your plot. 
# https://search.r-project.org/CRAN/refmans/dygraphs/html/dyOptions.html

# I will go through some code snippets
# add to the dygrpah object each time to highlight them separately. 

# basic area plot with no grid with green default colour. 
dy <- 
  dygraph(
    data = qrt_data,
    main = "Time-series using dyGraph",
    xlab = "Year-axis-title",
    ylab = "Score-value"
  ) %>%
  dyOptions(
    axisLineWidth = 1.5, # axis length thickness
    axisLineColor = "green", # axis colour
    axisLabelFontSize = 15, # font size of axis values
    drawGrid = FALSE, # remove grid lines
    fillGraph = TRUE, # fill under chart area
    fillAlpha = 0.4, # change opacity of filled chart. default is 0.15
    drawPoints = TRUE, # add scatter plot points
    pointSize = 3,
    pointShape = "diamond", # options: "dot", "triangle", "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"
    strokeWidth = 1.5 # line connecting points width
  ) 

# note you can't use dyOptions then pipe %>% later to another dyOptions and keep adding. 
# all options need to be inserted into one dyOptions. 


# basic scatter plot with gird with custom colour. 

dy <- 
  dygraph(
    data = qrt_data,
    main = "Time-series using dyGraph",
    xlab = "Year-axis-title",
    ylab = "Score-value"
  ) %>%
  dyOptions(
    #colors = "blue",
    colors = "#cf102d", # you can use custom colours
    drawAxesAtZero = TRUE,
    axisLineWidth = 2, # axis length thickness
    axisLabelFontSize = 15, # font size of axis values
    gridLineWidth = 1.5, # grids line width
    gridLineColor = "darkblue",
    drawPoints = TRUE, # add scatter plot points
    pointSize = 3,
    pointShape = "pentagon", # options: "dot", "triangle", "square", "diamond", "pentagon", "hexagon", "circle", "star", "plus", "ex"
    strokeWidth = 2, # line connecting points width
    strokePattern = "dashed" # "dashed", "dotted", "solid"
  ) 





## 1ii. other plots --------------------------------------------

# you can define other plot styles within dy options
# such as bar plots, stacked area plots, step/stem plot. 

# note stacked area plot example in multiple dygraph plot 

dygraph(qrt_data, main = "Area plot example") %>%
  dyOptions(fillGraph = TRUE)
  
dygraph(qrt_data, main = "Step plot example") %>%
  dyOptions(stepPlot = TRUE)

dygraph(qrt_data, main = "Step plot example") %>%
  dyOptions(stemPlot = TRUE)


## 1iii. add range ----------------------------------------------

# you can add a range selector for dygraphs
# a simple easy way to change the x axis range. 
# and limit x axis scales. 


dy %>% dyRangeSelector() # add full range
dy %>% dyRangeSelector(dateWindow = c("2014-01-01", "2022-01-01")) # set limit

dy %>% 
  dyRangeSelector(
   dateWindow = c("2013-01-01", "2022-01-01"),
   height = 20, # change height
   strokeColor = "red", # ammend line colour
   fillColor = "#cf102d" # range selector fill colour. # can chose "red","blue" etc. 
   )



## 1.1. legend and axis -------------------------------------------------
### axis ----------------------
# you can amend the axis outside of the main dygraph function
# using dyAxis. 

dy <- 
  dygraph(month_data, main = "Chart Title for time-series.") %>%
  dyAxis(
    "x", 
    label = "Time-series x axis", 
    drawGrid = FALSE) %>% # remove x axis grid lines. 
  dyAxis(
    "y",
    label = "Y-axis label",
    valueRange = c(3,10) # custom default y axis range. 
  ) %>%
  dyOptions(
    axisLineWidth = 1.5,
    colors = "darkgrey",
    strokeWidth = 2
  )

# further example. 
dy <- 
  dygraph(qrt_data, main = "Chart Title for time-series.", ylab = "Y axis title") %>%
  dyAxis(
    "x", 
    label = "Time-series x axis", 
    drawGrid = FALSE) %>% # remove x axis grid lines.
  dyOptions(
    includeZero = TRUE, # set 0 as start of y-axis. 
    colors = "darkblue",
    axisLineColor = "navy", 
    gridLineColor = "lightblue"
    )


### legend -----------------------
# legend label. 

# amend legend using dyLegend
dy %>% 
  dyLegend(show = "always") # "auto", "always", "onmouseover", "follow", "never" auto is default. 
    
dy %>% dyLegend(show = "onmouseover") # default when only one plot plotted
dy %>% dyLegend(show = "follow") 
dy %>% dyLegend(show = "never") 

dy %>%
  dyLegend(
    show = "always", # always display legend
    width = 100, # chnage width od div legend is inserted within. 
    showZeroValues = TRUE, # show zero values in legend
    labelsSeparateLines = TRUE # displays values on different line within legend div
  ) %>%
  dySeries("V1", label = "Value label") # manually change variable label. Ideally labels are corrected in xts data before dygraph. 



### double y-axis ------------------------------------------------
  
# you can easily set a double axis if you want to plot
# two time series together at varying sizes. 
  
# set up two seperate xts objects. 
# i.e data input has two values
# example: plotting import/exports on a double axis is useful to compare their trend. 


df <- trade_annual %>%
  filter(iso2 == "US")

# convert year to date

df$year <- as.Date(zoo::as.yearmon(df$year)) 

# individual data series
imports <- xts(x = df$import, order.by = df$year)
exports <- xts(x =df$export, order.by = df$year)
  
dt = cbind(imports,exports)

dygraph(dt) %>% # plots the two value columns as default
  dySeries("exports", axis = 'y2') 

# further example with formatting:

dygraph(dt, main = "Import-export plot") %>%
  dySeries("exports", axis = 'y2') %>%
  dyOptions(
    axisLineWidth = 1.5,
    drawGrid = FALSE,
    strokeWidth = 2
  ) %>%
  dyAxis(
    "y", 
    axisLabelWidth = 70, # adds space between label and values
    label = "Imports",  # example to use comma separator in y axis. 
    
    #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  ) %>%
  dyAxis(
    "y2", 
    axisLabelWidth = 100, # adds space between label and values
    label = "Exports",  # example to use comma separator in y axis. 
    
    #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  )



## 1.2 plot additions  -----------------------------------------------

### cross hair -------------------------------------------------

#' wit the cross-hair function you can add interactive lines which
#' highlight the point selected on the plot. 
#' 

#' function dsc: 
#' The dyCrosshair plugin draws a crosshair line over the point closest to the mouse when the user hovers over the graph. It has a "direction" option which is provided in the R wrapper function and then forwarded to the plugin using the "options" argument to dyPlugin.



dygraph(qrt_data) %>% dyCrosshair() # default direction =  "both"
dygraph(qrt_data) %>% dyCrosshair(direction = "horizontal")
dygraph(qrt_data) %>% dyCrosshair(direction = "vertical")

# I quite like the vertical line given the x axis automatic tick frequency can be scarce. 
# I feel this option adds to the drgrpah plot. 

### shade area ------------------------------------------------

#' you can add shading ontop of your graph to highlight specific periods. 
#' simply use dyshading and specific the time intervals you want to shade.
#' you can add multiple to a single plot. 

# x axis is default
dygraph(qrt_data) %>% 
  dyShading(from = "2012-01-01", to = "2014-01-01", color = "lightblue")


dygraph(qrt_data) %>% 
  dyShading(from = "2009-10-01", to = "2013-07-01", color = "lightblue") %>%
  dyShading(from = "2015-01-01", to = "2016-10-01", color = "#cf102d") %>%
  dyShading(from = "2019-01-01", to = "2020-04-01", color = "grey")


# y axis:

dygraph(qrt_data) %>% 
  dyShading(axis = "y",from = 7, to = 9, color = "lightblue") %>%
  dyShading(axis = "y", from = 4, to =5, color = "red")


# combined

dygraph(qrt_data) %>% 
  dyShading(axis = "y",from = 7, to = 9, color = "lightblue") %>%
  dyShading(from = "2012-01-01", to = "2014-01-01", color = "lightblue") %>%
  dyShading(from = "2015-01-01", to = "2016-10-01", color = "#cf102d") 



### event lines ------------------

# you can add custom event lines onto your dyPlot using dyEvent. 
# using dyEvent (vertical) or dyLimit (horizontal)


dygraph(qrt_data) %>%
  dyEvent(
    "2012-01-01", 
    label = "event line example", 
    labelLoc = "top",
    color = "blue",
    strokePattern = "solid" # dashed is default. Options: "dotted", "dashed", "dotdash", or "solid")
   )

dy <- 
 dygraph(qrt_data) %>%
   dyEvent(
     "2012-01-01", 
     label = "event line example", 
     labelLoc = "bottom", # default is top
     color = "blue",
     strokePattern = "solid" # dashed is default. Options: "dotted", "dashed", "dotdash", or "solid")
   ) %>%
   dyEvent("2014-01-01", label = "event line example2", color = "lightblue") %>%
   dyEvent("2019-01-01", label = "event line example3", color = "#cf102d")
  


# dyLimit:

dygraph(qrt_data) %>%
  dyLimit(
    limit = 5,
    label = "limit line example",
    labelLoc = "left",
    color = "blue",
    strokePattern = "dotted"
  )


dygraph(qrt_data) %>%
  dyLimit(5,"limit line example",labelLoc = "left",color = "blue") %>%
  dyLimit(7,"limit line example2",labelLoc = "left",color = "#cf102d")
  

# you can insert manually computed values.
# for example an average:

meanValue = mean(as.numeric(qrt$value))

dygraph(qrt_data) %>%
  dyLimit(meanValue,"limit line example",labelLoc = "left",color = "blue") %>%
  dyLimit(mean(as.numeric(qrt$value2)),"limit line example",labelLoc = "left")


# combined chart:

dygraph(qrt_data) %>%
  dyEvent("2012-01-01",label="event line",labelLoc = "bottom", color = "blue") %>%
  dyEvent("2014-01-01",label="event line2",labelLoc = "bottom", color = "red") %>%
  dyShading(from = "2012-01-01", to = "2014-01-01", color = "lightblue") %>%
  dyLimit(meanValue,"limit line example",labelLoc = "left",color = "blue")



# 2. multiple dygraph plots -----------------------------------------

# you can easily plot multiple time-series together on one chart. 
# this can come from the same dataset or a separate one. 
# as long as the data inputs are a time-series object
# dygraph is able to plot them together. 
# you can then ammend your chart depending on the data input
# add bars, lines, fill etc and cusotm labelling. 

# load in trade datasets 

df <- trade_annual %>%
  filter(iso2 == "DE")

# convert year to date

df$year <- as.Date(zoo::as.yearmon(df$year)) 

data <- xts(x = df$total, order.by = df$year)


# simple plot with one value. 
dygraph(data)

# combine values into one xts object using cbind. 

data <- xts(x = cbind(df$total,df$import,df$export), order.by = df$year)

dygraph(data)

# expand plot and amend labels creating line-plot. 



dy <-
    dygraph(
      data,
      main = "Country trade figures plot") %>%
    dyAxis("x", label = "Year", drawGrid = FALSE) %>%
    dyAxis(
      "y", 
      axisLabelWidth = 70, # adds space between label and values
      label = "Trade values",  # example to use comma seperator in y axis. 
      
      #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
      valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
      axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
     ) %>%
    dyOptions(
      colors = list("red","blue","purple"), # you can insert RColorBrewer pre-set palletes. 
      #strokeWidth = 1.5,
      axisLineWidth = 1.5,
      gridLineColor = "grey",
      gridLineWidth = 0.7
    )


#### custom series ----------
#####  by series -----------

# dySeries()
# you can create custom Dy plots using dySeries outside of plotting within dygraph. 
# you can rename labels using dySeries or further customise you plot
# for example multiple line plot you can make specific lines dashed/solid using dySeries. 


dygraph(data) %>% # basic plot
  dySeries("V1", strokeWidth = 2, color = "red") %>%
  dySeries("V2", strokePattern = "dashed", color = "blue", stepPlot = TRUE) %>%
  dySeries("V3", fillGraph = TRUE)

# alternative dySeries can be used to simple change label titles. 
dy <-
  dygraph(data) %>% 
  dyRangeSelector() %>%
  dyLegend(
    show = "onmouseover",
    width = 350
  ) %>%
  dySeries("V1", label = "Total") %>%
  dySeries("V2", label = "Import") %>%
  dySeries("V3", label = "Export")



#####  by group  --------------------------------------

# dyGroup()

#' you can make custom series plots by combining series using dyGroup. 

dygraph(data) %>%
  dyGroup(c("V2","V3"), stemPlot = TRUE, color = c("red","blue"))

dygraph(data) %>%
  dyGroup(
    c("V2","V3"), 
    fillGraph = TRUE, 
    drawPoints = TRUE, 
    pointSize = 3, 
    color = c("red","blue")
  ) %>%
  dyOptions(
    drawPoints = TRUE, 
    pointSize = 6) # amend the ungrouped series. 


### 2i. highlight series -------------------------------

# you can highlight the specific series you click on. 
# emphising the interactivity of dyGraphs. 


dy %>% 
  dyHighlight(
    highlightCircleSize = 4, # size of circle used to highlight points on chart
    highlightSeriesBackgroundAlpha = 0.1,
    hideOnMouseOut = FALSE, #when mouse moves from chart - the highlighted series remains. Defualt is TRUE
    highlightSeriesOpts = list(strokeWidth = 3) # if strokeWidth is in dyOptions - this won't run
  )

# alt:
dy %>% 
  dyHighlight(
    highlightCircleSize = 4, # size of circle used to highlight points on chart
    highlightSeriesBackgroundAlpha = 0.1,
    hideOnMouseOut = TRUE, #when mouse moves from chart - the highlighted series remains. Defualt is TRUE
    highlightSeriesOpts = list(strokeWidth = 3) # if strokeWidth is in dyOptions - this won't run
  )


## 2ii. stacked area plot -------------------------------------------------

df <- trade_annual %>% filter(iso2 == "W1")
# convert year to date
df$year <- as.Date(zoo::as.yearmon(df$year)) 
# combine values into one xts object using cbind. 
data <- xts(x = cbind(df$import,df$export), order.by = df$year)

# for the stack chart you need to call the options:
# fillGraph to fill the area underneath each plot
# and if you want to stack the plots ontop of each other
# use stackedGraph

dy <-
  dygraph(
    data,
    main = "Country trade figures plot") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis(
    "y", 
    axisLabelWidth = 80, # adds space between label and values
    label = "Trade values",  # example to use comma seperator in y axis. 
    
    #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  ) %>%
  dyOptions(
    fillGraph = TRUE, # fill areas under each plot. 
    stackedGraph = TRUE, # stack on top # compare plot when commenting this line out. 
    fillAlpha = 0.4,
    colors = RColorBrewer::brewer.pal(3, "Set2"), # you can insert RColorBrewer pre-set palletes. 
    axisLineWidth = 1.5,
    gridLineColor = "grey",
    gridLineWidth = 0.7,
    strokeWidth = 2,
    drawPoint = TRUE,
    pointSize = 2
  ) %>%
  dySeries("V1", label = "Imports") %>%
  dySeries("V2", label = "Exports") %>%
  dyLegend(width = 300)




## 2iii. different plot styles ---------------------------------------------

# plot different plot styles on the same chart for different series. 
# 

df <- trade_annual %>% filter(iso2 == "W1")
# convert year to date
df$year <- as.Date(zoo::as.yearmon(df$year)) 
# combine values into one xts object using cbind. 
data <- xts(x = cbind(df$import,df$export,df$total), order.by = df$year)


# you can define each series plot within dySeries. 

dygraph(data, main = "Multi-plot example") %>%
  dyOptions(
    colors = RColorBrewer::brewer.pal(3, "Set1"), # you can insert RColorBrewer pre-set palletes. 
    axisLineWidth = 1.5,
    gridLineColor = "grey",
    gridLineWidth = 0.7,
    strokeWidth = 2,
    drawPoint = TRUE,
    pointSize = 2
  ) %>%
  dySeries("V1",label = "Imports",stepPlot = TRUE) %>%
  dySeries("V2",label = "Exports",fillGraph = TRUE) %>%
  dySeries("V3", label = "Total", stemPlot = TRUE)

# not the most elegant chart - but useful to demonstrate you can plot different styles. 

# multi plot - stem plot. 
dygraph(data, main = "Multi-plot example") %>%
  dyOptions(
    stemPlot = TRUE,
    colors = RColorBrewer::brewer.pal(3, "Set1"), # you can insert RColorBrewer pre-set palletes. 
    axisLineWidth = 1.5,
    gridLineColor = "grey",
    gridLineWidth = 0.7,
    strokeWidth = 2,
    drawPoint = TRUE,
    pointSize = 2
  )

# shaded stem area plot + line plot:


df <- trade_annual %>% filter(iso2 == "W1")
# convert year to date
df$year <- as.Date(zoo::as.yearmon(df$year)) 
# combine values into one xts object using cbind. 
data <- xts(x = cbind(df$import,df$export), order.by = df$year)

  
dygraph(data, main = "Multi-plot example") %>%
  dyOptions(
    colors = RColorBrewer::brewer.pal(3, "Set1"), # you can insert RColorBrewer pre-set palletes. 
    axisLineWidth = 1.5,
    gridLineColor = "grey",
    gridLineWidth = 0.7,
    strokeWidth = 2,
    drawPoint = TRUE,
    pointSize = 2
  )%>%
  dySeries("V1",label = "Imports",stepPlot = TRUE) %>%
  dySeries("V2",label = "Exports",stepPlot = TRUE, fillGraph = TRUE)


# 3. js plotter examples --------------------------------------

# https://rstudio.github.io/dygraphs/gallery-custom-plotters.html

#' within the dygraphs R library there are multiple
#' js scripts which can be called on to chart different plots. 
#' such as bar charts etc. 
#' these can be combined with default plots within a multi-value plot. 
#' you utilise the dyPlotter function. 
#' to simplify or reduce code complexity
#' these plotters can be called within functions. 
#'  

# NOTE: these functions are inbuilt. 
# defining them is a way to seperate out this script in the navigation pane. 

dyBarChart <- function(dygraph) {
  dyPlotter(
    dygraph = dygraph,
    name = "BarChart",
    path = "data/plotters/barchart.js"
  )
}


dygraph(data) %>% 
  dyBarChart()


dyStackedBarChart <- function(dygraph) {
  dyPlotter(
    dygraph = dygraph,
    name = "StackedBarChart",
    path = "data/plotters/stackedbarchart.js"
  )
}



dygraph(data) %>% 
  dyStackedBarChart() 



dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = "data/plotters/multicolumn.js"
  )
}


# multi column with plot a "dodge" bar chart, 
# i.e. bars are next to each other 
# in the time-series. 


dygraph(data) %>% 
  dyMultiColumn() %>%
  dyRangeSelector() %>%
  dyOptions(rightGap = 70)



##### 3.1 grouped plots ---------------------------------------------

# utilising dygraphs inbuilt JS plotted functions
# you can select speific groups to plot
# to further customise your charts. 

#' Example - can plot a stacked area plot + a line plot. 

df <- trade_annual %>% filter(iso2 == "W1")
# convert year to date
df$year <- as.Date(zoo::as.yearmon(df$year)) 
# combine values into one xts object using cbind. 
data <- xts(x = cbind(df$import,df$export,df$total), order.by = df$year)
nm <- c("Imports","Exports","Total")
names(data) <- nm

# you can't rename legend names using dySeries if you used a dyGroupedPlot
# therefore you need to rename your xts data labels before inserting into dygraph

dyStackedBarGroup <- function(){}

dygraph(data) %>% 
  dyStackedBarGroup(c('Imports','Exports')) # define variables for stacked bar plot. 
  

dygraph(data) %>%
  dyStackedBarGroup(c('Imports','Total')) %>%
  dySeries("Exports",drawPoints = TRUE, pointSize = 5, color = "red")



dyStackedRibbonGroup <- function(){}

# acheives simple and quick stacked area plot. 
dygraph(data) %>%
  dyStackedRibbonGroup(c('Imports','Exports','Total'))


dyMultiColumnGroup <- function(){}
# side-byside grouped bar plot. 
dygraph(data) %>%
  dySeries("Total",fillGraph= TRUE,drawPoints=TRUE,pointSize = 3) %>% # ensure series is plot first. 
  dyMultiColumnGroup(c('Imports','Exports'))
  

  
#### 3.2 Examples -----------------

##### Trade plot example --------------------------------------
# full example of formatted plot utilising js dygrpah functions

# total UK -world trade plot broken down by total, import and export values. 
# total will be a stepplot and import/export will be stacked bar plot. 

###### world trade -----------------------------

dygraph(
  data,
  main = "Total UK-World trade") %>%
  dyAxis(
    "y", 
    axisLabelWidth = 80, # adds space between label and values
    label = "Trade values",  # example to use comma seperator in y axis. 
    valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  ) %>%
  dyOptions(
    axisLineWidth = 1.5, # issue with axis line - js stacked bar plot overlaps. 
    drawGrid = FALSE,
    colors = RColorBrewer::brewer.pal(3, "Set2"), 
    strokeWidth = 2)%>%
  dyStackedBarGroup(c('Imports','Exports')) %>%
  dySeries(
    "Total",
    stepPlot = TRUE, 
    color = "red",
    fillGraph = TRUE,
    drawPoints = TRUE,
    pointSize = 2) %>%
  dyRangeSelector() %>%
  dyLegend(width = 300)
    
  
###### country event plot ---------------------------------

#' plot example highlighting two countries trade - with event points and shading. 
#' 

df <- trade_annual %>% filter(iso2 %in% c("KR","JP")) %>%
  select(-import,-export,-balance,-iso2) %>%
  pivot_wider(names_from = "country", values_from = "total") %>%
  clean_names()

# convert year to date
df$year <- as.Date(zoo::as.yearmon(df$year)) 
# combine values into one xts object using cbind. 
data <- xts(x = cbind(df$south_korea,df$japan), order.by = df$year)
nm <- c("South Korea","Japan")
names(data) <- nm

dygraph(data, main = "South Korea dashboard plot") %>%
  dyAxis(
    "y", 
    axisLabelWidth = 80, # adds space between label and values
    label = "£ million.",  # example to use comma seperator in y axis. 
    valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  ) %>%
  dyOptions(
    axisLineWidth = 1.5,
    drawGrid = FALSE,
    drawPoints = TRUE,
    pointSize = 4,
    strokeWidth = 2,
    titleHeight = 25
  ) %>%
  dySeries("South Korea", color = "#cf102d") %>%
  dySeries("Japan", color = "#4f0b7b") %>%
  dyEvent(
    "2011-01-01", 
    label ="EU FTA signed", 
    color =  "blue", 
    strokePattern = "dashed"
  ) %>%
  dyRangeSelector(
    dateWindow = c("2010-01-01", "2021-01-01"),
    height = 20, # change height
    strokeColor = "red"
  ) %>%
  dyLegend(width = 100, labelsSeparateLines = TRUE, show = "follow") %>% #, show = "onmouseover") %>%
  dyCrosshair(direction = "vertical")

  
  
##### Multi country plot ----------------------------------

# use dyGraph to plot multiple countries trade values together. 

# filter and turn into wide data format.
df <- trade_annual %>% filter(iso2 %in% c("SE","DE","FR","IE","NL")) %>%
  select(-import,-export,-balance,-iso2) %>%
  pivot_wider(names_from = "country", values_from = "total") %>%
  clean_names()

df$year <- as.Date(zoo::as.yearmon(df$year)) 

data <- xts(x = cbind(df$sweden,df$germany,df$france,df$ireland,df$netherlands),
            order.by = df$year)

nme <- c("Sweden","Germany","France","Ireland","Holland")

names(data) <- nme

###### i.  line plots ----------------------

dygraph(data, main = "Multi-country Plot.") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(
    axisLineWidth = 1.5,
    colors = RColorBrewer::brewer.pal(5, "Set2"),
    drawPoints = TRUE,
    pointSize = 3
  ) # if you want a stacked line/area plot - utilise stackedGraph and fillGraph dyOptions input. 
  


# stacked line plot - custom lines. 

dygraph(data, main = "Multi-country Plot.") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(
    axisLineWidth = 1.5,
    colors = RColorBrewer::brewer.pal(5, "Set2"),
    drawPoints = TRUE,
    pointSize = 3,
    fillGraph = TRUE,
    stackedGraph = TRUE
  ) %>%
  dySeries("Sweden",strokePattern = "dashed") %>% # "dotted", "dashed", "dotdash", or "solid")
  dySeries("France", strokePattern = "dotted", strokeWidth = 2, drawPoints = FALSE)
# note when changing the series after Options
# the names series is at the bottom of the stacked plot. 


dygraph(data, main = "Multi-country Plot.") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(
    axisLineWidth = 1.5,
    colors = RColorBrewer::brewer.pal(5, "Set2"),
    drawPoints = TRUE,
    pointSize = 3
  ) %>%
  dyStackedRibbonGroup(c("Sweden","Germany","France","Ireland","Holland")) 
# alternative you can simply use this function


###### ii. stacked bar -----------------------------

dygraph(data, main = "Multi-country total Trade plot.") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(
   # axisLineWidth = 1.5, # doens't work well with dy bar plots. 
    colors = RColorBrewer::brewer.pal(5, "Paired"),
    drawPoints = TRUE,
    pointSize = 3
  ) %>%
  dyStackedBarGroup(c("Sweden","Germany","France","Ireland","Holland")) %>%
  dyLegend(show = "follow")

# other plot examples:
# stacked fill area plot
dygraph(data) %>%
  dyStackedRibbonGroup(c("Sweden","Germany","France","Ireland","Holland"))

# side by side bar plot
dygraph(data) %>%
  dyMultiColumnGroup(c("Sweden","Germany","France","Ireland","Holland"))

# stacked line plot
dygraph(data) %>%
  dyStackedLineGroup(c("Sweden","Germany","France","Ireland","Holland"))


##### Proportion plot ----------------------------------------

# create a stacked plot of a the proportion of a value to a total group measure. 
# i.e. total proportion of a value each year compared to all other values. 
# trade: total proportion between imports vs services. EU trade bs Non EU trade. 


df <- trade_annual %>%
  filter(iso2 == "US") %>%
  group_by(year) %>%
  mutate(improp = import/sum(total))%>%
  mutate(exprop = 1-improp)


# convert year to date

df$year <- as.Date(zoo::as.yearmon(df$year)) 

# combine values into one xts object using cbind. 

data <- xts(x = cbind(df$improp,df$exprop), order.by = df$year)

dygraph(data) %>%
  dyOptions(
    drawGrid = FALSE,
    fillGraph = TRUE,
    stackedGraph = TRUE,
    colors = c("red","blue"),
    fillAlpha = 0.4,
    strokeWidth = 2,
    includeZero = TRUE # starts y axis at 0. 
  ) %>%
  dyAxis("y", # code from https://gist.github.com/Arkoniak/8191386f2e36309f694511dfd5bf0808
         valueFormatter = "function(v){return (v*100).toFixed(1) + '%'}",
         axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}") %>%
  dyCrosshair(direction = "vertical")
         
  
# End. 