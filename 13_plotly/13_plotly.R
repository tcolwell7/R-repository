# 08 ggplot2 script. 

#' The following script will go through how to create basic and general plots using plotly
#' Plotly is a good alternative to ggplot to know and apply
#' I am exploring plotly for use in R shiny dashboards
#' so I don't have ot use ggplotly - and convert ggplot to 
#' interactive plotly visuals in apps
#' which increases run time and lag
#' And to learn a new library and skill 
#' 
#' this script will be utilising ONS trade data. 
#' Trade data time series are a perfect dataset to demonstrate the effectiveness of plotting data
#' 


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
country_filt <- c("CA","DE","IE","FR","JP")
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
  

## area plot -----------

# simple area plot, ensure mode is none and fill is specified

plot_ly(
  data = df1, 
  x = ~year, 
  y = ~total,
  color = I("lightblue"),
  type = "scatter",
  mode = "none",
  fill = "tozeroy" # options: tozeroy, tozerox, toself, tonexty,tonextx 
) %>% 
  layout(
    title = "Area Plot of Trade ", 
    xaxis = list(title = "Year"), 
    yaxis = list(title = "Total Trade £mn.")
  )


# interesting plot when trade has decreased below trend line
plot_ly(
  data = df1, 
  x = ~year, 
  y = ~total,
  color = I("lightblue"),
  type = "scatter",
  mode = "none",
  fill = "toself"
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
    marker = list(color = "orange", line = list(color = "black", width = 1)), # color bar chart
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

# add the grouping from the df within the group and  color/fill argument
# line plot won't work with just group argument
# works with just colour argument


plot_ly(
  data = df2, 
  x = ~year, 
  y = ~total, 
  type = "scatter", 
  mode = "lines+markers", 
  #group = ~country,
  color = ~country,
  marker = list(color = "white", symbol = "triangle-down",
                line = list(color = "black", width = 1.5))
)

grouped_plot <- 
  plot_ly(
   data = df2, 
   x = ~year, 
   y = ~export, 
   type = "scatter", 
   mode = "lines", 
   color = ~country,
   line = list(width = 3)
  )


## area plot -----------

# stacked area plot examples using grouped data

# stack areas on top of each other
plot_ly(
  df2,
  x = ~year, 
  y = ~total, 
  type = "scatter", 
  mode = "none",
  color = ~country,
  stackgroup = "one"
  ) 


# fill tozeroy, doesn't stack but creates area plot
# where you can see other areas inside the largest ones

plot_ly(
  df2,
  x = ~year, 
  y = ~total, 
  type = "scatter", 
  mode = "none",
  color = ~country,
  fill = "tozeroy",
  #fill = "toself", # toggle between different fill inputs. This acts like a filled area line plot
  alpha = 0.8
) 


# this is better than tozeroy as you can see each group
plot_ly(
  df2,
  x = ~year, 
  y = ~total, 
  type = "scatter", 
  mode = "none",
  color = ~country,
  fill = "tonexty",
  alpha = 0.5
) 


## scatter plot --------


plot_ly(
  data = df2, 
  x = ~import, 
  y = ~export, 
  type = "scatter", 
  mode = "markers", 
  color = ~country,
  marker = 
    list(
      symbol = "square", 
      line = list(color="black",width = 1)
       )
   )



## bar chart -----------

# simple grouped bar chart, toggle between stacked and side by side
# removing the layout function

plot_ly(
  data = df2, 
  x = ~year, 
  y = ~total,
  type = "bar",
  color = ~country,
  name = ~country,
  legendgroup = ~country
  ) %>% # add laytout, barmode = "stack" for stacked barchart 
  layout(barmode = "stack") 


plot_ly(
   data = df2, 
   x = ~year, 
   y = ~total,
   type = "bar",
   color = ~country,
   name = ~country,
   legendgroup = ~country
  ) %>% # over lay lays bars on top of each other, here
   layout(barmode = "overlay")


# 3. basic formatting ----------

# basic formatting using plotly. 

## titles and axis --------------

plot_ly(
   data = df1, 
   x = ~year, 
   y = ~total,
   color = I("darkred"),
   size = I(4),
   type = "scatter", 
   mode = "lines" # lines creates line plot
 ) %>%
layout(
  title = "My Chart Title",
  titlefont = list(
    family = "Arial",
    size = 20,
    color = "#7f7f7f"
  ), # changes axis fonts
  font = list(family = "Courier New, monospace"),
  titlefont = list(
    family = "Courier New, monospace",
    size = 14,
    color = "darkred"
    ),# changed axis labels
  xaxis = list(title = "Year"), 
  yaxis = list(title = "Total Trade £mn.")
  )



  
## legend formatting ------------

### position ---------

grouped_plot <- 
  plot_ly(
    data = df2, 
    x = ~year, 
    y = ~export, 
    type = "scatter", 
    mode = "lines", 
    color = ~country,
    line = list(width = 3)
  ) 

# legend position

# you can toggle / change position based on x/y coordinates
# default is 1,1
grouped_plot %>% layout(legend = list(x = 0, y = 1)) # top left
grouped_plot %>% layout(legend = list(x = 0.5, y = -0.5)) # 
grouped_plot %>% layout(legend = list(x = 1, y = 0.5)) # 

# flip legend items to horizontal position using orientation:
grouped_plot %>% layout(legend = list(x = 0.2, y = -0.2, orientation = "h"))

# legend at top, positioned horizontally 
grouped_plot %>% layout(legend = list(x = 0.2, y = 1.1, orientation = "h"))


# title 
grouped_plot %>% layout(legend = list(title = list(text = "Country")))


# combined
grouped_plot %>% layout(
  legend = 
    list(
      x = 0.2, 
      y = 1.1, 
      orientation = "h",
      title = list(text = "Country Title")
      )
  )
    


### create box --------

grouped_plot %>% 
  layout(
      legend = list(
       bgcolor = "grey", # background color
       borderwidth = 2,
       bordercolor = "black"
       )
     )
   

## h/v lines ---------



## grids ---------------------

plot <- plot_ly(
  data = df1, 
  x = ~year, 
  y = ~total,
  color = I("darkred"),
  size = I(4),
  type = "scatter", 
  mode = "lines" # lines creates line plot
)

# axis grid lines
plot %>%
  layout(
    xaxis = list(showgrid = FALSE), # toggle TRUE/FALSE to remove major grid lines
    yaxis = list(showgrid = TRUE)
  )

# axis borders 
plot %>%
  layout(
    xaxis = list(showline = TRUE),
    yaxis = list(showline = TRUE)
  )

# grid line colours
plot %>%
  layout(
    xaxis = list(gridcolor = "lightgray", linecolor = "darkblue"),
    yaxis = list(gridcolor = "lightgray", linecolor = "darkblue")
  )





## color pallets -------------




# 4. facet plots --------------






