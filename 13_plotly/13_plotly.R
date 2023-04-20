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



### axis scales -----------------


plot <- 
  plot_ly(
    data = df1, 
    x = ~year, 
    y = ~total,
    color = I("blue"),
    size = I(4),
    type = "scatter", 
    mode = "lines"
  ) %>% 
  layout(
    title = "Line Plot of Trade ", 
    xaxis = list(title = "Year"), 
    yaxis = list(title = "Total Trade £mn.")
  )


# set y axis scale at 0

plot %>% layout(yaxis = list(range = c(0, max(df1$total))))

# custom axis ticks

plot %>% 
  layout(
    yaxis = 
      list(
        range = c(0, max(df1$total)), # axis limits using range
        nticks = 5 # use the ntick argument
      ),
    xaxis =
      list(
        range = c(2000,2012), # specify x axis range
        nticks = 13
      )
  )


#### custom axis text ----

# million scale format

plot %>%
  layout(
    yaxis = 
      list(
       tickformat = ',.0f',  # format tick labels with commas for thousands
       ticksuffix = 'M'      # add 'M' to the end of tick labels to denote millions
      )
    )


# thousand scale format

df = trade_annual %>% filter(iso2 == "US")

plot_ly(
  data = df,
  x = ~year,
  y = ~balance,
  type = "scatter", 
  mode = "lines"
) %>% 
  layout(
    yaxis = list(
  tickformat = 's',     # use short format
  #tickformat = ',.0f',
  tickprefix = '£',     # add a pound sign to the beginning of the labels
  hoverformat = '£,.0f' # use dollar sign and comma separator with no decimal places for hover labels
  )
 )



# axis for percentage range 0 - 1

df <- trade_annual %>% 
  filter(iso2 == "US") %>% 
  mutate(export_pc = export / total)


plot_ly(
  data = df,
  x = ~year,
  y = ~export_pc,
  type = "scatter",
  mode = "markers"
 ) %>%
  layout(
    yaxis = list(
      range = c(0,1),
      nticks = 11,
      tickformat = '%' # add % format for y axis 
     )
   )


# modify the y-axis range and tick mode
layout(yaxis = list(range = c(0, 10), tickmode = "linear", tick0 = 0, dtick = 1))
  
layout(yaxis = list(range = c(0, 10), tickmode = "linear", tick0 = 0, dtick = 2))

layout(yaxis = list(range = c(0, 1100000), tickmode = "linear", tick0 = 0, dtick = 250000))

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

# you can add lines using the add_trace function and specific points:

grouped_plot %>%
 layout(
   shapes = list( # vertical line
     list(
       type = "line",
       x0 = 2016,
       x1 = 2016,
       y0 = 0,
       y1 = max(df2$export), # max value of grouped data
       line = list(
         color = "red",
         width = 2,
         dash = "dot"
        )
       ),
     list(
       type = "line",
       x0 = 1999,
       x1 = 2021,
       y0 = 30000,
       y1 = 30000, # max value of grouped data
       line = list(
         color = "blue",
         width = 2,
         dash = "dot"
       )
      )
     ), # add text # use annotations
       annotations = list(
           x = 2017,
           y = 50000,
           text = "BREXIT",
           xref = "x",
           yref = "y",
           showarrow = FALSE,
           textangle = 90,
           font = list(family = "Arial", size = 20, color = "red")
        )
      )
    
  

### add text ------------------

# you can add text using the annotations argument within layout

grouped_plot %>% 
  layout(
    annotations = list(
      x = 2020,
      y = 10000,
      text = "Anomoly value",
      xref = "x",
      yref = "y",
      font = list(family = "Arial", size = 18, color = "grey50"),
      showarrow = T # toggle false/true 
    )
  )

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

# zero line remove

plot %>% layout(xaxis = list(zeroline = FALSE))

# grid line colours
plot %>%
  layout(
    xaxis = list(gridcolor = "lightgray", linecolor = "darkblue"),
    yaxis = list(gridcolor = "lightgray", linecolor = "darkblue")
  )





## color pallets -------------

#' https://statisticsglobe.com/change-colors-plotly-graph-r
#

# custom color
grouped_plot <- 
  plot_ly(
    data = df2, 
    x = ~year, 
    y = ~export, 
    type = "scatter", 
    mode = "lines", 
    color = ~country,
    #colors = c("grey50", "blue", "red","green","yellow"),
    colors = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
    showlegend = T
  )


plot_ly(
  data = df2, 
  x = ~year, 
  y = ~export, 
  type = "scatter", 
  mode = "lines", 
  color = ~country,
  colors = "Set3", # you can add custom RColorBrewer colors
  showlegend = T
)
  



# 4. secondary axis -------

# basic plot creating secondary axis
# plot total trade and imports on secondary for country as time-series

# you can use the add_trace function to add secondary axis

df = trade_annual %>% filter(iso2 == "US")


plot_ly(
  data = df,
  x = ~year, 
  y = ~total, 
  type = "scatter",
  mode = "lines",
  name = "Total Trade"
  ) %>%
  add_trace(
  #  data = df1 # note you can select different data to plot within add_trace while piping
    x = ~year, 
    y = ~import, 
    mode = "lines", 
    yaxis = "y2", 
    name = "Imports"
    ) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) # secondary axis position



# stacked bar plot with secondary axis

df_long = df %>%
  pivot_longer(
    cols = import:balance,
    names_to = "flow",
    values_to = "value"
  )


plot_ly(
  data = df_long %>% filter(flow != "balance"),
  x = ~year,
  y = ~value,
  type = "bar",
  color = ~flow,
  name = ~flow,
  legendgroup = ~flow
) %>% # add laytout, barmode = "stack" for stacked barchart 
  layout(barmode = "stack") %>%
  add_trace(
    data = df_long %>% filter(flow == "balance"),
    x = ~year, 
    y = ~value, 
    type = "scatter",
    mode = "lines", 
    yaxis = "y2", 
    name = "balance"
  ) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")) %>% # secondary axis position
  layout(legend = list(x =1.1, y = 0.5))


# percentage as secondary axis

#' when plotting with ggplot
#'  you have to scale the secondary axis values
#'  but seemingly not with plotly add_trace 
#'  it automatically scales for you
#'   


df <- df %>% mutate(import_pc = import / total)


plot_ly(
  data = df,
  x = ~year,
  y = ~total,
  type = "scatter",
  mode = "lines"
) %>%
  add_trace(
    x = ~year,
    y = ~import_pc,
    type = "scatter",
    mode = "markers",
    yaxis = "y2", 
    name = "second"
  ) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))
  

## add_layer -----

# you can draw a secondary axis using the add_layer function

#' I used this method when converting a ggplot object 
#' using ggplotly() 
#' which removed the secondary axis 
#' so I had to add a fixed axis from 0% to 100% 
#' where add_layer I was able to find a solution

plot_ly(
  data = df,
  x = ~year,
  y = ~total,
  type = "scatter",
  mode = "lines"
) %>%
  add_lines( 
    data = df,
    x = ~year,
    y = ~import_pc,
    yaxis = "y2",
    inherit = FALSE,
    showlegend = FALSE
  ) %>%
  layout( # ensure layout is used to visualise y2 on right hand side
    yaxis2 = 
      list(
       tickfont = list(size=11.7),
       titlefont=list(size=14.6),
       overlaying = "y",
       showgrid = F,
       zeroline = F,
       nticks = 11,
       side = "right",
       title = ""
    )
  )
 

# render the interactive plotly graph, and add the annotations to it
chart <- ggplotly(HSSectionPlot, height = 700, tooltip = "text") %>% 
  layout(legend = list(orientation = "v", y = 0.5, x=1)) %>%
  add_lines(x = ~sector_name,y=~pur_fta_year, colors=NULL, yaxis="y2",
            data=df, showlegend=FALSE, inherit=FALSE) %>%
  
  ay <- list(
    tickfont = list(size=11.7),
    titlefont=list(size=14.6),
    overlaying = "y",
    showgrid = F,
    zeroline = F,
    nticks = 7,
    side = "right",
    title = ""
  )


# more tbc --------










