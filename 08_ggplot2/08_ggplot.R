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

# set up data: filter ons annual trade data for 5 countries:

country_filt <- c("US","DE","IE","FR","JP")
df <- trade_annual %>% filter(iso2 %in% country_filt)

print(head(df,10))

# 1. basic plot -------------------------------------

#  R documentation :
' ggplot() initializes a ggplot object. 
 It can be used to declare the input data frame for a graphic
 and to specify the set of plot aesthetics 
 intended to be common throughout all subsequent layers unless specifically overridden.'

#' you can initiate a plot using ggplot(). 
#' This creates a global object you can then run 
#' 

# plot a basic line plot for the US. 

df2 <- df %>% filter(iso2 == "US")

# define ggplot object. 

plot <- ggplot()

# nothing has happened - besides creating the object. 
# we need to input the arguments into the global object. 

plot <- 
  ggplot(
    data = df2, # define data
    aes(        # define aesthetics into plot (i.e. axis)
      x = year,
      y = total
    )
  )

plot


#' we have defined the axis aesthetic for the plot
#' data hasn't been plot as we haven't told R how. 
#' We need to add one of ggplots inbuilt plot functions
#' such as 'geom_point' 'geom_bar' etc. 
#'

plot <- 
  ggplot(
    data = df2, # define data
    aes(        # define aesthetics into plot (i.e. axis)
      x = year,
      y = total
    )
  ) +
  geom_point()

plot

# attentively we can just add a ggplot object to the already defined object:

plot + 
  geom_point(color = "blue") + # you can define the plot color within the ggplot
  geom_line(color = "red")

#' NOTE: you don't have to define an object then run that object separately. 
#'  you can run from the ggplot line i.e:
#'  

ggplot(
  data = df2, # define data
  aes(        # define aesthetics into plot (i.e. axis)
    x = year,
    y = total
  )
) +
  geom_point()+
  geom_line()

#' defining the object is necessary for certain tasks or functions. 
#' It's good to be aware of this. 


# basic bar plot

ggplot()+
geom_bar(
  data = df2,
  aes(
    x = year,
    y = total
  ),
  stat = "identity",
  color = "purple",
  fill = "darkblue"
)


# basic area plot

ggplot()+
  geom_area(
    data = df2,
    aes(
      x = year,
      y = total
    ),
    color = "blue",
    fill = "red",
    alpha = 0.5 # changes the fill fade. try different values to experiment
  )


## 1i. define plot aes -----------------------------

#' in the above example we defined the aesthetics of the plot
#' within defining the ggplot global objective (ggplot(data, aes,..)
#' this can alternatively be defined 



ggplot()+
  geom_point(
    data = df2,
    aes(
      x = year,
      y = total
    ),
    color = "blue",
    shape = 6  # http://www.sthda.com/english/wiki/ggplot2-point-shapes # shape codes. 
  )+
  geom_line(
    data = df2,
    aes(
      x = year,
      y = total
    ),
    color = "red", 
    size = 1
  ) 

#' defining data within each geom plot is helpful when
#' working with different data sets
#' or filtering data for different plots. 
#' If you are plotting using the same dataset 
#' it's best to define the data and aesthetics (axis, group etc.) within ggplot()

### 1ii. filter data within aes -----------------------

#' plot two line plots for US and Japan trade values with the UK:

 
ggplot()+
  geom_line(
    data = df %>% filter(iso2 == "US"),
    aes(
      x = year,
      y = total
    ),
    color = "blue"
  ) +
  geom_line(
    data = df %>% filter(iso2 == "JP") %>% filter(year >= 2005), # you can continue to pipe within the geom 
    aes(
      x = year,
      y = total
    ),
    color = "red"
  )




# barplot example


ggplot()+
  geom_bar(
    data = df %>% filter(iso2 == "US"),
    aes(
      x = year,
      y = total
    ),
    stat = "identity",
    color = "purple",
    fill = "darkblue"
  )+
  geom_bar(
    data = df %>% filter(iso2 == "JP"),
    aes(
      x = year,
      y = total
    ),
    stat = "identity",
    color = "red",
    fill = "blue"
  )
    
    
#' The default is to stack the data (but not ontop of each other so the bars increase in height)
#' This leads us into working with group data and
#' calling the necessary fill options and define the bar position
#' such as stacked bar plot of a bar plot side-by-side. 

#### 1iii. wide data ---------------------------------


#' you can plot multiple geom plots calling different columns within the dataset
#' 

ggplot(
   data = df %>% filter(iso2 == "JP"),
   aes(x = year)
  )+
  geom_line(
    aes(y = total),
    color = "blue"
  )+
  geom_line(
    aes(y = import),
    color = "red"
  )+
  geom_line(
    aes(y = export),
    color = "purple"
  )


# 2. grouped plots -----------------------------------

#' We use grouped data frequently. 
#' When data is in the long format there will likely be a grouping
#' In the data used in the above section it is a combination of wide/long
#' As we have year and country in the long format
#' with trade metrics in wide. 
#' How the data is currently set up there is a grouping we can plot
#' using the country names. 
#' 

#' We can plot multiple line plots
#' Or bar plots / stacked or side-by-side
#' By utilizing the group by inputting the "fill" or "color" aesthetic with 
#' the group you want to plot. 
#' 

country_filt <- c("US","DE","IE","FR","JP")
df <- trade_annual %>% filter(iso2 %in% country_filt)

ggplot(
  data = df,
  aes(
    x = year,
    y = total,
    color = iso2 # geom_line or point use color 
   )
  )+
  geom_line()

# if you wanted to plot each line differently
# you could create 5 custom geom plots. 
# utilising the fill/color aesthetic is simple and easy. 


# multiple bar plot side-by-side use stat = "dodge" outsdie of aes argument. 
ggplot()+
  geom_bar(
    data = df %>% filter(iso2 %in% c("US","DE")),
    aes(
      x = year,
      y = total,
      fill = iso2
    ),
    stat = "identity",
    position = "dodge",
    color = "blue"
  )


ggplot()+
  geom_bar(
    data = df,
    aes(
      x = year,
      y = total,
      fill = iso2
    ),
    stat = "identity",
    position = "dodge",
    color = "blue"
  )


# stacked bar chart

ggplot(
  data = df %>% filter(iso2 %in% c("US","JP")),
  aes(
    x = year,
    y = total,
    fill = iso2
   )
  ) +
    geom_bar(
      stat = "identity", 
      position = "stack",
      color = "red") # defining the colour outside the aes will fill the bars on the outside for all groups. 



# stacked area plot

ggplot()+
  geom_area(
    data = df,
    aes(
      x = year,
      y = total,
      fill = iso2
    ),
    color = "blue" # will color endge of area lines
  )


## 2i. multiple groups --------------------------

#' data can have multiple groups, especially when in long format. 
#' the ONS trade data within this script being used
#' current has countries as a group
#' and in the wider columns a further group with "trade type"
#' (i.e. import/export/total/balance)
#' If the data is turned into a fully long data format 
#' there will be two groups we can plot


# create long dataframe:

country_filt <- c("US","DE","IE")
df <- trade_annual %>% filter(iso2 %in% country_filt)

df_long <- df %>% 
  pivot_longer(
    import:export, # import and export values
    names_to = "trade_type", 
    values_to = "value"
    )

ggplot(
   data = df_long,
   aes(
     x = year,
     y = value,
     color = iso2,
     shape = trade_type,
     size = value
    )
  )+
  geom_point()

# notice you can create multiple groups within a plot.

### 2.1 grouped prop. plot -----------------------


#' Grouped proportional plot. Create a stacked bar or area plot
#' where each individual within a group's proportion is plotted
#' The plot scales is fixed at 1
#' rather than the actual trade values
#' This is a very handy plot to have. 


country_filt <- c("US","DE","IE","FR","JP")
df <- trade_annual %>% filter(iso2 %in% country_filt)

# group by the group which will be used to create a stacked plot
# i.e. year: how large proportionally is is each countries value across each year

df <- df %>% group_by(year) %>% mutate(total_prop = total / sum(total))
  
ggplot()+
  geom_bar(
    data = df,
    aes(
      x = year,
      y = total_prop,
      fill = iso2
    ),
    stat = "identity",
    position = "stack",
    color = "grey"
  )


# I quite like this plot and use it quite often 

ggplot()+
  geom_area(
    data = df,
    aes(
      x = year,
      y = total_prop,
      fill = iso2
    ),
    color = "darkgrey"
  )



# 3. basic formatting -------------------

#' ggplot's theme function we are able to customize plots. 
#' You can use pre-set themes or customise every main aspect of the plot
#' such as axis, titles, fonts, grids, legends etc. 
#' 

#' *in the console type ?theme for all options* 

#' I will cover the main ones generally used and ones I frequently use. 


# first create a plot:

plot <- 
  ggplot(
    data = df %>% filter(iso2 %in% c("DE","IE")),
    aes(
      x = year,
      y = total,
      color = iso2
     )
   )+
  geom_line(size = 2)

plot

### 3i.  axis labeling --------------

# we can label axis without using theme:

plot <- 
  plot + 
  xlab("Year") +
  ylab("Total trade with the UK") +
  ggtitle("Chart Title")

# we can chance the axis labels using theme:

plot <- plot + 
  theme(
     plot.title = element_text(size = 18, hjust = 0.5), # move title centrally
     axis.title.x = element_text(size = 16),
     axis.title.y = element_text(size = 12),
     axis.text.x = element_text(size = 12, family = "mono"),
     axis.text.y = element_text(size = 14, family = "mono") # change font text
     
)

plot

#### 3ii. background and panels ---------------------------

# using theme you can amend the background of the plot and grid lines. 

plot <- ggplot(
    data = df %>% filter(iso2 %in% c("DE","IE")),
    aes(
      x = year,
      y = total,
      color = iso2
    )
  )+
  geom_line(size = 2)+
  xlab("Year") +
  ylab("Total trade with the UK") +
  ggtitle("Chart Title")

# change grid lines and panel border to grey:

plot + 
  theme(
    panel.border = element_rect(color = "grey",fill=NA, size=2), # changes border color and size
    panel.grid = element_line(colour="lightgrey", size=0.5)
  )


# plot grid lines for y-axis only. 

plot + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "lightblue", size = 1)
  )

# compare where gird minor vs major position:
plot + 
  theme(
    panel.grid.minor.x = element_line(colour = "lightblue", size = 1),
    panel.grid.minor.y = element_line(colour = "lightblue", size = 1)
  )



##### 3iii. remove elements -----------------------------


#' notice with the above all the major and minor grid lines remain. 
#' we can remove all grid lines and then amend them from scratch. 
#' using element_blank()
#' we can apply this function to axis, title etc. 


# compare the above snippets of code plots when removing the panel background:

plot + theme(panel.background = element_blank()) # removes all gridlines and panel 


plot + 
  theme(
    panel.background = element_blank(),
    panel.grid.minor.x = element_line(colour = "lightblue", size = 1),
    panel.grid.minor.y = element_line(colour = "lightblue", size = 1)
    )
    
    
plot + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = "lightblue", size = 1),
    panel.grid.major.y = element_line(colour = "lightblue", size = 1),
    panel.grid.minor.x = element_line(colour = "red", size = 1),
    panel.grid.minor.y = element_line(colour = "red", size = 0.1),
    panel.border = element_rect(color = "grey",fill=NA, size=2) # create border line - ensure you use fill=NA otherwise plot is removed by default
  )
  

###### 3iv. legend formatting ------------------------------

#' within theme we can update and change the legend
#' such as position, size, color and labels. 

plot <-
 ggplot(
   data = df %>% filter(iso2 %in% c("DE","IE")),
   aes(
     x = year,
     y = total,
     color = iso2
    )
  )+
  geom_line(size = 2)+
  labs(color="COUNTRY") + # you can use labs to change the title of the aesthetic. 
  theme(
    legend.title = element_text(size = 12, family = "mono"), # chnage legend text
    legend.direction = "horizontal", # change legend direction
    legend.justification = c(0.5,0), # move legend position
    legend.background = element_rect(fill = "lightgray"),
    legend.key.size = unit(0.2, "cm") # changes the colour key size within the legend. 
  )

plot + theme(legend.title = element_blank()) # remove legend title
plot + theme(legend.background = element_rect(fill = NA, color = NA)) # remove background colour

# remove legend:

plot + theme(legend.position="none")

# 3.1 custom themes --------------------------------------

#' There are many custom themes set up for quick use. 
#' Plot each example to see what you like. 
#' You can add a custom theme then amend this as you like. 
#' 

#' useful web-link: https://themockup.blog/posts/2020-12-26-creating-and-using-custom-ggplot2-themes/


plot + theme_bw() # I ofetn use this as a base to build upon. 
plot + theme_classic()
plot + theme_dark()
plot + theme_light()
plot + theme_linedraw()
plot + theme_minimal()
plot + theme_void()


# you can utilise custom R packages such as ggthemes

#install.packages("ggthemes")  # Install ggthemes package
library("ggthemes")  

plot + ggthemes::theme_fivethirtyeight() 
plot + ggthemes::theme_economist()
plot + ggthemes::theme_wsj()
plot + ggthemes::theme_stata()
plot + ggthemes::theme_excel()
plot + ggthemes::theme_gdocs()
plot + ggthemes::theme_pander()
plot + ggthemes::theme_hc()
plot + ggthemes::theme_solarized()

# full list of exmaples: https://github.com/BTJ01/ggthemes/tree/master/inst/examples


# add own changes on top of custom themes:

plot <- plot +
  theme_bw()+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5), # move title centrally
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, family = "mono")
  )


# 4. advanced formatting -------------------------------

#' There are countless ways you can edit your plots 
#' Here are a few main ones which have served me well


# create bar plot example for one year worth of data
# create line plot for year scale example

country_filt <- c("US","DE","IE","FR","JP")
df <- trade_annual %>% filter(iso2 %in% country_filt)

plot <- 
  ggplot(
    data = df %>% filter(year == 2020),
    aes(
      x = country,
      y = total,
      fill = iso2
    )
  )+
  geom_bar(stat = "identity")


# line plot
plot2 <- 
  ggplot(
    data = df,
    aes(
      x = year,
      y = total,
      color = iso2
    )
  )+
  geom_line(size = 2)


## 4i. custom scales ------------------------------------

#' https://scales.r-lib.org/ 

# the default scales setting at times can be frustrating. The following are 
# examples to chnage x axis as year or a discrete value
# and to chnage the number of 'ticks' on the y-axis using scales. 

# total trade data is continuous so we can use the function
# scale_y_continuous 

plot <-
  plot + 
  theme_bw() +
  scale_y_continuous(
    "Scale title",
    labels = scales::comma, # add commas within the value 
    n.breaks = 10 # adds the number of breaks on the scale. 
  )

# ammend x axis so the text is at an angle and doens't overlap:

plot + theme(axis.text.x = element_text(angle = 20)) 

plot + theme(
  axis.text.x = element_text(angle = 30, vjust = 0.7), # use vjust to chnage vertical position of text so doenst overlap
  axis.title.x = element_blank()
)


# change x axis titles using scale_discrete:

custom_label <- c("FRANCE.","GERMANY.","IRE","JAPAN", "USA")

plot + scale_x_discrete("Custom x axis title", labels = custom_label)


# change x axis for yearly line plot:

# this will plot each individual year
plot2 + 
  scale_x_continuous("X Axis update", breaks = seq(min(data$year), max(data$year)))

# plots every 2 years from 200 on axis
plot2 + 
  scale_x_continuous("X Axis update", breaks = seq(2000, max(data$year), 2))


## 4ii custom colour pallets -----------------


#' you can apply custom colour pallets to your plot
#' based on pre-created pallets or custom from the user
#' 

# you can find pre-created colour pallets and individual color codes here:
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/


plot + scale_fill_brewer()
plot + scale_fill_brewer(palette = "Set1")
plot + scale_fill_brewer(palette = "Spectral")
plot + scale_fill_brewer(palette = "YlOrRd")
plot + scale_fill_brewer(palette = "Pastel2")
  


# manual fill colours

colr <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
plot + scale_fill_manual(values = c(colr))

colr <- c("#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot2 + scale_color_manual(values = colr)



## 4iii. flip scales -----------------------------------------

# it can be useful to plot barplots so the bars go along the x axis
# aesthetically this can improve the impact of the visual
# and help fit longer x labels into the plot. 
# we can use coord_flip() to achieve this. 



# coord flip

custom_label <- c("FRANCE.","GERMANY.","IRE","JAPAN", "USA")
colr <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

ggplot(
  data = df %>% filter(year == 2020),
  aes(
    x = country,
    y = total,
    fill = iso2
    )
  )+
  geom_bar(stat = "identity", color = "darkgrey")+
  theme_bw() +
  scale_y_continuous(
    "Scale title",
    labels = scales::comma, # add commas within the value 
    n.breaks = 7 # adds the number of breaks on the scale. 
  )+
  scale_x_discrete("Custom country labels", labels = custom_label)+
  scale_fill_manual(values = c(colr))+
  coord_flip()



# 5. multiple plots -----------------

# IDEA - plot using two different datasets
# geom line connecting specific ones
# bar char with geom point/line like the TRQ usage plot example
# geom text for trade balance?

#' you can combine multiple plots from different datasets together. 
#' This is where we can insert the data and aesthetics
#' within the ggplot geom plots
#' rather than the global object. (ggplot())
#' 

# Example: plot bar chart for countries while other countries plot line
# this plot is to demonstrate tha scale of US trade compared to Fr, Japan, Ireland. 

country_filt <- c("IE","FR","JP")
df <- trade_annual %>% filter(iso2 %in% country_filt)
df2 <- trade_annual %>% filter(iso2 %in% c("US"))


plot <- 
  ggplot()+
  geom_bar(
    data = df, # plot first dataset
    aes(
      x = year,
      y = total,
      fill = iso2
      ),
    position = "stack",
    stat = "identity"
   )+
  geom_line( 
    data = df2, # plot 2nd dataset
    aes(
      x = year,
      y = total
    ),
    color = "red",
    size = 2
  )+
  theme_minimal()
  
# an issue here is - if we wanted to plot a scatter/line plot for the US. 
# we need to add a further geom with the data inserted inside which adds to the code length. 


plot <-
  plot +
  geom_point(
    data = df2, # plot 2nd dataset
    aes(
      x = year,
      y = total
    ),
    color = "blue",
    size = 2
  )

# we can add more countries to this plot. 
# NOTE: we can't add an additional bar plot - as this is then added ONTOP of the current bars
# the bar plot needs to be plotted together. 


plot +
  geom_point(
    data = trade_annual %>% filter(iso2 %in% c("DE")), # add Germany into plot
    aes(
      x = year,
      y = total
    ),
    color = "black",
    size = 2
  )+
  geom_line(
    data = trade_annual %>% filter(iso2 %in% c("DE")), # add Germany into plot
    aes(
      x = year,
      y = total
    ),
    color = "grey",
    size = 1
  )

# notice when you plot the line after the points - the line is superseeded on top of the points. 
# its preferable to plot the line then points:


plot +
  geom_line(
    data = trade_annual %>% filter(iso2 %in% c("DE")), # add Germany into plot
    aes(
      x = year,
      y = total
    ),
    color = "grey",
    size = 1
  )+
  geom_point(
    data = trade_annual %>% filter(iso2 %in% c("DE")), # add Germany into plot
    aes(
      x = year,
      y = total
    ),
    color = "black",
    size = 2
  )


# 6. facet plots --------------------

#' facet plots enable you to plot grouped data in separate plots. 
#' instead of plotting a grouped line/scatter plot
#' the grouped data will be split into a grid of different plots
#' this can be very helpful for when looking to analyse charts charts
#' and see clear trends across data


country_filt <- c("IE","FR","ES","NL","ZA","CA")
df <- trade_annual %>% filter(iso2 %in% country_filt)

# for facte plots - we don't apply the group within the aes input. 
# we add the facet_wrap at the end

plot <-
 ggplot(
   data = df,
   aes(
     x = year,
     y = total
     )
   )+
   geom_point()+
   geom_line()

# facet_wrap()
plot +  facet_wrap(. ~ iso2)
plot +  facet_wrap(. ~ iso2, scales = "free") # individual scales plotted 
plot +  facet_wrap(. ~ iso2, scales = "free", ncol = 2) # ncol changes number of plots per row
# facet_grid()
plot +  facet_grid(. ~ iso2)
plot +  facet_grid(iso2 ~ .) # spread chart the opposite direction



# plot multiple groups against each other. 
# i.e. country vs trade type

country_filt <- c("NO","IS","CH")
df <- trade_annual %>% filter(iso2 %in% country_filt)

df_long <- df %>% 
  pivot_longer(
    import:total, # import and export values
    names_to = "trade_type", 
    values_to = "value"
  )

plot <-
  ggplot(
    data = df_long,
    aes(
      x = year,
      y = total
    )
  )+
  geom_point()+
  geom_line()+
  facet_wrap(trade_type ~ iso2, scales = "free") # comment out and try grid vs wrap
  facet_grid(trade_type ~ iso2, scales = "free")
  

# nice contrast plotting colours over grey points. 
ggplot(
  data = df_long %>% filter(iso2 == "NO"), 
  aes(
    x = year, 
    y = value
    )
  ) + 
   geom_point(data = df_long %>% filter(iso2 %in% c("NO","CH")), colour = "grey70") +
   geom_point(
     aes(colour = trade_type)) + 
   facet_wrap(~trade_type)
  

# for more this is good training material: https://ggplot2-book.org/facet.html



# 7. double-axis ---------------------------

#' plotting to plots together with a secondary axis
#' can be a very powerful way to present combined/related data together


#' *Example* plot Imports/Exports and Total trade on secondary axis. 
#' 

#' There isn't a function or formula that I am aware of
#' that allow syou to plot two sets of data with differing scales
#' you have to transform (or scale) the secondary data you want to plot 

df <- 
  trade_annual %>% 
  filter(iso2 == "DE") %>%
  mutate(avg = rowMeans(df[,c("import","export")])) %>%
  rowwise() %>% # group by row for row sum
  mutate(totalScaled = total*(avg/total)) %>% # scale value based on proportion %>%
  mutate(totalScaled2 = max(across(import:export))*1.05) %>% # plot 5% above the max value of two columns
  ungroup()

scale <- mean(df$total/max(df$import,df$export)) # individual scale value to apply to create the secondayr axis - e.g. apply a ration value. 

ggplot()+
  geom_line(
    data = df,
    aes(x = year,y = import),
    color = "blue",
    linetype = "dotted",
    size = 1
  )+
  geom_line(
    data = df,
    aes(x = year,y = export),
    color = "red",
    linetype = "dashed"
  )+
  geom_line(
    data = df,
    aes(x = year,y = totalScaled2),
    color = "darkblue",
    size = 1
  ) +
  theme_light()+
  scale_y_continuous(
    sec.axis = sec_axis(~ . * scale))
  

## example 2 -

#' *Exmaple* create a plot of a stacked plot - with % split as secondary axis. 
#'           want a stacked bar chart for imports/exports + scatter point for %. 


df <- 
  trade_annual %>% 
  filter(iso2 == "JP") %>%
  mutate(perc = export/total) %>%
  mutate(scaledPerc = perc*(export/perc)) %>%
  pivot_longer(
    cols = import:scaledPerc,
    names_to = "type",
    values_to = "value"
  )

ratio <- max(df$export) / max(df$perc)


ggplot()+
  geom_bar(
    data = df %>% filter(type %in% c("import","export")),
    aes(x=year,y=value,fill=type), 
    stat = "identity",
    width = 0.4,
    color = "darkblue",
    alpha = 0.6)+
  geom_point(
    data = df %>% filter(type == "scaledPerc"),
    aes(x=year,y=value), 
    size = 3, 
    label = "perc",
    color = "red")+
  theme_bw()+
  scale_y_continuous(
    labels = scales::label_dollar(),breaks = scales::breaks_extended(8),
    sec.axis = sec_axis(~ . / ratio,labels = scales::label_percent()))
    


# 8. other plots ---------------------------


tbc.

# there are plenty of charts to plot which can be found here:
# https://r-graph-gallery.com/index.html


# End. 
