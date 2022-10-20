## 08_ggplot_extended

# further exploring visualisaitons in R using ggplot and other packages. 

# Set up ------

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)
library(ggplot2)
library(ggridges) # for ridgeline plot
library(ggthemes)
library(viridis)
library(heatmaply) # for heatmap
library(RColorBrewer) # for set colour palletes
library(treemap) # for tree map

options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate functio


load("data/Trade_datasets.RData")
uk_trqs <- read_excel("data/uk_trqs.xlsx") %>% clean_names()

# 1. Density plots ----------------------------------

## ridgeplot ------------------------------------------

# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#' ridgeplot is a helpful way to plot distributions
#' using the ggridges package
#' we cna plot multiple density plots together 
#' to help visual the distribution of data
#' broken down by groups
#' 

#' Example using TRQ data to plot the distribution of fill rates (0-100%)
#' 


# filter data for specific countries and year
df = uk_trqs %>% 
  filter(quota_origin %in% 
        c("Turkey","Iceland","Norway","Mexico","South Africa")) %>%
  filter(year == 2022)


# simple density ridge plot:
ggplot(df, 
       aes(
         x = quota_fill_rate, 
         y = quota_origin, 
         fill = quota_origin)) +
  geom_density_ridges()+
  theme_ridges() + 
  theme(legend.position = "none")+
  scale_x_continuous(
    "Quota fill rates",
    labels = scales::comma, # add commas within the value 
    n.breaks = 9 # adds the number of breaks on the scale. 
  )+
  ylab("")+ # remove y label
  scale_fill_brewer(palette = "Set3")


# example 2 using sector breakdown:
df = uk_trqs %>% 
  filter(quota_origin %in% c("Turkey")) %>%
  mutate(count0 = n_distinct(quota_number)) %>%
  group_by(sector,year) %>%
  mutate(count = n(),
         count2 = n_distinct(quota_number)) %>%
  filter(count >= 5)

ggplot(df, 
       aes(
         x = quota_fill_rate, 
         y = sector, 
         fill = sector)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  scale_x_continuous(
    "Quota fill rates",
    labels = scales::comma, # add commas within the value 
    n.breaks = 7 # adds the number of breaks on the scale. 
  )+
  ylab("")+ # remove y label
  scale_fill_brewer(palette = "Set3")


# gradient fill of plots. 
ggplot(
  df, 
  aes(
    x = quota_fill_rate, 
    y = sector, 
    fill = stat(x)
    )
  ) +
  geom_density_ridges_gradient(rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Fill-rate", option = "C")+
  theme_ridges()+
  xlab("")+
  ylab("")



# adding quantile lines to dist. plots:

df = uk_trqs %>% 
  filter(quota_origin %in% 
           c("Turkey","Iceland","Norway","Mexico","South Africa")) %>%
  filter(year == 2022)


ggplot(
   df, 
   aes(
     x = quota_fill_rate, 
     y = sector, 
     fill = sector
    )
  )+
  stat_density_ridges(quantile_lines = TRUE)



ggplot(
   df, 
   aes(
     x = quota_fill_rate, 
     y = sector, 
     fill = sector
    )
  )+
  geom_density_ridges(jittered_points = TRUE)+
  theme_ridges()+
  theme(legend.position="none")
  


# combine with patchwork for side by side plot
## histogram --------

#' example histogram of total trade values in 2020 for all countries:


df <- trade_annual %>% 
  filter(year == 2020) %>%
  filter(iso2 %notin% c("W1","D5","B5")) %>%
  filter(total >0) %>%
  arrange(desc(total)) %>%
  head(50)

# using geom_hist(). Note we only need to insert the x axis into aes. 
# The y axis is the count of each bin grouping. 

ggplot(df, aes(x = total))+
  geom_histogram(binwidth = 25000, color = "white", fill = "darkblue")


# TRQ fill rate distribution

df <- uk_trqs %>% filter(year == 2021) %>% filter(quota_status != "Future")

ggplot(df, aes(x = quota_fill_rate))+
  geom_histogram(
    binwidth = 0.1, 
    color = "white", 
    fill = "darkblue"
    )+
  ggthemes::theme_economist_white()


## grouped histogram 
# highlight two countries fill rate distribution:

df <- uk_trqs %>% 
  filter(year == 2021) %>%
  filter(quota_origin %in% c("Turkey","Norway"))


ggplot(df, aes(x = quota_fill_rate, fill = quota_origin))+
  geom_histogram(
    binwidth = 0.1,
    color="#e9ecef", alpha=0.6, position = 'identity'
  )+
  scale_fill_manual(values=c("#69b3a2", "#404080"))+
  ggthemes::theme_excel()


# multi-plot histogram of fill_rates

df <- uk_trqs %>% 
  filter(year == 2021) %>%
  filter(quota_origin %in% 
           c("Turkey","Norway","Iceland",
             "South Africa","Singapore","Mexico",
             "South Korea","Vietnam","Ukraine"))

ggplot(
   df,
   aes(
     x=quota_fill_rate, 
     color=quota_origin, 
     fill=quota_origin
     )
   ) +
  geom_histogram(alpha=0.6, binwidth = 0.1) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  viridis::scale_color_viridis(discrete=TRUE)+
  facet_wrap(~quota_origin, ncol = 3)+
  theme_base()+
  theme(legend.position = "none")




## density plot ---------------------------


# as with the above histogram examples
# plot the distribution of fill rates across the UK and countries:
# using geom_density:
#' Computes and draws kernel density estimate, 
#' which is a smoothed version of the histogram. 
#' This is a useful alternative to the histogram
#'  for continuous data that comes from an underlying smooth distribution.
#'  


df <- uk_trqs %>% 
  filter(year == 2021) %>%
  filter(quota_origin %in% c("Norway","Iceland","Turkey"))


ggplot(
  df,
  aes(
    x=quota_fill_rate,
    y=..scaled..) # keeps density y axis limited to 1. 
  ) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_bw()


# 

ggplot(
  df,
  aes(
    x=quota_fill_rate,
    #y=..scaled..,
    fill = quota_origin) # keeps density y axis limited to 1. 
) +
  geom_density(color="black", alpha=0.6)+
  theme_bw()


# sacked distibution plot:
ggplot(
  df,
  aes(
    x=quota_fill_rate,
    #y=..scaled..,
    fill = quota_origin) # keeps density y axis limited to 1. 
) +
  geom_density(color="black", alpha=0.6, position = "stack")+
  theme_bw()



# filled density plot:
df <- uk_trqs %>% 
  filter(year == 2021) %>%
  filter(quota_origin %in% c("Norway","Iceland","Turkey","Vietnam"))

ggplot(
  df,
  aes(
    x=quota_fill_rate,
    fill = quota_origin) # keeps density y axis limited to 1. 
) +
  geom_density(color="black", alpha=0.6, adjust=1.5, position="fill")+
  theme_bw()

## violin plot ------------------------

#' A violin plot is a compact display of a continuous distribution. 
#' It is a blend of geom_boxplot() and geom_density(): 
#' a violin plot is a mirrored density plot displayed
#'  in the same way as a boxplot.

df <- uk_trqs %>% 
  filter(year == 2021)


ggplot(
  df,
  aes(
    x = "All origins",
    y=quota_fill_rate
    )
  )+
  geom_violin(fill = "lightblue")+
  theme(legend.position = "none")+
  theme_bw()


df <- uk_trqs %>% 
  filter(year == 2021) %>%
  filter(quota_origin %in% 
           c(
             "Mexico",
             "Vietnam","Ukraine"))
ggplot(
  df,
  aes(
    x = quota_origin,
    y=quota_fill_rate,
    fill = quota_origin) 
)+
  geom_violin(
    width =2.5, # toggle width value to chnage spacing between groups
    size=0.3
    ) +
  scale_fill_viridis(discrete=TRUE)+
  coord_flip()+
  theme_gdocs()+
  theme(legend.position = "none")



# 2. Heat maps -----------------------------------------

#'https://www.data-to-viz.com/graph/heatmap.html
#'https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html
#' 

#' heat maps are a powerful visual to highlight the variance of values
#' across group(s) within a data set. 
#' For example:
#' What is the value of trade across countries and trade type. 
#' This is a useful way to present all of this informaiton together
#' to aid in comparing countries data across groups. 
#' 

#' *Example* heat map of countries trade across trade type or year


df <- trade_annual %>%
  filter(iso2 %in% c("FR","CA","US","DE","IE","NL","SE","US")) %>%
  filter(year == 2020)%>%
  select(country,import,export)
# ggplot


## base R -------------------------------------------------------

#' base R heatmap works with numeric matrix only. 
#' first step is to prepare data and convert to wide data format
#' of values wanting to be plot within the heatmap
#' then convert to a matrix and run in the heatmap function

#' prepare goods data for top 10 exported goods:

topGoods <- ons_NSA_goods_Annual %>% 
  filter(year == 2020) %>%
  filter(commodity != "T Total") %>%
  group_by(commodity) %>%
  summarise(exports = sum(annual_exports, na.rm = T)) %>%
  mutate(rank = rank(-exports)) %>%
  filter(rank <= 10)


df <- ons_NSA_goods_Annual %>%
  filter(year == 2020) %>%
  filter(iso2 %in% c("IS","NO","CH","TR","JP","ZA")) %>%
  filter(commodity %in% topGoods$commodity) %>%
  group_by(country, commodity) %>%
  summarise(exports = sum(annual_exports, na.rm = T)) %>%
  pivot_wider(
    names_from = commodity,
    values_from = exports
  ) %>%
  ungroup()

rowNames <- unique(df$country)
df <- select(df, -country)
rownames(df) <- rowNames

mat <- as.matrix(df)

heatmap(mat)

# options:

heatmap(
  x = mat,
  Colv = NA, # remove dendograms
  Rowv = NA,
  xlab="Country name", # examples - does not fit chart
  ylab="Commodity", 
  main="heatmap example using goods data",
  col = topo.colors(16), # colours
  #col = colorRampPalette(brewer.pal(8, "Blues"))(25), # RColourBrewer example
  cexRow = 1, # size of row Font
  cexCol = 0.75, # size of col Font
  margins = c(5,7)
)


## heatmaply ----------------------------------------------------
#heatmaply

# data
  df2 <- trade_annual %>%
    filter(iso2 %in% c("FR","CA","US","DE","IE","NL","SE","US")) %>%
    filter(year == 2020)%>%
    select(country,import,export)


rownames(df) <- df$country # create row names for heatmap plot
heatmaply::heatmaply(select(df,Export = export,Import=import),
                     main = "Test heatmap of trade data",
                     label_names=c("Country","Flow","Total Trade £"),
                     Colv = NULL,Rowv = NULL)

rownames(df) <- df$country
# further test of function inputs
heatmaply::heatmaply(
  select(df,Export = export,Import=import), # select data and label
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2( # cusotm colour scale. 
    low = "blue", # you can insert custom colours i.e. "#E69F00"
    high = "red", 
    midpoint = max(df$export)/2, 
    limits = c(0, max(df$export))),
  dendrogram = "none", # comment this out and see output
  xlab = "", ylab = "", 
  main = "Heatmap example using trade data",
  margins = c(60,100,40,20), # centre plot
  grid_color = "black",
  titleX = FALSE,
 #hide_colorbar = TRUE, # default is FALSE
 label_names = c("Country", "Feature:", "Value"),
 fontsize_row = 8, fontsize_col = 9,
 labRow = rownames(df),
 heatmap_layers = # add ggplot objects
   list(
     theme(axis.line=element_blank()),
     theme_dark()
   )
)


# trq fill rate example

#' basic example of hihglihting country avg. fill-rates
#' across multiple sectors via a heat map, utilising heatmaply. 


df = uk_trqs %>% 
  filter(quota_origin %in% 
           c("Turkey","Iceland","Norway","Mexico","South Africa","Vietnam")) %>%
  filter(year == 2022) %>%
  group_by(quota_origin,sector) %>%
  summarise(quota_fill_rate = mean(quota_fill_rate)) %>%
  pivot_wider(
    names_from = sector,
    values_from = quota_fill_rate
  ) %>%
  #mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  ungroup()

rowNames <- unique(df$quota_origin) # unique list of countries
df <- df %>% select(-quota_origin) # remove quota origin to leave numeric columns only
rownames(df) <- rowNames # set row names used in heatmap

# further test of function inputs
heatmaply::heatmaply(
  df,
  na.value = "grey",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2( # custom colour scale.
    low = "white", # you can insert custom colours i.e. "#E69F00"
    mid = "yellow",
    high = "red",
    midpoint = 0.5,
    limits = c(0,1)),
  dendrogram = "none", # comment this out and see output
  xlab = "", ylab = "", 
  main = "Heatmap example using TRQ data",
  margins = c(60,100,40,20), # centre plot
  branches_lwd = 0.1,
  grid_color = "black",
  titleX = FALSE,
  label_names = c("Country", "Feature:", "Value"),
  fontsize_row = 8, fontsize_col = 9,
  labRow = rownames(df),
  labCol = colnames(df),
  heatmap_layers = # add ggplot objects
    list(
      theme(
        axis.line=element_blank(),
      axis.text.x = element_text(hjust = 0.1, size = 8)
      )
    )
  )


## treemap ----------------------------------------------


#' treemap creates a representation of a variables share of a value
#' visualsising this as a rectangular object compiled with other variables
#' these charts are very powerful to visualize the share of a metric 
#' across variables and groups. 
#' 


# basic examle of treemap using country share of TRQ volumes. 

# prepare data

### basic treemap ---------------------

df <- uk_trqs %>% filter(year == 2021, trq_type == "FTA") %>%
  group_by(quota_origin) %>%
  summarise(volume = sum(quota_volume)) %>%
  mutate(prop = round(volume/sum(volume),2)) %>%
  mutate(label = paste(quota_origin,paste0(prop*100,"%"),sep ="\n")) %>%
  arrange(-(prop)) %>%
  head(10)


  treemap::treemap(
    dtf = df,
    index = "label", # group name from df. 
    vSize = "prop", # value name from df. 
    title = "Heat map example for TRQ quota volume share",
    palette = "Set3", # change color patellae, example from RColorBrewer sets. 
    border.col="grey50", # Color of borders of groups, of subgroups
    border.lwds=2, # size of border lines
    fontcolor.labels="black",
    fontface.labels=1,
    fontfamily.labels = "mono", # change font type, mono, serif, sans (default)
    fontfamily.title = "serif"
    ) 

### grouped treemap ----------------------------
  
# upload and prepare grouped trq data
# groupings are > region > individual country/trade agreement
  
df <- 
    read_excel("data/trq_agg_data.xlsx", sheet = "grouping_level") %>%
    clean_names() %>%
    filter(year == 2022, quota_unit_final == "Kilograms",total_quota_volume > 100000)

# basic usage:
  treemap::treemap(
    dtf = df,
    index = c("region","grouping"),
    vSize = "total_quota_volume",
    type = "index"
    
  )


# extended:
t <- treemap::treemap(
  dtf = df,
  index = c("region","grouping"),
  vSize = "total_quota_volume",
  title = "UK-quota volume grouped tree map.",
  type = "index",
  palette = "Dark2",
  border.col=c("black","grey"), # Color of borders of groups, of subgroups, of subsubgroups ....
  border.lwds = c(2,1),
  fontsize.labels=c(12,10),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
  fontcolor.labels=c("white","orange"),    # Color of labels
  fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
  bg.labels=c("transparent"),              # Background color of labels
  align.labels=list(
    c("centre", "top"), 
    c("right", "bottom")
  ),                   # Where to place labels in the rectangle
  overlap.labels=0.5, # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
  inflate.labels=FALSE  # If true, labels are bigger when rectangle is bigger.
)
  


# mirror plot --------- 

https://stackoverflow.com/questions/63301270/how-to-draw-both-positive-mirror-bar-graph-in-r

# plots tbc -----------


#https://www.business-science.io/code-tools/2022/03/30/how-i-analyze-100-ggplots-at-once.html
#trelloscopejs

#https://www.business-science.io/r/2021/08/12/ggalt-dumbbell-plots-ggplot2.html

#https://www.business-science.io/r/2021/08/24/ggalt-lollipop-plots-ggplot2.html

#https://www.business-science.io/r/2021/07/22/ggdist-raincloud-plots.html

#https://r-graph-gallery.com/dendrogram.html
