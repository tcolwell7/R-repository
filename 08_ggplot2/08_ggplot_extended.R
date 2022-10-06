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







# mirror plot --------- 

https://stackoverflow.com/questions/63301270/how-to-draw-both-positive-mirror-bar-graph-in-r

