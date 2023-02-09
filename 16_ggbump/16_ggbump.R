


# 16 ggbump package for bump plots to help track changes over data points, for example - time. 

# resources:

#' https://github.com/davidsjoberg/ggbump
#' https://github.com/AlbertRapp/PublicTidyTuesday/blob/main/2022_w28_euroflights/2022_w28_euroflights.qmd
#' https://github.com/davidsjoberg/ggbump/wiki/geom_bump-with-flags
#' https://github.com/jimjam-slam/ggflags/issues/13
#' 

library(tidyverse)
library(openxlsx) 
library(janitor)
library(readxl)
library(stringr)
library(rlang)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) # for set colour palletes
library(ggalt)
library(ggiraph) # interactive ggplots
library(ggiraphExtra) # extention 
library(ggbump)
library(ggflags)

#if(!require(ggflags)) devtools::install_github("rensa/ggflags")
#remotes::install_github('rensa/ggflags')


#' bump chart can be really effective in highlighting a change in value
#' or grouping
#' for example taking a countries rank position and 
#' visualising how this chnages over time
#' a bumpb chart is perfect
#' the ggbump chart has this inbuild to create simple and aesthetic charts
#' 
#' 


# 1. basic bump chart -----------

#' example - taking select countries we can rank the highest trading country over time
#' 

# filter trade data

df <- trade_annual %>% 
  filter(year %in% c(2000,2010,2020)) %>%
  filter(iso2 %in% c("US","DE","NL","CN","FR","IE","BE","NO","ES","CH")) # top 10 countries for UK trade

# create rank

df <- df %>% 
  group_by(year) %>%
  mutate(rank = rank(-total)) %>%
  ungroup() %>%
  arrange(-total)

# print table
knitr::kable(head(df))

# make basic plot

df %>% 
  ggplot(aes(year, rank, color = country)) +
  geom_bump(linewidth=1,smooth=8)+ # larger smooth value - steeper the curve
  geom_point(size=1)+
  theme_minimal()+
  scale_y_reverse(n.breaks=11)+ # flip y-axis
  scale_x_continuous(n.breaks=3) # 3 data points on x-axis


## customize plot -----


#' add text values at data points
#' instead of legends to make chart clearer
#'
#'


# custom colour for countries

# https://r-graph-gallery.com/ggplot2-color.html

x = c("United States inc Puerto Rico"="red",
                       "Germany"="darkslategrey",
                       "France"="blue1",
                       "Netherlands"="darkorange",
                       "Ireland"="darkgreen",
                       "Spain"="darkgoldenrod1",
                       "Belgium"="darkorange2",
                       "Switzerland"="deeppink",
                       "Norway"="darkred",
                       "China"="brown3")

                       
                     
bump <-
  df %>%
    ggplot(aes(year,rank,color=country))+
    geom_text(
      data = df %>% filter(year == min(year)),
      aes(x = year - .2, 
          label = country
          ),
      size = 4, 
      hjust = 1) +
    geom_text(
      data = df %>% filter(year == max(year)),
      aes(
        x = year + .3, 
        label = country),
      size = 4, 
      hjust = 0) +
    geom_bump(size=1.5,smooth=7)+
    geom_point(size=3)+
    scale_y_reverse(n.breaks=11)+
    scale_x_continuous(
      limits = c(1998, 2021), # extend x axis limits
      n.breaks = 3
      )+
    xlab("")+ylab("")+
     scale_color_manual(
       values=x
       )+
    #scale_color_brewer(palette = "Paired")+
    theme_minimal()+
    theme(
      axis.text.y = element_text(size=12),
      legend.position="none",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()
    )
  


# test further formatting

bump + 
  labs(
    title = "Top 10 UK trade partners in 2021 ranking history",
    subtitle = "Ranking based on highest trade value at given year for current top 10 trading partners",
    caption = "Source: ONS 2022"
    )+
  theme(
    plot.background = element_rect(fill="azure1"),
    plot.title = element_text(color="grey52",size=16),
    plot.subtitle = element_text(color="darkgrey",size=11),
    axis.text.x = element_text(size=12)
  )

