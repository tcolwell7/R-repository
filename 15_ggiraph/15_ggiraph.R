
# 15 ggirafe package for interactive ggplot objects

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

options(scipen=999)


path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

`%notin%` <- Negate(`%in%`) # Custom negate function



uk_trqs <- read_excel("data/uk_trqs.xlsx") %>% clean_names()
uk_trqs$year <- as.character(uk_trqs$year)


# 1. basic charts ------------

## boxplot --------

# basic boxplot for fill-rate distribution for UK TRQs

box_plot <- uk_trqs %>%
  ggplot(
    aes(
      x = quota_fill_rate, 
      y = year,
      group=year,
      fill=year))+
  geom_boxplot(
    stat="boxplot",
    outlier.alpha=0.2,
    outlier.color="grey52")+
  theme_bw()+
  scale_fill_brewer(palette="Set1")


box_plot <- uk_trqs %>%
  filter(quota_origin %in% c("Turkey","Iceland","Norway","Morocco","Egypt","Chile")) %>%
  filter(trq_type == "FTA",year=="2021") %>%
  ggplot(
    aes(
      x = quota_fill_rate, 
      y = quota_origin,
      group=quota_origin,
      fill=quota_origin))+
  geom_boxplot(
    stat="boxplot",
    varwidth=TRUE,
    outlier.alpha=0.2,
    outlier.color="grey52")+
  theme_bw()+
  scale_fill_brewer(palette="Pastel2")



# interactive using ggirafe

# create data
df <- uk_trqs %>%
  filter(quota_origin %in% c("Lebanon","South Africa","Iceland","Norway","Morocco","Egypt","Chile")) %>%
  filter(trq_type == "FTA",year=="2021")


box_plot <-
 df %>%
  ggplot(
    aes(
      tooltip = quota_origin, # name tooltip and data_id for ggiraph interactivity 
      data_id = quota_origin,
      x = quota_fill_rate, 
      y = quota_origin,
      group=quota_origin,
      fill=quota_origin))+
  geom_boxplot_interactive(
    stat="boxplot",
    outlier.alpha=0.2,
    outlier.color="grey52"
    )+
  theme_bw()+
  scale_fill_brewer(palette="Set1")


girafe(
  ggobj = box_plot,
  options = list(
    opts_hover(css = ''),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_sizing(rescale = FALSE)
  ),
  height_svg = 7,
  width_svg = 9
)

 
## line chart -----------

# line chart of trade time-series for multiple countries

load("data/Trade_datasets.Rdata")


df <- trade_annual %>%
  filter(iso2 %in% c("DE","FR","IE","CH","NO","SE","NL","BE","ES","LU","FI","DK","PT","RO")) %>%
  mutate(label=paste0("Test",": ",total))



line_plot <-
  df %>%
  ggplot(
    aes(
      x=year,
      y=total,
      color=country,
      data_id = country,
      tooltip = glue::glue("{country} total trade <br> with the UK") # custom tool tip
    )
  )+
  geom_line_interactive(
    size=1
  )+
  theme_light()+
  theme(legend.position = "none")

    
girafe(
  ggobj = line_plot,
     options = list(
       opts_hover(css = ''),
       opts_hover_inv(css = "opacity:0.1;color:grey52"), 
       opts_sizing(rescale = FALSE)
      ),
   height_svg = 7,
   width_svg = 9
 )




## bar chart ------------

# example: world trade stacked bar chart goods vs services (improts and exports)

df <- worldtrade %>% 
  filter(Year>=2016) %>%
  select(-total_export,-total_import) %>%
  pivot_longer(
    cols = goods_export:services_import,
    names_to = "trade_type",
    values_to = "trade_value"
  ) %>%
  mutate(tooltip = paste0(trade_type,": £", scales::comma(trade_value)))

bc <- 
  df %>%
  ggplot(
    aes(
      x=Year,
      y=trade_value,
      fill=trade_type,
      data_id = trade_type,
      tooltip = tooltip
    )
  )+
  geom_bar_interactive(
    stat="identity",position="stack")+
  theme_minimal()


girafe(ggobj = bc,
       options = list(
         opts_hover(css = ''),
         opts_hover_inv(css = "opacity:0.4"),
         opts_sizing(rescale = FALSE)
       ),
       height_svg = 7,
       width_svg = 9
)

       
## scatter plot ---------





## facet_wrap -----------




### formatting -----------


# interactive title / labels 




### interactive scale -----------




#### patchwork example ----------





# 2. custom animation -------------








