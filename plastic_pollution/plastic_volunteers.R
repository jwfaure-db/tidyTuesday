
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Plastic pollution #tidyduesday
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-26
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(ggbump)
library(gghighlight)
library(ggtext)


# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Montserrat ExtraBold"))

theme_update(
  plot.title = element_text(size = 25, hjust = 0.5),
  plot.subtitle = element_markdown(hjust = 0.5),
  plot.caption = element_text(family = "Montserrat Light"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  strip.background = element_rect(color = "#363636", fill = "#474747"),
  plot.background = element_rect(color = "#363636", fill = "#363636"),
  panel.background = element_rect(color = "transparent", fill = "#474747"),
  panel.border  = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_text(color = "#A8A7A7"),
  text = element_text(color = "#A8A7A7"),
  strip.text = element_text(color = "#A8A7A7", size = 12),
  legend.position = "none",
  plot.margin = margin(1, 1, .5, .2, unit = "cm")
)


# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics


# data manipulation -------------------------------------------------------

## obtain population data
pop_data <- world_bank_pop %>% 
  filter(indicator == "SP.POP.TOTL") %>% 
  left_join(who, by = c("country" = "iso3")) %>% 
  filter(year == 2010) %>% 
  select(country.y, `2017`, iso2) %>% 
  rename(country = country.y,
         population = `2017`) %>% 
  mutate(country = str_to_title(country)) %>% 
  mutate(country = case_when(
    country == "United Kingdom Of Great Britain And Northern Ireland" ~ "United Kingdom",
    country == "United Republic Of Tanzania" ~ "Tanzania", 
    country == "Viet Nam" ~ "Vietnam",
    country == "China, Hong Kong Sar" ~ "Hong Kong",
    TRUE ~ country
  ))

## get world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

## check to see what the labeling for countries is like, we can see some are
## all caps, and some have slightly different spelling, need to tidy this up
plastics %>% 
  count(country) %>% 
  pull(country) 

plts <- plastics %>% 
  mutate(country = case_when(
           country == "United Kingdom of Great Britain & Northern Ireland" ~ "United Kingdom",
           country == "Taiwan_ Republic of China (ROC)" ~ "Taiwan",
           country == "Cote D_ivoire" ~ "Cote d'Ivoire",
           TRUE ~ country
           )) %>% 
  mutate(country = str_to_title(country)) %>% 
  filter(country != "Empty", parent_company != "Grand Total") %>% 
  select(country, year, volunteers, grand_total)

## investigate rate of volunteers
vols <- plts %>% 
  group_by(country, year) %>% 
  summarise(vols = max(volunteers)) %>% 
  ungroup() %>% 
  filter(!is.na(vols)) %>% 
  left_join(pop_data) %>% 
  mutate(population = if_else(country == "Taiwan", 23816775, population),
         rate = 100000 * (vols / population)) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-rate))

## investigate rate of plastic pollution
p_pol <- plts %>%  
  group_by(country, year) %>% 
  summarise(total_pollution = sum(grand_total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(total_pollution)) %>% 
  left_join(pop_data) %>% 
  mutate(population = if_else(country == "Taiwan", 23816775, population),
         rate = 100000 * (total_pollution / population)) %>% 
  group_by(country) %>% 
  filter(first(year) != last(year)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(rank = rank(-rate)) %>% 
  ungroup()



# bump plot ---------------------------------------------------------------

p_pol %>% 
  # filter(rank <= 20) %>% 
  ggplot(aes(x = year, y = rank, group = country, color = country)) +
  geom_point(size = 5) + 
  geom_text(data = p_pol %>% filter(year == 2019),
            aes(x = year - .1, label = paste0(country," (",round(rate, 2),")")), 
            hjust = 1) +
  geom_text(data = p_pol %>% filter(year == 2020),
            aes(x = year + .1, label = paste0(country," (",round(rate,2),")")), 
            hjust = 0) +
  geom_bump(smooth = 10, size = 1.3, lineend = "round") +
  gghighlight(country == "Australia",
              use_direct_label = FALSE,
              unhighlighted_params = list(alpha = 0.4)) + 
  scale_color_manual(values = "#81B622") + 
  scale_y_reverse() +
  scale_x_continuous(limits = c(2018.5, 2020.5),
                     breaks = seq(2019, 2020, 1)) +
  labs(
    title = "BREAK FREE FROM PLASTICS",
    subtitle = "HOW <span style='color:#81B622'>AUSTRALIA</span> STACKS UP AGAINST THE REST OF THE WORLD?",
    caption = "\n visualization by Josh Faure  ¦  data: #tidytuesday",
    x = NULL,
    y = NULL
  )


# export plot -------------------------------------------------------------

ggsave("plots/plastic_pollution.png", height = 7.5, width = 10, units = "in")
