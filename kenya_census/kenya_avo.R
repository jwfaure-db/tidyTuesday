
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Kenya Census #tidyduesday
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-19
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(rKenyaCensus)
library(ggtext)
library(Cairo)
library(wesanderson)


# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Oswald"))

theme_update(
  legend.position = "none",
  panel.border = element_blank(),
  plot.title = element_text(face = "bold", size = 18, color = "#A8A7A7"),
  plot.subtitle  = element_text(size = 9, color = '#A8A7A7'),
  plot.caption = element_text(size = 9, color = '#A8A7A7'),
  strip.text = element_text(size = 14, color = "#A8A7A7"),
  strip.background = element_rect(color = "#474747", fill = "#474747"),
  plot.background = element_rect(color = "#363636", fill = "#363636"),
  panel.background = element_rect(fill = "#474747"),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  panel.grid.major = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank()
)


# load data ---------------------------------------------------------------

# Description of different datasets
data("DataCatalogue")

# Shapefiles of Kenya County boundaries from rKenyaCensus dataset
shp <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

# Distribution of population by age, sex and county
pop <- rKenyaCensus::V3_T2.3 %>% 
  janitor::clean_names()

# Distribution of Households Growing Permanent Crops by Type and County
crops <- rKenyaCensus::V4_T2.21 %>% 
  janitor::clean_names()


# data manipulation -------------------------------------------------------

# get pop totals by county
pop_totals <- pop %>% 
  filter(sub_county == "ALL", age == "Total") %>% 
  transmute(
    county = str_replace_all(county, "[[:punct:]]", " ") %>% str_squish(),
    total_population = replace_na(total, 0)
    )

# grab avo numbers and get per 100k rates
avo_crops <- crops %>% 
  transmute(
    county = str_replace_all(sub_county, "[[:punct:]]", " ") %>% str_squish(),
    avocado = replace_na(avocado, 0)
    ) %>% 
  full_join(pop_totals) %>% 
  filter(county != "KENYA") %>% 
  mutate(hh_avo_rate = 100000 * (avocado / total_population))

# clean shp file county names to match
shp <- shp %>% 
  mutate(County = str_replace_all(County, "[[:punct:]]", " ") %>% str_squish())
  
# Join shp and age_grp_pop dataframes
plot_df <- avo_crops %>% 
  left_join(shp, by = c("county" = "County")) %>% 
  st_as_sf()

# get data for subtitle
kenya_pop <- pop_totals %>% summarise(kenya_pop = sum(total_population, na.rm = TRUE))

title_df <- crops %>% 
  transmute(
    county = str_replace_all(sub_county, "[[:punct:]]", " ") %>% str_squish(),
    avocado = replace_na(avocado, 0)
  ) %>% 
  full_join(pop_totals) %>% 
  mutate(total_population = 
           case_when(
             county == "KENYA" ~ as.numeric(kenya_pop),
             TRUE ~ total_population
           )
         ) %>% 
  mutate(hh_avo_rate = 100000 * (avocado / total_population)) %>% 
  filter(hh_avo_rate == max(hh_avo_rate, na.rm = TRUE))

# plot --------------------------------------------------------------------

plot_df %>% 
  ggplot() +
  geom_sf(aes(fill = hh_avo_rate), 
          color = "#474747", size = 0.6) +
  scale_fill_distiller(palette = "YlGn", direction = 1) +
  labs(title = "Farming Avocado in Kenya",
       subtitle = "Kenya has approximately 1 million households that farm avocado in a population of approximately 48 million. Among the 47 counties, Nyamira has the 
       highest rate of households famring avocado, with a rate of 7,158.46 households farming avocado per 100,000 people.",
       caption = "visualization by Josh Faure  ¦  data: #tidytuesday",
       x = NULL,
       y = NULL)


# export plot -------------------------------------------------------------

ggsave("plots/avocado_kenya.png", 
       width = 18, height = 28, units = "cm")
