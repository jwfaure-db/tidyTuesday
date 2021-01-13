
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Tate art collections #tidyduesday
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-01-12
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(ggmap)


# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Oswald"))

theme_update(
  panel.background = element_rect(fill = "#17263C"),
  plot.background = element_rect(fill = "#17263C"),
  plot.title = element_text(colour = "#C3ECB2", 
                            size=18, 
                            face="bold",
                            hjust = 0.5),
  plot.subtitle = element_text(colour = "#C3ECB2", 
                               size=9,
                               hjust = 0.5),
  plot.caption = element_text(colour = "#C3ECB2", size=9),
  panel.border = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)


# load data ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tt$artwork
artists <- tt$artists


# data manipulation -------------------------------------------------------

artists %>% count(placeOfBirth, sort = TRUE) # 492 artists with no placeOfBirth

#get more user friendly names for geocoding
artists <- artists %>% 
  mutate(
    placeOfBirth = case_when(
      placeOfBirth == "Shipborne, United Kingdom" ~ "Shipbourne, United Kingdom",
      placeOfBirth == "Piraiévs, Ellás" ~ "Piraeus , Greece",
      placeOfBirth == "Seine-Maritime, Département de la, France" ~ "Seine-Maritime , France",
      placeOfBirth == "Beijing, Zhonghua" ~ "Beijing, China",
      placeOfBirth == "Paimbœeuf, France" ~ "Loire-Atlantique, France",
      placeOfBirth == "Noremburg, Deutschland" ~ "Nuremberg, Germany",
      placeOfBirth == "Soul, Taehan Min'guk" ~ "Seoul, South Korea",
      placeOfBirth == "Baltimore Independent City, United States" ~ "Baltimore, United States",
      placeOfBirth == "Kyongsang-namdo, Taehan Min'guk" ~ "Gyeongsangnam-do, South Korea",
      placeOfBirth == "Tel Aviv, Mehoz, Yisra'el" ~ "Tel Aviv, Israel",
      placeOfBirth == "‘Afula, Yisra'el" ~ "Afula, Israel",
      placeOfBirth == "Quanzhou, Zhonghua" ~ "Quanzhou, China",
      placeOfBirth == "Pakanbaru, Indonesia" ~ "Pekanbaru, Indonesia",
      placeOfBirth == "Šid, Jugoslavija" ~ "Šid, Serbia",
      placeOfBirth == "Taehan Min'guk" ~ "Seoul, South Korea",
      placeOfBirth == "Kirkuk, Al-‘Iraq" ~ "Kirkuk, Iraq",
      placeOfBirth == "Jilin, Zhonghua" ~ "Jilin, China",
      placeOfBirth == "Channel Islands, United Kingdom" ~ "Channel Islands, Jersey",
      placeOfBirth == "Marj ‘Uyun, Al-Lubnan" ~ "Marjaayoun, Lebanon",
      placeOfBirth == "Krung Thep, Prathet Thai" ~ "Krung Thep, Thailand",
      placeOfBirth == "Kowon-up, Choson Minjujuui In'min Konghwaguk" ~ "Kowŏn-ŭp, North Korea",
      TRUE ~ placeOfBirth
    )
  )

#get coordinates of birthplaces
birthplaces <- artists %>% 
  distinct(placeOfBirth)

birthplaces <- geocode_OSM(birthplaces$placeOfBirth, as.data.frame = TRUE)

combined_data <- artists %>% 
  filter(!is.na(placeOfBirth)) %>% 
  inner_join(artwork, by = c("id" = "artistId")) %>% 
  left_join(birthplaces, by = c("placeOfBirth" = "query"))

#counting various features
combined_data %>% count(artist)
combined_data %>% count(artistRole)
combined_data %>% count(placeOfBirth, sort = TRUE)

#manipulate data for plotting
plot_df <- combined_data %>% 
  group_by(artist, placeOfBirth) %>% 
  summarise(
    lon = mean(lon, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    n = n()) %>% 
  ungroup() %>% 
  mutate(n = n %>% as.numeric(),
    pltsize = case_when(
    n < 5 ~ n,
    n < 10 ~ 6,
    n < 50  ~ 7,
    n < 100 ~ 8,
    TRUE ~ 9
  )
  )

#get world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# plot --------------------------------------------------------------------

plot_df %>% 
  ggplot() +
  geom_sf(data = world,
          color = "#D5D8DB", 
          fill = NA) +
  geom_point(aes(x = lon, y = lat, alpha = pltsize),
             colour = "orange",
             show.legend = FALSE) +
  labs(title = "Birthplaces of Artists", 
       subtitle = "Each point represents the birthplace of an artist whose work is owned by Tate, with the transparency \nof the point representing the number of their artworks that are owned by Tate.",
       caption = "visualization by Josh Faure  ¦  data: #tidytuesday",
       x = NULL,
       y = NULL)


# export plot -------------------------------------------------------------

ggsave("plots/artist_map.png", height = 12, width = 18, units = "in")
