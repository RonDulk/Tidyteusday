#The Office - Words and Numbers


# packages ----------------------------------------------------------------


install.packages("janitor")
install.packages("patchwork")
install.packages("ggpubr")
install.packages("delabj")
install.packages("forcats")
install.packages("tidyr")
install.packages("schrute")
install.packages("tibble")
install.packages("randomcoloR")
library(randomcoloR)
library(tibble)
library(schrute)
library(dplyr)
library(psych)
library(tidyverse)
library(magrittr)
library(janitor)
library(patchwork)
library(ggpubr)
library(forcats)
library(tidyr)
library(ggforce)


# Data --------------------------------------------------------------------


# Get the Data
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
tuesdata <- tidytuesdayR::tt_load('2020-03-17')
tuesdata <- tidytuesdayR::tt_load(2020, week = 12)
office_ratings <- tuesdata$office_ratings
office_text <- theoffice


# first -------------------------------------------------------------------


#clean up
a <- aggregate(office_ratings[,"imdb_rating"],list(office_ratings$season),min)
b <- aggregate(office_ratings[,"imdb_rating"],list(office_ratings$season),mean)
c <- aggregate(office_ratings[,"imdb_rating"],list(office_ratings$season),max)
a <- rename(a,imdb.min = "imdb_rating")
a[3] <- b[2]
a <- rename(a,imdb.mean = "imdb_rating")
a[4] <- c[2]
a <- rename(a,imdb.max = "imdb_rating")
a <- rename(a,season = "Group.1")
a[5] <- a[4]-a[2]
a <- rename(a,diff = imdb.max.1)
a$season <- as.factor(a$season)
a

#bad plot
ggplot(office_ratings, aes(season, imdb_rating)) +
       geom_point(size=1) +
       geom_smooth()

ggplot(office_ratings, aes(air_date, imdb_rating)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~season)

#nice plot
p3 <- a %>%
  pivot_longer(cols = -c(season), names_to = "rating_type", values_to = "rating" ) %>%
  filter(rating_type != "diff") %>%
  ggplot(aes(y= season))+
  geom_line(aes(x=rating))+
  geom_point(aes(x=rating, fill=rating_type), size = (4), pch = 21)+
  theme_pubr()+
  theme(legend.position = "None")+
  scale_fill_manual(values= c("#2D8192", '#000000','#762D92'))+
  theme(panel.grid.major.y = element_blank())+
  labs(y=c("Season"), x="IMDB Rating", 
       subtitle = "By Season",
       title = "IMDB Rating Intervals", 
       caption = "Data: The Office\nCreated By: Ron Dulkin")+
  xlim(6,10)
p3


# Second ------------------------------------------------------------------


#clean up
office_text2 <- office_text %>% 
  filter(character == "Michael" | character == "Jim" |
           character == "Andy" | character == "Kevin" |
           character == "Oscar" | character == "Dwight" |
           character == "Pam" | character == "Angela" |
           character == "Erin")
aa  <-  office_text2 %>%  
  group_by(office_text2$season,office_text2$episode_name,office_text2$character) %>% 
  summarize(n())
aa[5] <- 38797
aa
aa = rename(aa,season=1)
aa = rename(aa,episode=2)
aa = rename(aa,char=3)
aa = rename(aa,lines=4)
aa = rename(aa,total=5)
aa

#table with percentage
office_calc <- aa %>%
  filter(!is.na(season)) %>%  # filter NA states
  group_by(season, char) %>% 
  mutate(
    lines = sum(lines),  # calculate total enrollment for every state
    total = sum(total) 
  ) %>% 
  select(-episode) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(
    percentage = round(lines / total, 4), # calculate percentage per state
    perc_square = percentage ^ 2  # square for diversity index
  ) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  mutate(
    di = sum(perc_square),  # calculate diversity index per state
    category_nr = row_number()  # number categories (for ggplot)
  ) %>% 
  ungroup()
office_calc

f = 0.7  # change to change shape of the "balloon"

#coding the shape of baloons
office_shapes <- office_calc %>% 
  rowwise() %>% 
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    x = list(c(0,
               f * percentage * sin(category_nr * 2 * pi / 9 - pi/4),
               percentage * sin(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * percentage * sin(category_nr * 2 * pi / 9 + pi/5),
               0
    )),
    y = list(c(0,
               f * percentage * cos(category_nr * 2 * pi / 9 - pi/5),
               percentage * cos(category_nr * 2 * pi / 9), # real percentage for main "radius"
               f * percentage * cos(category_nr * 2 * pi / 9 + pi/4),
               0
    ))
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(season, di), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_9)

# character colors
pal <- c(randomColor(9))
# Pull characters from the dataset
cat <- office_text2 %>% 
  distinct(character) %>% 
  pull()
# Join colors with characters
pal_df <- data.frame(c = pal, l = cat)
office_flower <- office_shapes %>% 
  filter(di > 0.537 | di < 0.31)

# Plot
ggplot(office_flower) + 
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = season, fill = pal[1]), alpha = 0.7) +
  geom_bspline_closed(aes(x_2, y_2, group = season, fill = pal[2]), alpha = 0.7) +
  geom_bspline_closed(aes(x_3, y_3, group = season, fill = pal[3]), alpha = 0.7) +
  geom_bspline_closed(aes(x_4, y_4, group = season, fill = pal[4]), alpha = 0.7) +
  geom_bspline_closed(aes(x_5, y_5, group = season, fill = pal[5]), alpha = 0.7) +
  geom_bspline_closed(aes(x_6, y_6, group = season, fill = pal[6]), alpha = 0.7) +
  geom_bspline_closed(aes(x_7, y_7, group = season, fill = pal[7]), alpha = 0.7) +
  geom_bspline_closed(aes(x_8, y_8, group = season, fill = pal[8]), alpha = 0.7) +
  geom_bspline_closed(aes(x_9, y_9, group = season, fill = pal[9]), alpha = 0.7) +
  scale_fill_identity(guide = guide_legend(title = "", nrow = 2, override.aes = list(alpha = 0.7, shape = 2)), breaks = pal, labels = pal_df$l) +
  coord_fixed() +
  facet_wrap(vars(season), ncol = 3) +
  labs(title = 'Number of Memorable Quotes of Top Characters "The Office"',
    subtitle = "Showing Distribution By 9 Seasons of The Office ",
    caption = "Source: Officequotes.net | Graphic: Ron Dulkin"
  ) +
  # Theme
  theme_void(base_family = "IBM Plex Sans", base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(family = "IBM Plex Sans Condensed"),
    legend.margin = margin(40, 0, 0, 0),
    plot.background = element_rect(fill = "grey97", colour = NA),
    strip.text.x = element_text(family = "IBM Plex Serif Bold", size = 17, colour = "grey20", margin = margin(0, 0, 10, 0)),
    plot.title = element_text(margin = margin(20, 0, 10, 0), hjust = 0.5, size = 25, family = "IBM Plex Serif Medium"),
    plot.subtitle = element_text(margin = margin(0, 0, 55, 0), hjust = 0.5, size = 18, colour = "grey20"),
    plot.caption = element_text(margin = margin(40, 0, 0, 0), hjust = 0.5, colour = "grey20", family = "IBM Plex Sans Light"),
    plot.margin = margin(20, 20, 35, 20)
  ) +
  ggsave("office-2020(1).png", dpi = 320, width = 15, height = 13.25)
