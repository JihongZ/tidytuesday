library(tidyverse)
library(ggplot2)

setwd("~/Documents/tidytuesday/codes/")
nfl <- read.csv("../data/2018-08-28/nfl_2010-2017.csv", stringsAsFactors = T, row.names = 1)

head(nfl)
str(nfl)
colnames(nfl)

## strutured data
nfl_yds_yer <- nfl %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  group_by(game_year, team) %>% 
  summarise(rush_yds = mean(rush_yds, na.rm = T),
            pass_yds = mean(pass_yds, na.rm = T),
            rec_yds = mean(pass_yds, na.rm = T),
            ) %>%
  ungroup 

## wide to long
nfl_yds_yer_plot <- nfl_yds_yer %>% 
  gather(key = "yds_average", value = "avg", -c(game_year, team))


## plots
nfl_yds_yer_plot %>% 
  group_by(game_year, yds_average) %>% 
  summarise(avg_sum = mean(avg)) %>% 
  ggplot(mapping = aes(x = game_year, y = avg_sum)) +
  geom_line(aes(color = yds_average)) +
  labs(x = "Year", y = "Average Yds")

nfl_yds_yer_plot %>% 
  mutate_if(is.factor, trimws) %>% 
  filter(nchar(team) == 3) %>% 
  group_by(game_year, team) %>% 
  summarise(avg_sum = sum(avg)) %>% 
  ggplot(mapping = aes(x = game_year, y = avg_sum)) +
  geom_line(aes(color = team)) +
  labs(x = "Year", y = "Average Yds") +
  scale_color_discrete(name="Teams")

