## Libraries
library(tidyverse)
library(rvest)

food_scrape <- function(restaurant, tbl_sel){
  url <- glue::glue("https://fastfoodnutrition.org/{restaurant}/chart")
  url %>% 
    read_html() %>% 
    html_table() %>% 
    .[tbl_sel] %>% 
    bind_rows() %>% 
    select(-X16) %>%
    set_names(nm = c("item", "calories", "cal_fat", "total_fat", "sat_fat", "trans_fat",
                     "cholesterol", "sodium", "total_carb", "fiber", "sugar", "protein",
                     "vit_a", "vit_c", "calcium")) %>% 
    mutate(restaurant = str_replace(restaurant, "-", " "),
           restaurant = str_to_title(restaurant)) # save restaurant name
} 

mcd_df <- food_scrape("mcdonalds", c(1,2,3,9,19))
cfa_df <- food_scrape("chick-fil-a", c(1,2,8,13))
sonic_df <- food_scrape("sonic", c(1,2,17,18,20))
arbys_df <- food_scrape("arbys", c(1:4,8))
bk_df <- food_scrape("burger-king", c(1:3,7,11:12))
dq_df <- food_scrape("dairy-queen", c(5, 7, 12, 25, 27))
sub_df <- food_scrape("subway", c(1,2,3,4,5,6,7,8,9))
taco_df <- food_scrape("taco-bell", c(1,2,3,4,5,15,18,19,20,22,23,24))

final_df <- bind_rows(mcd_df, cfa_df, sonic_df, arbys_df, bk_df, dq_df, sub_df, taco_df) %>% 
  select(restaurant, everything()) %>% 
  mutate(salad = case_when(str_detect(item, "salad") ~ "Salad",
                           TRUE ~ "Other"))
setwd("~/Documents/tidytuesday_JZ/data/2018-09-04")
final_df %>% write_csv("fastfood_calories.csv")
