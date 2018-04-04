library(wru)
library(gender)
library(tidyverse)

Election_data <- read_csv("Cleaned_SG_Election.csv")
Election_data <- Election_data %>% 
  select(-c(1,10,11)) %>% 
  rename(surname = Last_name) %>% 
  predict_race(surname.only = TRUE)

Party_avg <- Election_data %>% 
  group_by(Est, Year) %>% 
  summarise(Avg_White = mean(pred.whi)) %>% 
  mutate(Avg_NW = 1 - Avg_White )

Seat_avg <- Election_data %>% 
  group_by(Seat, Year) %>% 
  summarise(Avg_White = mean(pred.whi)) %>% 
  mutate(Avg_NW = 1 - Avg_White )
