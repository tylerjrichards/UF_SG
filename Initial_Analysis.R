library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

Spring_elections <- read.csv(here("Spring_total.csv"))
Fall_elections <- read.csv(here("Fall_total.csv"))



#Now we need to group by the fall elections and edit some party names
Fall_elections <- Fall_elections %>% 
  replace_na(list(Won = FALSE)) %>% 
  mutate_all(funs(toupper)) %>% 
  mutate(Party = ifelse(Party == "SWAMP PARTY", "SWAMP", Party)) %>% 
  mutate(Party = ifelse(Party == "THE STUDENTS PARTY", "STUDENTS PARTY", Party)) %>% 
  mutate(Won = as.logical(Won))

#note that Student Party is different that Students Party, which appeared a few years later. 

Establishment <- Fall_elections %>% 
  filter(Seat == "DISTRICT A") %>% 
  group_by(Party, Year, Seat) %>% 
  summarise(Seats_won = sum(Won), Candidates = n()) %>% 
  mutate(Est = ifelse(Seats_won > 1, "System", "Independent")) %>% 
  select(Party, Year, Est)


Party_success <- Fall_elections %>% 
  group_by(Party, Year) %>% 
  summarise(Seats_won = sum(Won), Candidates = n()) %>% 
  mutate(Percent_success =  100 * (Seats_won / Candidates)) %>% 
  left_join(Establishment, by = c("Party", "Year"))


ggplot(Party_success, aes(x=Year, y=Seats_won)) + geom_point() + geom_text(label = Party_success$Party)

ggplot(Party_success, aes(x=Year, y=Seats_won, color = Est, size = 1.5)) + geom_point() + ylab("Number of Seats Won") + theme(legend.title=element_blank()) + guides(size=FALSE)

Seat_breakdown <- Fall_elections %>% 
  left_join(Establishment, by = c("Party", "Year")) %>% 
  group_by(Seat, Est)