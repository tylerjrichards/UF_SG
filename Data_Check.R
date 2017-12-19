library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

Spring_elections <- read.csv(here("Spring_total.csv"))
Fall_elections <- read.csv(here("Fall_total.csv"))

head(Spring_elections)

unique(Spring_elections$Party)
#we need to change Vision 2001 and Vision 2000 to just Vision, and remove some empty rows


Spring_elections <- Spring_elections %>% 
  filter(!is.na(Spring_elections$Votes)) %>% 
  mutate(Party = as.character(Party)) %>% 
  mutate(Party = ifelse(Party == "Vision_2000", "Vision", Party)) %>% 
  mutate(Party = ifelse(Party == "Vision_2001", "Vision", Party))


#let's do some checks on the data
Doubles <- Spring_elections[duplicated(Spring_elections[c("First_Name", "Last_Name", "Votes", "Year")]),]

#there was a double in the 2009 election which was a data entry error, I went back and corrected it. There should be no doulbes now

#another check is the number of candidates
Num_Candidates <- Fall_elections %>% 
  group_by(Year, Party, Seat) %>% 
  summarise(Candidates = n())


