library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

Spring_elections <- read.csv(here("Spring_total.csv"))
Fall_elections <- read.csv(here("Fall_total_v2.csv"))

#Let's convert the votes and years column to numeric values for Spring and Fall

Spring_elections$Votes <- as.numeric(as.character(Spring_elections$Votes))
Spring_elections$Year <- as.numeric(as.character(Spring_elections$Year))

Fall_elections$Votes <- as.numeric(as.character(Fall_elections$Votes))
Fall_elections$Year <- as.numeric(as.character(Fall_elections$Year))

#Now we need to group by the fall elections and edit some party names
Fall_elections <- Fall_elections %>% 
  replace_na(list(Won = FALSE)) %>% 
  mutate_all(funs(toupper)) %>% 
  mutate(Party = ifelse(Party == "SWAMP PARTY", "SWAMP", Party)) %>% 
  mutate(Party = ifelse(Party == "THE STUDENTS PARTY", "STUDENTS PARTY", Party)) %>% 
  mutate(Won = as.logical(Won)) %>%
  mutate(Election_date = "FALL")

#Now for Spring
Spring_elections <- Spring_elections %>% 
  replace_na(list(Won = FALSE)) %>% 
  mutate(Party = as.character(Party)) %>%
  mutate(Party = ifelse( Party == "The_Students", "Students Party", Party)) %>% 
  mutate(Party = ifelse(Party == "FSP", "Florida Students Party", Party)) %>%
  mutate(Party = ifelse(Party == "Vision_2000" | Party == "Vision_2001", "Vision", Party)) %>% 
  mutate_all(funs(toupper)) %>% 
  mutate(Election_date = "SPRING") %>% 
  mutate(Won = as.logical(Won)) %>% 
  filter(!is.na(Spring_elections$Votes))

#note that Student Party is different that Students Party, which appeared a few years later. 

#let's get establishment vs independent

Est_Fall <- Fall_elections %>% 
  filter(Seat == "DISTRICT A") %>% 
  group_by(Party, Year, Seat) %>% 
  summarise(Seats_won = sum(Won), Candidates = n()) %>% 
  mutate(Est = ifelse(Seats_won > 1, "SYSTEM", "INDEPENDENT")) %>% 
  select(Party, Year, Est)

Est_Spring <- Spring_elections %>% 
  filter(Seat == "BUSINESS") %>% 
  group_by(Party, Year, Seat) %>% 
  summarise(Seats_won = sum(Won), Candidates = n()) %>% 
  mutate(Est = ifelse(Seats_won > 1, "SYSTEM", "INDEPENDENT")) %>% 
  select(Party, Year, Est)

Establishment_total <- rbind(Est_Spring, Est_Fall)

Election_total <- Fall_elections %>% 
  bind_rows(Spring_elections) %>% 
  left_join(Establishment_total, by = c("Party", "Year")) %>% 
  distinct(Seat, Year, Party, First_Name, Last_Name, Votes, .keep_all = TRUE) %>% 
  mutate(Est = ifelse(is.na(Est), "INDEPENDENT", Est))

Check_candidate_totals <- Election_total %>% 
  group_by(Party, Year, Election_date) %>% 
  count(Est)

Party_success_senate <- Election_total %>% 
  filter(Seat != "STUDENT BODY PRESIDENT" & Seat != "TREASURER") %>% 
  group_by(Party, Year, Election_date, Est) %>% 
  summarise(Seats_won = sum(Won), Candidates = n()) %>% 
  mutate(Percent_success =  100 * (Seats_won / Candidates))
  
#At this point, we need to look though the party success file as well as the check candidate totals to make sure everything is correct

ggplot(Party_success_senate, aes(x=Year, y=Seats_won)) + geom_point() + geom_text(label = Party_success_senate$Party)

#Spring vis
ggplot(Party_success_senate[Party_success_senate$Election_date == "SPRING",], aes(x=Year, y=Seats_won, color = Est, size = 1.5)) + geom_point() + ylab("Number of Seats Won") + theme(legend.title=element_blank()) + guides(size=FALSE)

Seat_breakdown <- Fall_elections %>% 
  left_join(Est_Fall, by = c("Party", "Year")) %>% 
  group_by(Seat, Est)

#Let's ensure that all of the candidates are present in the data

Seatswon_Year <- Party_success_senate %>% 
  group_by(Year) %>% 
  summarize(Won = sum(Seats_won))

#This will give us a breakdown of how many seats were won each year, we can cross-refrence this the seats allotted each year
#This will not match up for all years because there we no candidates who ran in certain elections

#Now that the data is all checked we're good to go with analysis!

#Let's sort by who won

Spring_success <- Spring_elections %>%
  filter(Won == "TRUE") 

Fall_success <- Fall_elections %>%
  filter(Won == "TRUE") 


#there are a few things we're going to want to do
#the first is write a function for how long a party has been around

#the general idea is for every year, if the party didn't exist in the previous year make the age 1
#if it did, make the age = age + 1
#I have to go rn, but i'll probably be able to write this function later, shouldn't be too bad.
#i'm sure there are better ways of doing this also, this is just the first thing I thought of 

Year_data <- unique(Election_total[,c('Party', 'Year', 'Election_date')])
Year_data$age <- 1
x <- unique(Election_total$Year)
x <- x[-length(x)]
#my.data.frame <- data[(data$V1 > 2) & (data$V2 < 4), ]
Year_data$age[(Year_data$Year == 2000) & (Year_data$Party == 'VISION') & (Year_data$Election_date == "SPRING")] = 7

for(i in c(2001:2017)){
  print(i)
  if
  
}

#Let's index all unique parties
parties <-unique(Election_total$Party)

#Convert all election years into numeric values so we can utilize that in the function
Election_total$Year <- as.numeric(as.character(Election_total$Year))

#Attempt at loop / function
#Rough idea / need to figure out once it gives T/F values transferring that into new col.
for (i Year in Election_total$Year) {
  ifelse(parties %in% Election_total$Party) {
    mutate(Election_total, Age = )
  } else {
    mutate(Election_total, Age = )
    {
}      
  