library(ggplot2)
library(tidyverse)
library(readr)
Election_data <- read_csv("Cleaned_SG_Election.csv")
summary(Election_data)

#Before I start modeling, I usually begin by visualizing the data and then I go from there

election_by_party <- Election_data %>% 
  group_by(Year, Party, Est, Election_date) %>% 
  filter(Seat != "STUDENT BODY PRESIDENT" & Seat != "TREASURER") %>% 
  summarise(Votes_avg = mean(Votes),  Candidates_won = sum(Won), Candidates = n(), Percent_won = sum(Won)/n(), Age = mean(Age_Semester))

#plot party success over the years

ggplot(election_by_party, aes(x = Age, y = Percent_won, color = Est, size = .2)) +
  geom_point() + ggtitle("Dominance of System Parties by Year") + guides(size = F) + xlab("Age of Party") + ylab("Winning Percentage")
#Power ranking by seat

election_by_seat <- Election_data %>% 
  group_by(Seat, Est) %>% 
  summarise(Percent_Won = sum(Won) / n())

ggplot(election_by_seat[election_by_seat$Est == "INDEPENDENT",], aes(x = reorder(Seat, Percent_Won), y = Percent_Won)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Power Ranking of Senate Seats") + xlab("Senate Seat")


#Let's start
Election_data$First_Name <- NULL
Election_data$Last_Name <- NULL

Election_data <- Election_data %>% 
  mutate(Est = ifelse(is.na(Age_Semester), "NPA", Est))

#Need to create more variables
#Previous election success
previous_success <- election_by_party %>% 
  select(Est, Year, Party, Candidates, Percent_won, Election_date) %>% 
  filter(Est != "NPA") %>% 
  mutate(Next_Year = ifelse(Election_date == "FALL", Year + 1, Year),
         Next_Election_Date = ifelse(Election_date == "FALL", "SPRING", "FALL")) %>% 
  ungroup() %>% 
  select(-c(Year, Est, Election_date)) %>% 
  rename(Slate_Size_Previous_Year = Candidates, Win_Percent_Previous_Year = Percent_won)

Election_data <- left_join(Election_data, previous_success, by = c("Year" = "Next_Year", "Election_date" = "Next_Election_Date", "Party" = "Party"))

#Number of Parties in Race
Number_of_Parties <- Election_data %>% 
  filter(Party != "INDEPENDENT" & Party != "WRITE-IN") %>% 
  group_by(Year, Election_date) %>% 
  summarise(Num_Parties = n_distinct(Party))

Election_data <- left_join(Election_data, Number_of_Parties, by = c("Year", "Election_date"))

#Number of Parties in seat Race
Number_of_Opponents_by_Seat <- Election_data %>% 
  filter(Party != "WRITE-IN") %>% 
  group_by(Year, Election_date, Seat) %>% 
  summarise(Num_Opponents_by_Seat = n_distinct(Party) - 1)

Election_data <- left_join(Election_data, Number_of_Opponents_by_Seat, by = c("Year", "Election_date", "Seat"))


#we'll start with Spring data
Spring_election_data <- Election_data %>% 
  filter(Election_date == "SPRING") %>% 
  select(-c(X1, First_name, Last_name, Party, Votes, Election_date))


#let's do train and test set
smp_size <- floor(0.75 * nrow(Spring_election_data))

## set the seed to make your partition reproductible
set.seed(12345)
train_ind <- sample(seq_len(nrow(Spring_election_data)), size = smp_size)

#subset(mydata, age >= 20
train <- subset(Spring_election_data, !(Year %in% c(2015,2016, 2017)))
test <- subset(Spring_election_data, Year %in% c(2015, 2016, 2017))

#now let's try a logistic regression model
Election_model <- glm(Won ~ -Year, family = binomial(link = 'logit'), data = train)

library(broom)
tidy(Election_model)

summary(Election_model)

anova(Election_model, test="Chisq")

predict(Election_model, test, type = "response")
#now I can't figure out how to predict things well, will approach again later

