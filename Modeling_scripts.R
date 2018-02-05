library(ggplot2)
library(tidyverse)
library(readr)
library(caTools)
library(broom)
library(e1071)
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
  filter(Election_date == "SPRING" & Seat != "FORESTRY" & Seat != "NRE" & Est != "NPA" & Seat != "STUDENT BODY PRESIDENT" & Seat != "FORESTRY_NRE") %>% 
  select(-c(X1, First_name, Last_name, Party, Votes, Election_date)) %>% 
  mutate(Won = ifelse(Won == T, "Yes", "No")) %>% 
  mutate(Won = as.factor(Won))
Spring_election_data[is.na(Spring_election_data)] <- 0


set.seed(12345)
split <- sample.split(Spring_election_data$Won, SplitRatio = .8)
Spring_train <- subset(Spring_election_data, split == TRUE)
Spring_test <- subset(Spring_election_data, split == FALSE)
Election_model_Spring <- glm(Won ~ . -Year +Seat*Est -Seat, family = binomial(link = 'logit'), data = Spring_train, na.action = na.omit)
summary(Election_model_Spring)

predict <- predict(Election_model_Spring, newdata = Spring_test, type = 'response')
table(Spring_test$Won, predict > .5)
#this gives us around a ~83% success rate
Spring_model_params <- tidy(Election_model_Spring)

#ok that works, but it doesn't work that well
#let's try classification boundaries instead of logistic regression

#switch to factor
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
library(caret)
svm_Linear <- train(Won ~ . -Year +Seat*Est -Seat, data = Spring_train, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
prediction <- predict(svm_Linear, Spring_test)
confusionMatrix(prediction, Spring_test$Won)

#from this, we get around 81% which is worse

#let's try a random forest, which would make sense here as well
#random forests can't have columns that are character, so we'll transform each character column to factor
library(randomForest)
Spring_election_data[sapply(Spring_election_data, is.character)] <- lapply(Spring_election_data[sapply(Spring_election_data, is.character)],as.factor)
Spring_train <- subset(Spring_election_data, split == TRUE)
Spring_test <- subset(Spring_election_data, split == FALSE)

rf = randomForest(Won ~ . -Year +Seat*Est -Seat,  
                  ntree = 100,
                  data = Spring_train)
plot(rf)
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
Spring_test$predicted = predict(rf, Spring_test)
confusionMatrix(data = Spring_test$predicted, 
                reference = Spring_test$Won)
#this is great! brings it all the way up to 86%
#now let's see what is predicted for this next election

Spring_2018 <- read_csv("2018_Candidates.csv")
Spring_2018[sapply(Spring_2018, is.character)] <- lapply(Spring_2018[sapply(Spring_2018, is.character)],as.factor)
Impact <- Spring_2018 %>% 
  filter(Party == "Impact") %>% 
  select(-c(Party, Year))
Impact$pred <- predict(rf, Impact)
