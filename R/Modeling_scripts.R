library(ggplot2)
library(tidyverse)
library(readr)
library(caTools)
library(broom)
library(e1071)
library(ggthemes)
Election_data <- read_csv("Data/Cleaned_SG_Election.csv")
summary(Election_data)

#Before I start modeling, I usually begin by visualizing the data and then I go from there

election_by_party <- Election_data %>% 
  mutate(Year = as.numeric(Year)) %>% 
  group_by(Year, Party, Est, Election_date) %>% 
  filter(Seat != "STUDENT BODY PRESIDENT" & Seat != "TREASURER") %>% 
  summarise(Votes_avg = mean(Votes),  Candidates_won = sum(Won), Candidates = n(), Percent_won = sum(Won)/n(), Age = mean(Age_Semester))

#plot party success over the years

ggplot(election_by_party, aes(x = Age, y = Percent_won, color = Est, size = .2)) +
  geom_point() + ggtitle("Dominance of System Parties by Year") + 
  guides(size = F) + 
  xlab("Age of Party") + 
  ylab("Winning Percentage") +
  theme_fivethirtyeight()

#plot establishment success over the years

ggplot(election_by_party[election_by_party$Age != 0 & !is.na(election_by_party$Age) & election_by_party$Election_date == "SPRING",], aes(x = Year, y = Percent_won, color = Est, size = .1)) + 
  geom_point() + 
  ggtitle("The System Throughout the 21st Century") + 
  guides(size = F) + xlab("Year") + 
  ylab("Winning Percentage") +
  theme_fivethirtyeight()

Election_data %>% 
  group_by(Est) %>% 
  summarise(M = sum(Won) / n())

#Power ranking by seat

election_by_seat <- Election_data %>% 
  group_by(Seat, Est) %>% 
  summarise(Percent_Won = sum(Won) / n())

library(RColorBrewer)
colourCount <- length(unique(election_by_seat$Seat)) # number of levels
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
ggplot(election_by_seat[(election_by_seat$Est == "INDEPENDENT" & election_by_seat$Seat != "NRE" & election_by_seat$Seat != "FORESTRY" & election_by_seat$Seat != "FORESTRY_NRE"),], aes(x = reorder(Seat, Percent_Won), y = Percent_Won, fill = Seat)) +
  scale_fill_manual(values = getPalette(colourCount)) + 
  geom_col() +
  ggtitle("Power Ranking of Senate Seats for Independent Parties") + xlab("Senate Seat") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  guides(fill = F)
  


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
  filter(Election_date == "SPRING" & Seat != "FORESTRY" & Seat != "NRE" & Est != "NPA" & Seat != "STUDENT BODY PRESIDENT" & Seat != "TREASURER" & Seat != "FORESTRY_NRE") %>% 
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

#let's visualize the parameters to rank the independent seats

Spring_logistic_ind <- Election_model_Spring %>% 
  tidy() %>% 
  filter(grepl("EstIND", term)) %>% 
  mutate(term = sub("\\:.*", "\\:", term)) %>% 
  mutate(term = substr(term, 5, 100)) 

logistic_model_params <- ggplot(Spring_logistic_ind, aes(x = reorder(term, -estimate),  estimate)) + geom_bar(stat="identity") + coord_flip() + ylab("Relative Likelihood of Success: Left = System") + xlab("Seat") + labs(title = "What Seats are Independent Parties Likely to Win? A parameter estimation using Logistic Regression")

#ok that works, but it doesn't work that well
#let's try classification boundaries instead of logistic regression

#switch to factor
library(caret)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
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

rf = randomForest(Won ~ . -Year +Seat*Est -Seat + Est*Age_Semester - Age_Semester,  
                  ntree = 150,
                  data = Spring_election_data)
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
  select(-c(Party)) %>% 
  mutate(Won = NA)

Total_elections <- rbind(Spring_election_data, Impact) #to homogenize the data

Impact_2018_Spring <- Total_elections %>% 
  filter(Est == "SYSTEM" & Year == 2018)
Train_Spring <- Total_elections %>% 
  filter(Year != 2018)
rf_2018 <- randomForest(Won ~ . -Year +Seat*Est -Seat + Est*Age_Semester - Age_Semester,  
                         ntree = 100,
                         data = Train_Spring)
Results_2018 <- data.frame(Seats = Impact_2018_Spring$Seat, Result = predict(rf_2018, Impact_2018_Spring))


svm_Linear <- train(Won ~ . -Year +Seat*Est -Seat, data = Train_Spring, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
Impact_2018_Spring$SVM_prediction <- predict(svm_Linear, Impact_2018_Spring)


test <- Total_elections %>% 
  filter(Year == 2017 & Est == "INDEPENDENT")
predict(rf_2018, test)
  

# my next effort will be to predict the number of votes in an individual seat using a poisson approximator
# I will only do this for the establishment party 



Election_data_Poisson <- Election_data %>% 
  filter(Election_date == "SPRING" & Seat != "FORESTRY" & Seat != "NRE" & Est != "NPA" & Seat != "STUDENT BODY PRESIDENT" & Seat != "TREASURER" & Seat != "FORESTRY_NRE") %>% 
  select(-c(X1, First_name, Last_name, Won, Election_date)) %>% 
  group_by(Seat, Year, Party) %>% 
  summarise(Votes = mean(Votes),
            Est = unique(Est),
            Age_Semester = mean(Age_Semester),
            Slate_Size_Previous = mean(Slate_Size_Previous_Year, na.rm = T),
            Win_Percent_Previous_Year = mean(Win_Percent_Previous_Year),
            Number_of_Opponents_by_Seat = mean(Num_Opponents_by_Seat),
            Num_Parties = mean(Num_Parties))

Election_data_Poisson[is.na(Election_data_Poisson)] <- 0
split <- sample.split(Election_data_Poisson$Votes, SplitRatio = .8)
Poisson_split_train <- subset(Election_data_Poisson, split == TRUE)
Poisson_split_test <- subset(Election_data_Poisson, split == FALSE)
Poisson_model_spring <- glm(Votes ~ . -Year +Seat*Est -Seat -Party, data = Poisson_split_train, family = "poisson")

x <- data.frame(Seat = "ENGINEERING", Est = "SYSTEM", Age_Semester = 1, Slate_Size_Previous = 50, Win_Percent_Previous_Year = .6, Number_of_Opponents_by_Seat = 2, Num_Parties = 2, Year = 2018, Party = "IMPACT")
predict(Poisson_model_spring, x)

#when we do this, we drastically underestimate the number of votes per year because we are comparing two different populations
#need to change the number of votes by looking at the percentage of population

old_data <- read_csv("C:/Users/Tyler/Documents/Data_Projects/SG_Models/sgtotal.csv")
old_data %>% 
  group_by(College, Year) %>% 
  summarise(pop = mean(`Population of area/college`))
