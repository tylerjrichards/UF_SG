library(ggplot2)
library(dplyr)
Election_data <- read.csv("Cleaned_SG_Election.csv")
summary(Election_data)

#Before I start modeling, I usually begin by visualizing the data and then I go from there

election_by_party <- Election_data %>% 
  group_by(Year, Party, Est) %>% 
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












