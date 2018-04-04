library(readr)
library(reshape2)
Election_data <- read_csv("Cleaned_SG_Election.csv")
Election_data <- Election_data[,-c(1, 10, 11)]





