library(data.table)
library(ggplot2)
library(dplyr)
library(gganimate)
library(devtools)
library(animation)
library(readxl)

work.data = read_excel("R_Megasheet_All.xlsx")

head(work.data)

##################
### CLEAN DATA ###

work.data = work.data %>% 
  select(ID, Date, Days, Condition, Days_arrival, yi, SD, phase_dev, age, gender)

# Create Factors
work.data$Condition = as.factor(work.data$Condition)
work.data$ID = factor(work.data$ID)
work.data$gender = as.factor(work.data$gender)
work.data$Date = as.factor(work.data$Date)

# Remove day 0's
work.data = work.data[-which(work.data$Days ==0),]

View(work.data)

