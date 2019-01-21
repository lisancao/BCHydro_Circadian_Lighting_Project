#load libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(purrr)
library(reshape2)
library(tibble)
library(lubridate)
library(lme4)
library(broom)
library(Gviz)
library(data.table)

##-----------------HOUSEKEEPING---------------------
#load in dataset 
BCHydro_Data <- read_xlsx("~/BC_Hydro_CircRhythms_Project_2018/R_Megasheet_Lisa.xlsx")
#convert to dataframe
BCHydro_Data <- data.frame(BCHydro_Data)
#preview data
head(BCHydro_Data)
tail(BCHydro_Data)
#view column names
names(BCHydro_Data)

attach(BCHydro_Data)

##-------------Date Conversion------------------------
#Lubridate (stores as the number of seconds since 1970-01-01 00:00:00UTC), as_date(), as_datetime(), as_hms()
BCHydro_Timeline <- as_date(BCHydro_Data$Date)
BCHydro_Timeline
##Alternative Method
#BCHydro_Date1 <- as.POSIXct(BCHydro_Data$Date, origin="1970-01-01")
#BCHydro_Date1

#Convert dates to integer with origin 1970-01-01
BCHydro_NumericTime <- as.numeric(as.Date(BCHydro_Data$Date, origin = "1970-01-01"))
BCHydro_NumericTime

#Add in new column called Numeric_Date
BCHydro_Data$Numeric_Date <- BCHydro_NumericTime
str(BCHydro_Data)

##-----------------N/A Value Handling---------------------
#### Missing cases
#show only complete cases
#complete.cases(BCHydro_Data)
#view cases with missing values 
#view_missing <- BCHydro_Data[!complete.cases(BCHydro_Data),]
#new dataset without the missing data 
#BCHydro_NAO <- na.omit(BCHydro_Data)
#view
#BCHydro_NAO

####ignore N/A values in function, useful for later 
#returns TRUE/FALSE for a given column
#is.na(BCHydro_Data$SO)
#omit NA from analysis
#mean(BCHydro_Data$SO, na.rm=TRUE)

##-----------------NEST DATA STRUCTURE---------------------
#group by ID and condition
BCHydro_Grouped <- BCHydro_Data %>% 
  group_by(ID, Condition) %>%
  nest()
#view new dataframe 
BCHydro_Grouped$data[[1]]
BCHydro_Grouped


############UNDER CONSTRUCTION#################
##---------------- STATISTICAL MODELs -------------

###SUMMARIES

##Counts
#Counts Number of data points (days of data) per participant by condition
BCHydro_GroupedIDTally <- BCHydro_Data %>% 
  group_by(ID, Condition) %>%
  tally()
#print
BCHydro_GroupedIDTally

#Counts of unique participants in each condition
BCHydro_ConditionCounts <- subset(BCHydro_Grouped, select = 'Condition')
table(BCHydro_ConditionCounts)
#Counts Condition Datapoints Total
BCHydro_Condition <- c(BCHydro_Data$Condition)
table(BCHydro_Condition)

#summary stats on average # days based on condition
#Condition 0
Cond0 <- subset(BCHydro_Grouped, Condition == "0")
summary(Cond0)
#Condition 1
Cond1 <- subset(BCHydro_Grouped, Condition == "1")
summary(Cond1)


#Linear model (example for mapping)
BCHydro_SumModel <- function(BCHydro_Data) {
  lm(SD ~ Numeric_Date, data = BCHydro_Data, na.action = na.exclude)
}
#map to Grouped Dataset
BCHydro_SumNest <- map(BCHydro_Grouped$data, BCHydro_SumModel)
#view output
BCHydro_SumNest


######====================

BCHydro_NestedModel <- 
  BCHydro_Grouped %>%
  mutate(model = map(BCHydro_Data, BCHydro_SDSumModel)) %>%
  print()

BCHydro_NestedModel

BCHydro_NestedPredict <- 
  BCHydro_NestedModel %>%
  mutate(pred = map2(BCHydro_SDSumModel, BCHydro_Data, predict)) %>%
  print()

###########################################################
###Linear Mixed Effects Models (Multilevel models)
#lme4 application, model 1: Sleep Duration Analysis over condition by days of data collection, variated by ID
BCHydro_ModelSD <- lmer(SD ~ 1 + Condition*Data_Days + (1| ID), data=BCHydro_Data)

#summarize model
summary(BCHydro_ModelSD)


###mapping

##Apply models to each frame using purr::map()
# BCHydro_Models <- map(BCHydro_Grouped$data, custommodel)
