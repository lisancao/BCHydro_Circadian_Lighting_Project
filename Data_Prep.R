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
BCHydro_Data <- read_xlsx("~/BC_Hydro_CircRhythms_Project_2018/megasheet_all_Jan21.xlsx")
#convert to dataframe
BCHydro_Data <- data.frame(BCHydro_Data)
#preview data
head(BCHydro_Data)
tail(BCHydro_Data)
#view column names
names(BCHydro_Data)
attach(BCHydro_Data)

View(BCHydro_Data)

# Create Factors
BCHydro_Data$Condition = as.factor(BCHydro_Data$Condition)
BCHydro_Data$ID = factor(BCHydro_Data$ID)
BCHydro_Data$gender = as.factor(BCHydro_Data$gender)
BCHydro_Data$Date = as.factor(BCHydro_Data$Date)

# Remove day 0's
BCHydro_Data = BCHydro_Data[-which(BCHydro_Data$Days ==0),]

#omit NA values
BCHydro_Data <- na.omit(BCHydro_Data)


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

##For each participant by group

#Sleep Duration basic stats
#mean
BCHydro_SDMean <- 
  BCHydro_Grouped %>%
  na.omit() %>%
  mutate(mean_SD = map_dbl(data, ~ mean(.x$SD)))
plot(data = BCHydro_Grouped, BCHydro_SDMean$mean_SD ~ BCHydro_SDMean$Condition, xlab = "Condition", ylab = "Mean Sleep Duration")

#standard deviation
BCHydro_Grouped %>%
  na.omit() %>%
  mutate(sd_SD = map_dbl(data, ~ sd(.x$SD)))


#Sleep deviation basic stats
#mean
BCHydro_phase_devMean <- 
BCHydro_Grouped %>%
  na.omit() %>%
  mutate(mean_phasedev = map_dbl(data, ~ mean(.x$phase_dev)))
plot(data = BCHydro_Grouped, BCHydro_phase_devMean$mean_phasedev~ BCHydro_phase_devMean$Condition, xlab = "Condition", ylab = "Mean Sleep Deviation")

######returning NA 

#standard deviation
BCHydro_Grouped %>%
  na.omit() %>%
  mutate(sd_phasedev = map_dbl(data, ~ sd(.x$phase_dev)))
#######retuning abnormally high NA 


















###SUMMARIES
#subset raw data based on condition <- ### THIS IS NOT SEGREGATED BY ID! 
Cond0_Raw <- subset(BCHydro_Data, Condition == "0", na.rm = TRUE) 
Cond0_Raw
summary(Cond0_Raw)

Cond1_Raw <- subset(BCHydro_Data, Condition == "1", na.rm = TRUE) 
Cond1_Raw
summary(Cond1_Raw)

#variance for Sleep Duration on raw data
var(Cond0_Raw$SD, na.rm = TRUE)
var(Cond1_Raw$SD, na.rm = TRUE)

#variance for Phase Deviation on raw data
var(Cond0_Raw$phase_dev, na.rm = TRUE)
var(Cond1_Raw$phase_dev, na.rm = TRUE)

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

##variance 
#variance 
var(BCHydro_Data$SD, na.rm = TRUE)
var()

var(Cond0_Raw$SD, na.rm = TRUE)
var()

var(Cond1_Raw$SD, na.rm = TRUE)
var()

##summary data for raws split by condition
summary(Cond0_Raw)
summary(Cond1_Raw)

#Linear model (example for mapping)
BCHydro_SumModel <- function(BCHydro_Data) {
  lm(SD ~ Numeric_Date, data = BCHydro_Data, na.action = na.exclude)
}
#map to Grouped Dataset
BCHydro_SumNest <- map(BCHydro_Grouped$data, BCHydro_SumModel, na.action = na.exclude)
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
