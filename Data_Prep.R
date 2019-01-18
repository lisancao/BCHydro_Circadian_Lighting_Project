#load libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(purrr)
library(reshape2)
library(tibble)
library(lubridate)
library(lme4)

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
  group_by(ID) %>%
  nest()
#view new dataframe 
BCHydro_Grouped$data[[1]]

BCHydro_Grouped


############UNDER CONSTRUCTION#################
##---------------- STATISTICAL MODELLING -------------

#lme4 application, model 1: Sleep Duration Analysis over condition by days of data collection, variated by ID
BCHydro_Model <- lmer(SD ~ 1 + Condition*Data_Days + (1| ID), data=BCHydro_Data)

#summarize model
summary(BCHydro_Model)


###mapping
play_model <- function(BCHydro_Data){ 
  lmer(SO ~ Numeric_Date + 
         (1 | Chronotype), 
       data = BCHydro_Data)
  }

play_model

summary(play_model)


play_map <- map(BCHydro_Grouped$data, play_model)

names(BCHydro_Data)


##Apply models to each frame using purr::map()
# BCHydro_Models <- map(BCHydro_Grouped$data, custommodel)
