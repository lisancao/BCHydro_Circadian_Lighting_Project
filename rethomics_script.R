##-----rethomics installation-----
library(devtools)
#install_github("rethomics/behavr")
#install_github("rethomics/ggetho")
#install_github("rethomics/zeitgebr")
#install_github("rethomics/sleepr")

##-----Load Libraries-----
library(readxl)
library(data.table)
library(ggplot2)
library(behavr)
library(ggetho)
library(zeitgebr)
library(sleepr)

##-----------------Load Dataset---------------------
#load in dataset 
BCHydro_Data <- read_xlsx("~/BC_Hydro_CircRhythms_Project_2018/R_Megasheet_Lisa.xlsx")
#convert to data table
BCHydro_Data <- data.table(BCHydro_Data, key = "ID")
#view
BCHydro_Data
#view column names
names(BCHydro_Data)

##----------------Metafile----------------
#subset data by unique ID, first entry
BCHydro_Meta <- subset(BCHydro_Data, !duplicated(ID))
#Save to CSV file
write.csv(BCHydro_Meta, file = "BCHydro_Meta.csv")
#import metadata
BCHydro_Meta <- fread("BCHydro_Meta.csv")
#set as meta with key
BCHydro_Meta <- data.table(BCHydro_Meta, key = "ID")
#link metadata to main dataset 
BCHydro_Behavr <- behavr(BCHydro_Data, BCHydro_Meta)
#setbehavr(BCHydro_Data, BCHydro_Meta) <-- set as behavr table without copy 
#check if behavr data frame
is.behavr(BCHydro_Behavr)
#view behavr
print(BCHydro_Behavr)
View(BCHydro_Behavr)
summary(BCHydro_Behavr)

##------------SUBSET DATA BY CONDITION-------------
BCHydro_Cond0 <-BCHydro_Behavr[xmv(Condition) == "0"]
BCHydro_Cond1 <- BCHydro_Behavr[xmv(Condition) == "1"]
                         







##################  UNDER CONSTRUCTION #######################

##-----------ggetho visualizations---------------
##Tile Plots 
Tileplot_A <- ggetho(BCHydro_Behavr, aes(x=age, y=ID, z=SO) + 
  (summary_FUN = mean)) +
  stat_tile_etho()

Tileplot_A


##Bar tiles 
#Barplot_A <- (BCHydro_Behavr, aes(x=SO, z=))



##Actograms (RETURNS ERROR!)
actogram_A <- ggetho(BCHydro_Behavr, 
                     time_wrap = hours(24),
                     y = paste(BCHydro_Behavr$Condition, BCHydro_Behavr$ID),
                     z = BCHydro_Behavr$SD)