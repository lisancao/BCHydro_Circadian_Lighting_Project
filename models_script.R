#################
### LIBRARIES ###

library(readxl)
library(lmerTest) #lmer
library(car) # for ANOVA
library(e1071) # for probplot
library(MASS) # boxcox
library(glmmLasso)
library(tidyr)
library(dplyr)
library(psycho)
library(ggplot2)
library(ggcorrplot)

###################
### IMPORT DATA ###

work.data = read_excel("~/BC_Hydro_CircRhythms_Project_2018/megasheet_all_Jan21.xlsx")

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
#####################
### MODEL FITTING ###

# fit simple models
simple1 = lmer(SD ~ Condition + (1|ID),data = work.data) # extremely large p
Anova(simple1, type=3, test.statistic = "F")

simple2 = lmer(phase_dev ~ Condition + (1|ID),data = work.data)
Anova(simple2, type=3, test.statistic = "F")

simple3 = lmer(yi ~ Condition + (1|ID),data = work.data)
Anova(simple3, type=3, test.statistic = "F")

# Make more complex models

model1 = lmer(SD ~ Condition*Days_arrival + age*Condition + gender*Condition + (1|ID),data = work.data)
Anova(model1, type=3, test.statistic = "F") # only age is significant

model2 = lmer(phase_dev ~ Condition*Days_arrival+ age*Condition + gender*Condition + (1|ID),data = work.data)
Anova(model2, type=3, test.statistic = "F")

model3 = lmer(yi ~ Condition*Days_arrival + age*Condition + gender*Condition + (1|ID),data = work.data)
Anova(model3, type=3, test.statistic = "F")

##########################
### VARIABLE SELECTION ###

step(model1) # Nothing is significant
step(model2)
step(model3)

# Fit reduced models
final2 = lmer(phase_dev ~ Condition + Days_arrival + (1 | ID), data = work.data)
Anova(final2, type=3, test.statistic = "F")

plot(final2)

final3 = lmer(yi ~ Condition + Days_arrival +  Condition:Days_arrival + (1 | ID) , data = work.data)
Anova(final3, type=3, test.statistic = "F")


final4 = lmer(SO ~ Condition + Days_arrival +  Condition:Days_arrival + (1 | ID) , data = work.data)
Anova(final4, type=3, test.statistic = "F")


final5 = lmer(SD ~ Condition + Days_arrival +  Condition:Days_arrival + (1 | ID) , data = work.data)
Anova(final5, type=3, test.statistic = "F")

########################
### DIAGNOSTIC PLOTS ###

# Residual plots
plot(predict(final2),residuals(final2))
plot(predict(final3),residuals(final3))
plot(predict(final4),residuals(final4))
plot(predict(final5),residuals(final5))

# qq plots
qqnorm(residuals(final2))
qqnorm(residuals(final3))
qqnorm(residuals(final4))
qqnorm(residuals(final5))

######################
### MODEL ANALYSIS ###         # phase_dev model seems to be the best

# Read this
results <- analyze(final2)
print(results)

analyze(final3)
analyze(final4)
analyze(final5)

View(work.data)

final2_predict <- predict(final2)

final2_predict

plot(final2)


#############################################################
##VISUALIZATION 

vizprep <- expand.grid(Condition=unique(work.data$Condition),
                       days=c(min(work.data$Days_arrival), 
                              max(work.data$Days_arrival)))
plot_final2 <- ggplot(work.data, aes(x=work.data$Days_arrival, y=work.data$phase_dev, colour = Condition)) +
  geom_point(size=3)+
  geom_line(aes(y=predict(final2), group=ID), size="ID") +
  geom_line(data = vizprep, aes(y=predict(final2, level=0, work.data=vizprep, size="Condition")) +
  scale_size_manual(name = "predictions", values = c("ID"=0.5, "condition"=3)) +
  theme_bw(base_size = 22))

plot_final2 


boxplot(work.data$phase_dev ~ work.data$Condition, data = work.data, 
        transition_time(work.data$Days)) 
  


#x=Days Arrival, Y=Sleep Deviation(improvement), colour = Condition))         


lmer(work.data$phase_dev ~ .*.+ -ID -Date+(1|ID) , data = work.data)
plot(data = work.data, phase_dev ~ Days_arrival)
library(purrr)
library(tidyr)
library(ggplot2)
work.data %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") + 
  geom_histogram()
