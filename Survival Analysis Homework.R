library(haven)
library(survival)
library(zip)
library(ggpubr)
library(survminer)
library(flexsurv)
library(visreg)
library(ggplot2)
library(MASS)
#Reading in the dataset
hurricane <- read_sas("hurricane.sas7bdat")

#Create motor_failure variable
hurricane$motor_failure=ifelse(hurricane$reason==2, 1, 0)
table(hurricane$motor_failure, hurricane$reason)

#Need to delete trashrack because there are no motor failures that had trashrack - do not include in the model
hurricane$trashrack <- hurricane$trashrack
table(hurricane$motor_failure, hurricane$trashrack)


#Run a PH model
recid.ph <- coxph(Surv(hour, motor_failure == 1) ~ age + backup + bridgecrane + elevation + gear + servo + slope, data = hurricane)
summary(recid.ph)

#Check if age is linear 
# age  seems fine
visreg(recid.ph, "age", xlab = "Age", ylab = "Partial Residuals", main="Linearity of the Age Variable", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red", level = 0.985) + theme_bw() +
  ggtitle("Linearity of the Age Variable", ) +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(family="serif"))
                                                                    
                                                                    


#Check linear elevation - interval variable so don't need to check
visreg(recid.ph, "elevation", xlab = "Elevation", ylab = "Partial Residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red", level = 0.985) + theme_bw()+
  ggtitle("Linearity of the Elevation Variable", ) +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(family="serif"))

#Check if slope if linear - also only an interval variable
visreg(recid.ph, "slope", xlab = "Slope", ylab = "Partial Residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red", level = 0.985) + theme_bw() + 
  ggtitle("Linearity of the Slope Variable", ) +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(family="serif"))


# Check PH assumption
# PH seems to be met for everything
recid.ph.zph <- cox.zph(recid.ph, transform = "identity")
recid.ph.zph
recid.ph.zph <- cox.zph(recid.ph, transform = "log")
recid.ph.zph


#Backwards selection, remove elevation, gear, backup, bridgecrane, servo (pvalue=.0308)
recid.ph <- coxph(Surv(hour, motor_failure == 1) ~ age + slope, data = hurricane)
summary(recid.ph)


## Created a data set that counted strating and stopping times in SAS
startstop <- read.csv(file='COUNTHOUR.csv')
#PH model on the new data set
new.ph<- coxph(Surv(start, stop, censor2 == 1) ~ age + slope + HYPER2, data = startstop)
summary(new.ph)

#Compare constant run time of >= 12 hours to not using mode age and slope
table(hurricane$age, hurricane$motor_failure)
table(hurricane$slope, hurricane$motor_failure)

#Setting some parameters for age and slope using the modes of these variables
newdata <- data.frame(HYPER2 = c(1, 0), age = 6.2, slope = 2)

#Plotting the Survival Curve
ggsurvplot(survfit(new.ph, newdata), data = newdata, break.y.by = 0.1,
           palette = c("blue", "red"), ylab = "Survival Probability", 
           xlab = "Hour", legend.labs = c("Running for Less than 12 Hours", "Running for 12 Hours or More"), legend.title = "Motor Run Time",
           ggtheme = theme_classic2(base_size=12, base_family = "Times New Roman" ),
           font.family = "Times New Roman" )



  theme(plot.title = element_text(hjust = 0.5), text=element_text(family="serif"))

#Using a new set of parameters for age and slope to compare results 
newdata1 <- data.frame(HYPER2 = c(1, 0), age = 5.8, slope = 2)

#Running Survival Curves with different age and slope

ggsurvplot(survfit(new.ph, newdata1), data = newdata1, break.y.by = 0.1,
           palette = c("purple", "black"), ylab = "Survival Probability",
           xlab = "Hour", legend.labs = c("1", "2"), legend.title = "Subject")

#Using another new set of parameters for age and slope to compare results 
newdata2 <- data.frame(HYPER2 = c(1, 0), age = max(hurricane$age), slope = 1)
ggsurvplot(survfit(new.ph, newdata2), data = newdata2, break.y.by = 0.1,
           palette = c("purple", "black"), ylab = "Survival Probability",
           xlab = "Hour", legend.labs = c("1", "2"), legend.title = "Subject")



#Checking concordance
concordance(new.ph)
