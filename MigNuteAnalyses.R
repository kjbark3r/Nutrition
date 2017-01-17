###########################################################
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#           -DATA ANALYSES AND VISUALIZATIONS-            #
#                    KRISTIN BARKER                       #
#                       NOV 2016                          #
###########################################################

#################
####  Setup  ####
#################


# PACKAGES #

library(ggplot2) # graphics
library(gridExtra) # >1 plot per display
library(dplyr)


# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
    setwd(wd_laptop)
}
rm(wd_workcomp, wd_laptop)

# DATA #

mignute.avg <- read.csv("mig-avgDE.csv") %>%
  na.omit() %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 

mignute.ndays <- read.csv("mig-ndaysDE.csv")%>%
  na.omit() %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) %>%
  mutate(nExcGood = nExc+nGood)


###################
####  Visuals  ####
###################

# distribution of responses
hist(mignute.avg$AvgDE) # slight L-skew; normal enough
par(mfrow=c(3,1))
hist(mignute.ndays$nExcGood)
hist(mignute.ndays$nMarg)
hist(mignute.ndays$nPoor)


# mig continuum & avg daily DE
scatter.smooth(mignute.avg$AvgDE ~ I(mignute.avg$VI95*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition")
scatter.smooth(mignute.avg$AvgDE ~ I(mignute.avg$VI50*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition")

# mig factor & avgde (prob biologically meaningless)
ggplot(data = mignute.avg, 
       aes(x = MigStatus, y = AvgDE)) +
       geom_boxplot(aes(fill = AvgDE))

# n days exposure - excellent/good/marginal/poor
exc <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nExc)) +
       geom_boxplot(aes(fill = nExc)) +
       labs(title = "Excellent")
gd <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nGood)) +
       geom_boxplot(aes(fill = nGood)) +
       labs(title = "Good")
marg <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nMarg)) +
       geom_boxplot(aes(fill = nMarg)) +
       labs(title = "Marginal")
pr <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nPoor)) +
       geom_boxplot(aes(fill = nPoor)) +
       labs(title = "Poor")
grid.arrange(exc, gd, marg, pr, nrow = 2)

# n days exposure - adequate/marginal/poor
ad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nExcGood)) +
       geom_boxplot(aes(fill = nExcGood)) +
       labs(title = "Adequate Forage Quality",
            x = "", y = "Number of Days Exposure")
mar <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nMarg)) +
       geom_boxplot(aes(fill = nMarg)) +
       labs(title = "Marginal Forage Quality",
            x = "", y = "Number of Days Exposure")
inad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nPoor)) +
       geom_boxplot(aes(fill = nPoor)) +
       labs(title = "Poor Forage Quality",
            x = "", y = "Number of Days Exposure")
grid.arrange(ad, mar, inad, nrow=3)


#################
####  Stats  ####
#################

# adequate forage quality


# marginal forage quality

# poor forage quality
