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

library(RODBC)
library(ggplot2)
library(adehabitatHR)
library(raster)
library(dplyr)


# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
  wd <- wd_workcomp
} else {
    setwd(wd_laptop)
    wd <- wd_laptop
}
rm(wd_workcomp, wd_laptop)

# DATA #

mignute <- read.csv("mig-nute-cndtn.csv") %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
# PLOTS #

# mig continuum & avg daily GDM
scatter.smooth(mignute$SumGDM ~ I(mignute$VI95*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition")

# boxplots #

#avg daily gdm
ggplot(data = mignute, 
       aes(x = MigStatus, y = AvgGDM)) +
       geom_boxplot(aes(fill = AvgGDM))
#maxfat  
ggplot(data = mignute, 
       aes(x = MigStatus, y = MAXFAT)) +
       geom_boxplot(aes(fill = MAXFAT))
#rump fat
ggplot(data = mignute, 
       aes(x = MigStatus, y = RUMP)) +
       geom_boxplot(aes(fill = RUMP))
#girth
ggplot(data = mignute, 
       aes(x = MigStatus, y = girth)) +
       geom_boxplot(aes(fill = girth))

# STATS #

hist(mignute$SumGDM) # normal enough i think

nut.mod <- lm(SumGDM ~ MigStatus, data = mignute)
summary(nut.mod)
anova(nut.mod)

# in progress...