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

# average DE exposure per indiv per day
mignute.avg <- read.csv("mig-avgDE.csv") %>%
  na.omit() %>%
  within(Date <- as.POSIXlt(Date, format = "%Y-%m-%d")) %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
mignute.avg$DOY <- mignute.avg$Date$yday #day of year

# number days each indiv exposed to excellent/good/marginal/poor 
# and adequate/inadequate forage quality
mignute.ndays <- read.csv("mig-ndaysDE.csv")%>%
  na.omit() %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) %>%
  mutate(nAdequate = nExc+nGood) %>%
  mutate(nInadequate = nMarg+nPoor)

# average DE value per day per migratory status
avgday <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(DOY, MigStatus) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) 

# ppn mres/intermed/mig adequate DE per day
ppn <- mignute.avg %>%
  dplyr::select(-Date) %>% #dplyr hates POSIXlt
  group_by(DOY, MigStatus) %>%
  summarise(nTotal = n(),
            nAd = length(which(AvgDE >= 2.75)),
            nInad = length(which(AvgDE < 2.75)),
            ppnAd = nAd/nTotal,
            ppnInad = nInad/nTotal) %>%
  ungroup()


###################
####  Visuals  ####
###################

# distribution of responses
hist(mignute.avg$AvgDE) # slight L-skew; normal enough
par(mfrow=c(3,1))
hist(mignute.ndays$nAdequate)
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
       aes(x = MigStatus, y = nAdequate)) +
       geom_boxplot(aes(fill = nAdequate)) +
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

# n days exposure - adequate/inadequate
ad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nAdequate)) +
       geom_boxplot(aes(fill = nAdequate)) +
       labs(title = "Adequate Forage Quality",
            x = "", y = "Number of Days Exposure")
inad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nInadequate)) +
       geom_boxplot(aes(fill = nInadequate)) +
       labs(title = "Inadequate Forage Quality",
            x = "", y = "Number of Days Exposure")
grid.arrange(ad, inad, nrow=2)

# body condition - excellent/good/marginal/poor
exc <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = MAXFAT)) +
       geom_boxplot(aes(fill = MAXFAT)) +
       labs(title = "MAXFAT")
gd <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = RUMP)) +
       geom_boxplot(aes(fill = RUMP)) +
       labs(title = "RUMP")
marg <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = Chest)) +
       geom_boxplot(aes(fill = Chest)) +
       labs(title = "Chest")
pr <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = girth)) +
       geom_boxplot(aes(fill = girth)) +
       labs(title = "Girth")
grid.arrange(exc, gd, marg, pr, nrow = 2)

# timeplot DE by day
tp <-  ggplot(avgday, 
              aes(DOY, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.6)

# add hist - ppn res/int/mig with adequate fq per day
tp + geom_bar(data = ppn,
              aes(DOY, y = ppnAd, fill = MigStatus), 
              position = "stack", stat = "identity")
  
  
# hist - ppn res/int/mig with adequate fq per day
ggplot(ppn,
       aes(DOY, y = ppnAd, fill = MigStatus)) +
       geom_bar(position = "stack", stat = "identity")



#################
####  Stats  ####
#################

# adequate forage quality


# marginal forage quality

# poor forage quality
