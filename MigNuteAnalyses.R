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
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {setwd(wd_laptop)}
rm(wd_workcomp, wd_laptop)


# ORIGINAL DATA (from MigrationNutrition.R) #

# average DE exposure per indiv per day
mignute.avg <- read.csv("mig-avgDE.csv") %>%
#  na.omit() %>%
  within(Date <- as.POSIXlt(Date, format = "%Y-%m-%d")) %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
mignute.avg$DOY <- mignute.avg$Date$yday #day of year

# number days exposure to each nutrition category per indiv
mignute.ndays <- read.csv("mig-ndaysDE.csv") %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) %>%
  mutate(nAdequate = nExc+nGood) %>%
  mutate(nInadequate = nMarg+nPoor)


# MAKING NEW DATAFRAMES FROM ORIGINAL DATA #

# PER MIG STATUS average DE value per day
avgday <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(DOY, MigStatus) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) 

# PER INDIV average DE value per day
avgday.indiv <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(IndivYr, MigRank) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor"))))

# PER DAY ppn mres/intermed/mig adequate DE
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
par(mfrow=c(2,1))
scatter.smooth(mignute.avg$AvgDE ~ I(mignute.avg$VI95*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition",
               main = "VI95")
scatter.smooth(mignute.avg$AvgDE ~ I(mignute.avg$VI50*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition",
               main = "VI50")

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

# IFBF - res/intermed/mig 
ifbf.nona <- filter(mignute.ndays, !is.na(IFBF))
ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = IFBF)) +
       geom_boxplot(aes(fill = IFBF)) +
       labs(title = "IFBF")

# timeplot DE by day
tp <-  ggplot(avgday, 
              aes(DOY, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.75)
tp

# add hist - ppn res/int/mig with adequate fq per day
tp + geom_bar(data = ppn,
              aes(DOY, y = ppnAd, fill = MigStatus), 
              position = "stack", stat = "identity")

# hist - ppn res/int/mig with adequate fq per day
ggplot(ppn,
       aes(DOY, y = ppnAd, fill = MigStatus)) +
       geom_bar(position = "stack", stat = "identity")

# avg DE by day of plot visit, out of curiosity
plotinfo <- read.csv("../Vegetation/de-plot-summeronly.csv")
plotinfo$DOY <- as.POSIXlt(plotinfo$Date)$yday
scatter.smooth(plotinfo$DE ~ plotinfo$DOY)
  # oh good, DOY alone didn't change DE much

# avg DE per day by elk location
test <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(DOY) %>%
  summarise(AvgDE = mean(AvgDE)) %>%
  ungroup()
scatter.smooth(test$AvgDE ~ test$DOY)
  # ha, quite the selection

# residual plots - avgDE
a.avg <- glm(AvgDE ~ MigStatus, data = mignute.avg)
summary(a)
par(mfrow=c(2,2))
plot(a)

# residual plots - n days exposed to adequate
a.nd <- glm(nAdequate ~ MigStatus, data = mignute.ndays)
summary(a)
par(mfrow=c(2,2))
plot(a)

# nute ~ MigRank
par(mfrow=c(2,1))
scatter.smooth(mignute.avg$AvgDE ~ mignute.avg$MigRank)
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$MigRank)

par(mfrow=c(2,1))
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$MigRank)
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$MigRank)

######################################
####  Actual presentation graphs  ####
######################################

# proportion resident/intermediate/migrant
piedat <- mignute.ndays %>%
  group_by(MigStatus) %>%
  transmute(Prop = n()/nrow(mignute.ndays)) %>%
  ungroup() %>%
  distinct()
pie(piedat$Prop, labels = piedat$Prop)
gpie <- ggplot(piedat, 
               aes(x="", y=Prop, fill=MigStatus)) +
  coord_polar("y", start=0)
#eh screw it, i'll just throw piedat into excel


#################
####  Stats  ####
#################

# anova: adequate DE exposure
adeq <- aov(nAdequate ~ MigStatus, data = mignute.ndays)
adeq


# excellent forage quality
# good forage quality
# marginal forage quality
# poor forage quality
