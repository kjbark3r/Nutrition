###########################################################
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#           -DATA ANALYSES AND VISUALIZATIONS-            #
#                    KRISTIN BARKER                       #
#                  NOV 2016 - JAN 2017                    #
###########################################################

#################
####  Setup  ####
#################


# PACKAGES #

library(dplyr)
library(tidyr)
library(ggplot2) # graphics
library(gridExtra) # >1 plot per display
library(pscl)
library(lme4)

# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {setwd(wd_laptop)}
rm(wd_workcomp, wd_laptop)


# ORIGINAL DATA (from MigrationNutrition.R) #

# average DE exposure per indiv per day
mignute.avg <- read.csv("mig-avgDE.csv") %>%
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

# to join with new df's created later
migstatus <- read.csv("migstatus.csv")

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
  group_by(IndivYr) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) %>%
  left_join(migstatus, by = "IndivYr")%>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 

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

# data for lac/nonlac comparison
ifbf.lac <- filter(mignute.ndays, !is.na(IFBF),
                   LactStatus == "Yes" | LactStatus == "No")
bod <- read.csv(file = "bodycondition.csv")
  
# LACTATING ELK body condition
lac <- ifbf.lac %>%
  filter(LactStatus == "Yes")%>%
  group_by(AnimalID) %>%
  distinct(MigStatus) %>%
  ungroup() %>%
  mutate(NewStatus = ifelse(AnimalID == 140040, "Migrant", 
                     ifelse(AnimalID == 140120, "Migrant",
                     ifelse(AnimalID == 140400, "Resident",
                     ifelse(AnimalID == 140960, "Resident",
                     ifelse(AnimalID == 141060, "Resident",
                            paste(MigStatus))))))) %>%
  left_join(ifbf.lac) %>%
  dplyr::select(-MigStatus) %>%
  dplyr::select(AnimalID, NewStatus, IFBF) %>%
  distinct() %>%
  transform(NewStatus = factor(NewStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE))

# NON-LACTATING ELK body condition
nolac <- ifbf.lac %>%
  filter(LactStatus == "No")%>%
  group_by(AnimalID) %>%
  distinct(MigStatus) %>%
  ungroup() %>%
  mutate(NewStatus = ifelse(AnimalID == 140050, "Resident", 
                     ifelse(AnimalID == 140060, "Resident",
                     ifelse(AnimalID == 140100, "Resident",
                     ifelse(AnimalID == 140560, "Migrant",
                     ifelse(AnimalID == 140630, "Resident",
                     ifelse(AnimalID == 140710, "Resident",
                     ifelse(AnimalID == 140850, "Migrant",
                     ifelse(AnimalID == 140910, "Migrant",
                     ifelse(AnimalID == 140940, "Resident",
                     ifelse(AnimalID == 140980, "Migrant",
                     ifelse(AnimalID == 141080, "Resident",
                     ifelse(AnimalID == 141100, "Resident",
                     ifelse(AnimalID == 141490, "Resident",
                            paste(MigStatus))))))))))))))) %>%
  dplyr::select(-MigStatus) %>%
  left_join(ifbf.lac, by = "AnimalID") %>%
  dplyr::select(AnimalID, NewStatus, IFBF) %>%
  distinct() %>%
  transform(NewStatus = factor(NewStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE))

alllac <- bind_rows(lac, nolac) %>%
  dplyr::select(-IFBF) %>% #avoid duplicates
  left_join(bod, by = "AnimalID") %>% #add body condition
  dplyr::select(AnimalID, NewStatus, IFBF, preg_pspb, LactStatus) %>%
  filter(LactStatus == "Yes" | LactStatus == "No") %>%
  transform(LactStatus = factor(LactStatus,
                                levels = c("Yes", "No"),
                                ordered = TRUE))

###################
####  Visuals  ####
###################

# distribution of responses
hist(mignute.avg$AvgDE) # slight L-skew; normal enough
par(mfrow=c(3,1))
hist(mignute.ndays$nAdequate)
hist(mignute.ndays$nMarg)
hist(mignute.ndays$nPoor)

# n days exposure - excellent/good/marginal/poor
exc <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nExc)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Excellent")
gd <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nGood)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Good")
marg <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nMarg)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Marginal")
pr <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nPoor)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Poor")
grid.arrange(exc, gd, marg, pr, nrow = 2)

# n days exposure - adequate/inadequate
ad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nAdequate)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Adequate Forage Quality",
            x = "", y = "Number of Days Exposure")
inad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nInadequate)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Inadequate Forage Quality",
            x = "", y = "Number of Days Exposure")
grid.arrange(ad, inad, nrow=2)

# avg de exposure by mig rank, AND
# IFBF - res/intermed/mig 
avgde <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayDE)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily DE Exposure")+
       geom_hline(yintercept=2.75)

ifbf <- ggplot(data = mignute.ndays, 
           aes(x = MigStatus, y = IFBF)) +
           geom_boxplot(aes(fill = MigStatus)) +
           labs(title = "IFBF")
grid.arrange(avgde, ifbf, nrow=2)
 
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

# better nute ~ MigRank
par(mfrow=c(2,1))
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$MigRank)
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$MigRank)

# nute ~ MigRank - nDays vs AvgDailyDE
par(mfrow=c(2,1))
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$MigRank)
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$MigRank)
# basically the same relationship

# nute ~ mign - MigRank vs. VI95 
par(mfrow=c(2,1))
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$MigRank)
scatter.smooth(avgday.indiv$AvgDayDE ~ -(avgday.indiv$VI95))

# same as above but reverse order to check asymptote 
par(mfrow=c(2,1))
scatter.smooth(avgday.indiv$AvgDayDE ~ -(avgday.indiv$MigRank))
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$VI95)

# count vs continuous response (check can use zeroinfl)
par(mfrow=c(2,1))
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$VI95)
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$VI95)
# looks essentially the same, so feel ok about
  # using the count data

# ndaysad - relationship with and without zeros
ndays.no0 <- filter(mignute.ndays, VI95 > 0)
par(mfrow=c(2,1))
scatter.smooth(mignute.ndays$nAdequate ~ mignute.ndays$VI95)
scatter.smooth(ndays.no0$nAdequate ~ ndays.no0$VI95)

# avgde - relationship with and without zeros
avgde.no0 <- filter(avgday.indiv, VI95 > 0)
par(mfrow=c(2,1))
scatter.smooth(avgday.indiv$AvgDayDE ~ avgday.indiv$VI95,
               col = c("red","blue","green")[avgday.indiv$MigStatus])
scatter.smooth(avgde.no0$AvgDayDE ~ avgde.no0$VI95)

scatter.smooth(avgde.no0$AvgDayDE ~ avgde.no0$VI95)
scatter.smooth(log10(avgde.no0$AvgDayDE) ~ avgde.no0$VI95)


# hist of VI95
par(mfrow=c(1,1))
hist(avgday.indiv$VI95)
hist(ndays.no0$VI95)
hist(log10(ndays.no0$VI95))
hist(mignute.ndays$nAdequate)
hist(avgday$AvgDayDE)

# exploring migstatus and age
scatter.smooth(mignute.ndays$VI95 ~ mignute.ndays$Age)
# no obvious relationship

# ifbf - lactators and non-lactators
ifbf.lac <- ggplot(data = lac, 
           aes(x = NewStatus, y = IFBF)) +
           geom_boxplot(aes(fill = NewStatus)) +
           labs(title = "Lactator IFBF")
ifbf.nolac <- ggplot(data = nolac, 
           aes(x = NewStatus, y = IFBF)) +
           geom_boxplot(aes(fill = NewStatus)) +
           labs(title = "Non-lactator IFBF")
grid.arrange(ifbf.lac, ifbf.nolac, nrow=1, ncol=2)

######################################
####  Actual presentation graphs  ####
######################################




#################
####  Stats  ####
#################


###############################################
## diffs in FQ exposure per migratory status ##


## NDAYS EXPOSURE ##

# ndays exposure adequate fq
nadfq <- aov(nAdequate ~ MigStatus, data = mignute.ndays)
summary(nadfq)
#super significant
#checking pairwise comparisons

  # bonferroni multiple comparison: ndaysad
  nadb <- pairwise.t.test(x = mignute.ndays$nAdequate, 
                          g = mignute.ndays$MigStatus, 
                          p.adjust.method = "bonf")
  nadb
  #says residents and intermediates not significantly diff (barely)
  #migrants significantly diff from both
  
  # holm multiple comparison: ndaysad
  nadh <- pairwise.t.test(x = mignute.ndays$nAdequate, 
                          g = mignute.ndays$MigStatus, 
                          p.adjust.method = "holm")
  nadh
  #this says all 3 are different
  
  # tukey hsd multiple comparison - ndays de
  nadt <- TukeyHSD(aov(nAdequate ~ MigStatus, data = mignute.ndays))
  nadt
  #says residents and intermediates not significantly diff (barely)
  #migrants significantly diff from both

  
## AVG DE EXPOSURE PER DAY ##

# avg de exposure per day
aadfq <- aov(AvgDayDE ~ MigStatus, data = avgday.indiv)
summary(aadfq)
#super significant
#checking pairwise comparisons

  # bonferroni multiple comparison: avgde
  aadb <- pairwise.t.test(x = avgday.indiv$AvgDayDE, 
                          g = avgday.indiv$MigStatus, 
                          p.adjust.method = "bonf")
  aadb
  #says residents and intermediates not significantly diff
  #migrants significantly diff from both

  # holm multiple comparison: avgde
  aadh <- pairwise.t.test(x = avgday.indiv$AvgDayDE, 
                          g = avgday.indiv$MigStatus, 
                          p.adjust.method = "holm")
  aadh
  #says all 3 are different 

  # tukey hsd multiple comparison - avgde
  aadt <- TukeyHSD(aov(AvgDayDE ~ MigStatus, data = avgday.indiv))
  aadt
  #says residents and intermediates not significantly diff (barely)
  #migrants significantly diff from both
  

# ok, going with no sig diff (/weak evidence for diff)
## between residents and intermediates
### (due to agreement bt bonferroni and tukey)
# definite significant diff bt migrants and both other grps


########################################
## diffs in ifbf per migratory status ##

# mixed effects anova - lactstat random; migstatus fixed
mx <- lmer(IFBF ~ NewStatus + (1|LactStatus), data = alllac)
mx
confint(mx)

# confidence intervals overlap 0, so no dig diff

# just looking at average IFBF correcting for lact
il <- glm(IFBF ~ LactStatus, data = alllac)

######################################
## diffs migstatus ppns 2014 - 2015 ##

# compare proportion resident/intermediate/migrant
# in each year

ppns <- migstatus %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  select(Year, MigStatus) %>%
  group_by(Year) %>%
  summarize(ppnRes = length(which(MigStatus == "Resident"))/n(),
            ppnInt = length(which(MigStatus == "Intermediate"))/n(),
            ppnMig = length(which(MigStatus == "Migrant"))/n()) %>%
  ungroup()
write.csv(ppns, file = "migstatus-ppns-per-yr.csv", row.names=F)  
  
# these look essentially the same
