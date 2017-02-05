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
library(reshape2)
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
mignute.avg <- read.csv("mig-avgforage.csv") %>%
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

# fecal nitrogen
fn <- read.csv("fecalnitrogen.csv") %>%
  within(MigStatus <- ifelse(MigStatus == "Resident", "Non-Migrant",
                             "Migrant")) %>%
  transform(MigStatus = factor(MigStatus, levels = c("Non-Migrant",
                         "Migrant"), ordered = TRUE)) %>%
  within(Date <- as.POSIXlt(Date, format = "%m/%d/%Y"))
fn$DOY <- fn$Date$yday

# MAKING NEW DATAFRAMES FROM ORIGINAL DATA #

# PER MIG STATUS average forage values per day
avgday <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(DOY, MigStatus) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) 
# same as above but split per year
avgday14 <- mignute.avg %>%
  dplyr::select(-Date) %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  subset(Year == 2014) %>%
  group_by(DOY, MigStatus) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) 
avgday15 <- mignute.avg %>%
  dplyr::select(-Date) %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  subset(Year == 2015) %>%
  group_by(DOY, MigStatus) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
  ungroup() %>%
  mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
                   ifelse(AvgDayDE >= 2.75 & AvgDayDE < 2.9, "Good",
                   ifelse(AvgDayDE > 2.40 & AvgDayDE < 2.75, "Marginal",
                          "Poor")))) 

# PER INDIV average forage values per day
avgday.indiv <- mignute.avg %>%
  dplyr::select(-Date) %>%
  group_by(IndivYr) %>%
  summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
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
  filter(LactStatus == "Yes") %>% 
  arrange(IndivYr) %>% #make 2014 status 1st row for ea indiv
  group_by(AnimalID) %>% #newstatus = 2014 migstatus
  mutate(NewStatus = first(MigStatus)) %>%
  ungroup() %>%
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
  filter(LactStatus == "No") %>% 
  arrange(IndivYr) %>% #make 2014 status 1st row for ea indiv
  group_by(AnimalID) %>% #newstatus = 2014 migstatus
  mutate(NewStatus = first(MigStatus)) %>%
  ungroup() %>%
  dplyr::select(-MigStatus) %>%
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

# avg de exposure by mig rank
avgde <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayDE)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily DE Exposure")+
       geom_hline(yintercept=2.75)
avgde

# g herbaceous forage exposure by mig rank
avggh <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayGHerb)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily Herb Abundance")
avggh

# g shrub forage exposure by mig rank
avggs <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayGShrub)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily Shrub Abundance")
avggs

# total g forage exposure by mig rank
avggf <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayGForage)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily Forage Abundance")
avggf

# all avg daily forage plots together
grid.arrange(avggh, avggs, avggf, nrow=3)

# IFBF - res/intermed/mig 
ifbf <- ggplot(data = mignute.ndays, 
           aes(x = MigStatus, y = IFBF)) +
           geom_boxplot(aes(fill = MigStatus)) +
           labs(title = "IFBF")
ifbf

# fecal nitrogen - mig vs non-mig 
fnp <- ggplot(data = fn,
             aes(x = MigStatus, y = PctN)) +
             geom_boxplot(aes(fill = MigStatus)) +
             labs(title = "Fecal Nitrogen")
fnp

#ifbf + fecal n
grid.arrange(ifbf, fn, nrow=(2))

# home range area
hra <- ggplot(data = mignute.ndays, 
           aes(x = MigStatus, y = HRarea)) +
           geom_boxplot(aes(fill = MigStatus)) +
           labs(title = "Home Range Area (m^2)")
hra

# timeplot DE by day
tp <-  ggplot(avgday, 
              aes(DOY, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.75)
tp

# timeplot DE by day - split by year
tp14 <-  ggplot(data = avgday14, 
              aes(DOY, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.75) + 
              labs(title = "DE 2014")
tp15 <-  ggplot(data = avgday15,
              aes(DOY, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.75) + 
              labs(title = "DE 2015")
grid.arrange(tp14, tp15, nrow = 2)

## avg daily de exposure ~ migratory RANK
ndaysrank <- ggplot(mignute.ndays,
                    aes(MigRank, nAdequate)) +
                    geom_point() +
                    stat_smooth(method=loess)
avgderank <- ggplot(avgday.indiv,
                    aes(MigRank, AvgDayDE)) +
                    geom_point() +
                    stat_smooth(method=loess)
grid.arrange(ndaysrank, avgderank, nrow = 2)

# timeplot FN by day
tpf <-  ggplot(data= fn, 
              aes(DOY, PctN, colour = MigStatus)) +
              geom_line() +
              geom_point() 
tpf


# timeplots FN by day - split by year
##bc analyzed by different labs
tpf14 <- ggplot(data=subset(fn, Year == 2014),
              aes(DOY, PctN, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              labs(title = "Fecal N 2014")
tpf15 <- ggplot(data=subset(fn, Year == 2015),
              aes(DOY, PctN, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              labs(title = "Fecal N 2015")
grid.arrange(tpf14, tpf15, nrow = 2)


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

# abundance, transformed from logged vals back to g
gherb <- raster("../Vegetation/gherb2014.tif")
gshrub <- raster("../Vegetation/gshrub2014.tif")
foo <- function(a, b) {
  newval <- (a+b)/10000
  return(newval)
}
gfor <- overlay(gherb, gshrub, fun = "foo")
plot(gfor, main = "Forage Quantity (g/m^2)")


######################################
####  Actual presentation graphs  ####
######################################

# timeplot DE by day
avgday.date <- avgday %>%
  mutate(Date = as.Date(DOY, origin = "2014-01-01"))
tp <-  ggplot(avgday.date, 
              aes(Date, AvgDayDE, colour = MigStatus)) +
              geom_line() +
              geom_point() +
              geom_hline(yintercept=2.75) +
              labs(title = "Daily Nutritional Exposure",
                   x = "", 
                   y = expression(paste(
                        "Forage Quality (kcal / ", 
                         m^2, ")", sep=""))) +
              theme(legend.title=element_blank())
tp

# avg de exposure by mig status
avgde <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayDE)) +
       geom_boxplot(aes(fill = MigStatus)) +
       geom_hline(yintercept=2.75) +
       labs(title = "Daily Nutritional Exposure",
            x = "", y = expression(paste(
                        "Forage Quality (kcal / ", 
                         m^2, ")", sep=""))) +
              theme(legend.position="none")

avgde

# n days adequate/inadequate exposure by mig status
ad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nAdequate)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Exposure to Adequate Forage Quality",
            x = "", y = "Number of Days Exposure") +
              theme(legend.position="none")

ad
inad <- ggplot(data = mignute.ndays, 
       aes(x = MigStatus, y = nInadequate)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Exposure to Inadequate Forage Quality",
            x = "", y = "Number of Days Exposure") +
              theme(legend.position="none")

inad


# n days exposure - excellent/good/marginal/poor
exc <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nExc)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Excellent",
       x = "", y = "Number of Days Exposure") +
  theme(legend.position="none") + 
  ylim(0,50)
gd <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nGood)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Good", x="", y="") +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  ylim(0,50)
marg <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nMarg)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Marginal", x="", y="") +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  ylim(0,50)
pr <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nPoor)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Poor", x="", y="") + 
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  ylim(0,50)
grid.arrange(exc, gd, marg, pr, ncol = 4)



# available nutrition across study area
de14 <- raster("../Vegetation/DE2014.tif")
de15 <- raster("../Vegetation/DE2015.tif")
par(mfrow = c(1,2))
plot(de14, main = " 2014 Forage Quality (kcal/m^2)")
plot(de15, main = "2015 Forage Quality (kcal/m^2)")






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
  #migrants significantly diff from both
  #residents and intermediates sig diff too, but less so
  
  # holm multiple comparison: ndaysad
  nadh <- pairwise.t.test(x = mignute.ndays$nAdequate, 
                          g = mignute.ndays$MigStatus, 
                          p.adjust.method = "holm")
  nadh
  #migrants significantly diff from both
  #residents and intermediates sig diff too, but less so
 
  # tukey hsd multiple comparison - ndays de
  nadt <- TukeyHSD(aov(nAdequate ~ MigStatus, data = mignute.ndays))
  nadt
  #migrants significantly diff from both
  #residents and intermediates sig diff too, but less so
 
  
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
## between residents and intermediates avg daily exposure
  # so intermediates are more likely on a given day to be exposed to adequate nutrition
  # but they're less likely to be exposed to adequate nutrition for as many days total
  # which makes it seem residents are killing it
  # intermediates are fine but slightly worse
  #and migrants seem to be making the best of a bad situation
  
  
#############################################
## diffs in abundance per migratory status ##
  
## AVG FORAGE ABUNDANCE PER DAY ##

# avg de exposure per day
afa <- aov(AvgDayGForage ~ MigStatus, data = avgday.indiv)
summary(afa)
#sig


  # bonferroni multiple comparison: avgde
  aadb <- pairwise.t.test(x = avgday.indiv$AvgDayGForage, 
                          g = avgday.indiv$MigStatus, 
                          p.adjust.method = "bonf")
  aadb
  #sig diff is mig vs others; res/int the same

  # holm multiple comparison: avgde
  aadh <- pairwise.t.test(x = avgday.indiv$AvgDayGForage, 
                          g = avgday.indiv$MigStatus, 
                          p.adjust.method = "holm")
  aadh
  #only sig diff is mig vs others 

  # tukey hsd multiple comparison - avgde
  aadt <- TukeyHSD(aov(AvgDayGForage ~ MigStatus, data = avgday.indiv))
  aadt
  #only sig diff is mig vs others

# conclusion: migrants are in areas with higher abundance than residents

########################################
## diffs in ifbf per migratory status ##

# mixed effects anova - lactstat random; migstatus fixed
mx <- lmer(IFBF ~ NewStatus + (1|LactStatus), data = alllac)
mx
confint(mx)

# confidence intervals overlap 0, so no dig diff
#but may have low power to detect diff due to small sample size

# just looking at average IFBF correcting for lact
il <- glm(IFBF ~ LactStatus, data = alllac)
il

######################################
## diffs migstatus ppns 2014 - 2015 ##

# compare proportion resident/intermediate/migrant
# in each year

ppns <- migstatus %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  dplyr::select(Year, MigStatus) %>%
  group_by(Year) %>%
  summarize(ppnRes = length(which(MigStatus == "Resident"))/n(),
            ppnInt = length(which(MigStatus == "Intermediate"))/n(),
            ppnMig = length(which(MigStatus == "Migrant"))/n()) %>%
  ungroup()
write.csv(ppns, file = "migstatus-ppns-per-yr.csv", row.names=F)  
  


###############################################
## diffs in home range area by mig status ##

# hr area
hr <- aov(HRarea ~ MigStatus, data = mignute.ndays)
summary(hr)

  # bonferroni multiple comparison: ndaysad
  nadb <- pairwise.t.test(x = mignute.ndays$HRarea, 
                          g = mignute.ndays$MigStatus, 
                          p.adjust.method = "bonf")
  nadb
  #says residents and intermediates the same
  #migrants significantly diff from both
  
  # holm multiple comparison: ndaysad
  nadh <- pairwise.t.test(x = mignute.ndays$HRarea, 
                          g = mignute.ndays$MigStatus, 
                          p.adjust.method = "holm")
  nadh
  #ditto above
  
  # tukey hsd multiple comparison - ndays de
  nadt <- TukeyHSD(aov(HRarea ~ MigStatus, data = mignute.ndays))
  nadt
  #double ditto
  
############################################
## diffs in fecal nitrogen per mig/nonmig ##

fnt <- t.test(PctN ~ MigStatus, data = fn)
fnt
# insignificant difference