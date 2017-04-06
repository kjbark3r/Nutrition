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

library(plyr)
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
require(dplyr)
avgday14 <- mignute.avg %>%
  dplyr::select(-Date) %>%
  mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015)) %>%
  subset(Year == 2014) %>%
  group_by(DOY, MigStatus) %>%
  dplyr::summarise(AvgDayDE = mean(AvgDE, na.rm=T),
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
  dplyr::group_by(IndivYr) %>%
  dplyr::summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
  dplyr::ungroup() %>%
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
  dplyr::summarise(nTotal = n(),
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



# avg de exposure by mig status
avgde <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayDE)) +
       geom_boxplot(aes(fill = MigStatus)) +
       labs(title = "Avg Daily DE Exposure")+
       geom_hline(yintercept=2.75)
avgde

# g herbaceous forage exposure by mig status
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
           labs(title = "Home Range Area (ha)")
hra

## home range area ~ migstatus
hrmig <- ggplot(mignute.ndays,
                aes(x = MigRank, y = HRarea)) +
  ylab("Home range size (ha)") +
  xlab("Strength of Migratory Behavior") +
  geom_smooth()
hrmig


## daily nute ~ migstatus
fqmig <- ggplot(avgday.indiv,
                aes(x = MigRank, y = AvgDayDE)) +
  ylab("Average DE (kcal/g)") +
  xlab("Strength of Migratory Behavior") +
  geom_smooth()
fqmig


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
                    stat_smooth(method=loess) +
                    geom_hline(yintercept=2.75)
avgderank

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
  dplyr::summarise(AvgDE = mean(AvgDE)) %>%
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


# freq distn of avgdayde by migstatus
ggplot(data = avgday.indiv) +
  geom_histogram(aes(x = AvgDayDE,
                     y = ..count..,
                     fill = MigStatus))

######################################
####  Actual presentation graphs  ####
######################################

# timeplot DE by day
avgday.date <- avgday %>%
  mutate(Date = as.Date(DOY, origin = "2014-01-01"))
tp <-  ggplot(avgday.date, 
              aes(Date, AvgDayDE, 
                  shape = MigStatus,
                  linetype = MigStatus)) +
              geom_point() +
    geom_smooth(color = "black")+
              geom_hline(yintercept=2.75) +
              labs(x = "", 
                   y = "Available Nutrition (kcal/g of forage)") +
              theme(legend.title=element_blank(),
                    text = element_text(size=12))
tp
ggsave("timeplot.jpg", plot = tp, device = "jpeg",
       dpi = 300)

# avg de exposure by mig status
avgde <- ggplot(data = avgday.indiv, 
       aes(x = MigStatus, y = AvgDayDE)) +
       geom_boxplot(aes(fill = MigStatus)) +
       geom_hline(yintercept=2.75) +
       labs(x = "", y = "Average Nutrition (kcal/g)") +
              theme(legend.position="none",
                    text = element_text(size=20))

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


# n days exposure - adequate/marginal/poor
ad <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nAdequate)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Adequate",
       x = "", y = "# Days Access") +
  theme(legend.position="none",
        text = element_text(size=15),
        axis.text.x = element_text(size = 10)) + 
  ylim(0,50)
marg <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nMarg)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Marginal", x="", y="") +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 10)) + 
  ylim(0,50)
pr <- ggplot(data = mignute.ndays, 
  aes(x = MigStatus, y = nPoor)) +
  geom_boxplot(aes(fill = MigStatus)) +
  labs(title = "Poor", x="", y="") + 
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 10)) + 
  ylim(0,50)
grid.arrange(ad, marg, pr, ncol = 3)

# violinplots - n days exposure - ad/marg/pr
  # new df with abbreviated migstatuses
mignute.ndays.rn <- mignute.ndays
mignute.ndays.rn$MigStatus <- ifelse(
  mignute.ndays.rn$MigStatus == "Resident", "Res",
  ifelse(mignute.ndays.rn$MigStatus == "Intermediate", 
         "Int", "Mig"))
mignute.ndays.rn$MigStatus = factor(
  mignute.ndays.rn$MigStatus,
                        levels = c("Res",
                                   "Int",
                                   "Mig"),
                            ordered = TRUE) 
  # plots
ad <- ggplot(data = mignute.ndays.rn, 
  aes(x = MigStatus, y = nAdequate)) +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Adequate FQ",
       x = "", y = "# Days Access") +
  theme(legend.position="none",
        text = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)
marg <- ggplot(data = mignute.ndays.rn, 
  aes(x = MigStatus, y = nMarg)) +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Marginal FQ", x="", y="") +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)
pr <- ggplot(data = mignute.ndays.rn, 
  aes(x = MigStatus, y = nPoor)) +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Poor FQ", x="", y="") + 
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)
  #plot all together
ndaysplot <- grid.arrange(ad, marg, pr, ncol = 3)
  #export
ggsave("ndaysaccess", plot = ndaysplot, device = "jpeg",
       dpi = 300)

# available nutrition across study area
de14 <- raster("../Vegetation/DE2014.tif")
de15 <- raster("../Vegetation/DE2015.tif")
par(mfrow = c(1,2))
plot(de14, main = " 2014 Forage Quality (kcal/m^2)")
plot(de15, main = "2015 Forage Quality (kcal/m^2)")


# DE by lifeform and phenophase

# read in raw data
de.dat <- read.csv("../Vegetation/DE-bylifeform.csv")
# make it longform
de <- de.dat %>%
  gather(key = "Stage", value = "DE", 
         DEemerg, DEflwr, DEfrt, DEcure) %>%
  rename(LifeForm = Class)
# make phenophase stage names correct
de$Stage <- ifelse(de$Stage == "DEemerg", "Emergent", 
                   ifelse(de$Stage == "DEflwr", "Flowering",
                          ifelse(de$Stage == "DEfrt", "Fruiting",
                                 "Cured"))) # make names pretty
# order stages in temporal order of phenophase
de$Stage <- factor(de$Stage, 
                   levels = c("Emergent", "Flowering",
                               "Fruiting", "Cured"),
                            ordered = TRUE) 
#capitalize lifeform names
de$LifeForm <- paste(toupper(substring(de$LifeForm, 1, 1)), 
                     substring(de$LifeForm, 2), sep = "") #capitalize
# order lifeforms in decreasing order of DE
de$LifeForm <- factor(de$LifeForm, 
                   levels = c("Graminoid", "Forb", "Shrub"),
                            ordered = TRUE) 
# plot, finally
ggplot(data = de, aes(x = Stage, y = DE, fill = LifeForm)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("darkgreen","navy", "tan2")) +
  labs(x = "", y = "Digestibility (kcal)")


# home range area
hra <- ggplot(data = mignute.ndays, 
           aes(x = MigStatus, y = HRarea)) +
           geom_boxplot(aes(fill = MigStatus)) +
    labs(title = "", x="", y=expression(paste(
                        "Home Range Size ( ", 
                         km^2, ")", sep=""))) + 
  theme(legend.position="none",
        text = element_text(size=20))
hra

## home range area ~ migstatus
hrmig <- ggplot(mignute.ndays,
                aes(x = MigRank, y = HRarea)) +
  ylab("Home range size (ha)") +
  xlab("Strength of Migratory Behavior") +
  geom_smooth()
hrmig


## daily nute ~ migrank
fqmig <- ggplot(avgday.indiv,
                aes(x = MigRank, y = AvgDayDE)) +
  labs(x = "Resident                                                   Migrant", 
       y = "Nutrition (kcal/g)") +
  geom_smooth(color = "black")+
  geom_hline(yintercept=2.75) +
  theme(text = element_text(size=20),
        axis.text.x=element_blank()) 
fqmig
ggsave("nute-migrank.jpg", plot = fqmig, device = "jpeg",
       dpi = 480)

## EXPORTS ##

ggsave("nute-migrank", plot = fqmig, device = "jpeg",
       dpi = 480)
ggsave("avgde-migstatus", plot = avgde, device = "jpeg",
       dpi = 480)
ggsave("timeplot-dailyde", plot = tp, device = "jpeg",
       dpi = 480)
ggsave("homerangesize", plot = hra, device = "jpeg",
       dpi = 480)

################~#
####   STATS  ####
################~#

#################################~#
## sample sizes, summaries, etc ####

# elk sample size per year
nmig <- migstatus %>%
      mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015))
count(nmig, Year == 2014)

# number res, int, mig
table(migstatus$MigStatus) 

# summary stats, avg DE per day per migstatus
any(is.na(mignute.avg$AvgDE))
mignute.avg.t <- dplyr::select(mignute.avg, -Date)
sumtab <- ddply(mignute.avg.t, "MigStatus", summarise,
                N = length(AvgDE),
                mean = mean(AvgDE),
                sd = sd(AvgDE),
                se = sd/sqrt(N))
sumtab

# summary stats, ndays adequate per migstatus
# also checking median bc data skewed
any(is.na(mignute.ndays$nAdequate))
sumtab.n <- ddply(mignute.ndays, "MigStatus", summarise,
                N = length(nAdequate),
                mean = mean(nAdequate),
                median = median(nAdequate),
                sd = sd(nAdequate),
                se = sd/sqrt(N))
sumtab.n

# summary stats, ndays irrig ag per migstatus
# also checking median bc data skewed
any(is.na(mignute.ndays$nDaysAg))
sumtab.ag <- ddply(mignute.ndays, "MigStatus", summarise,
                N = length(nDaysAg),
                mean = mean(nDaysAg),
                median = median(nDaysAg),
                sd = sd(nDaysAg),
                se = sd/sqrt(N))
sumtab.ag


# ndays in summer
max(avgday$DOY) - min(avgday$DOY)

# summary stats, avg summer HR area
any(is.na(mignute.ndays$HRarea))
sumtab.hr <- ddply(mignute.ndays, "MigStatus", summarise,
                N = length(HRarea),
                mean = mean(HRarea),
                sd = sd(HRarea),
                se = sd/sqrt(N))
sumtab.hr

#  diet results-  n spp; pct grass/fb/shrub
dat.diet <- read.csv("../Vegetation/NSERP_ForagePlants_Summer.csv")
de.diet <- read.csv("../Vegetation/de-byspecies.csv")
diet <- left_join(dat.diet, de.diet, by = "Species") %>%
  dplyr::select(-c(Genus2, ForagePlant)) %>%
  rename(DE = mean)
#ppns
nrow(subset(diet, LifeForm == "graminoid"))/nrow(diet)
  # graminoid = 0.419
nrow(subset(diet, LifeForm == "forb"))/nrow(diet)
  # forb = 0.440
nrow(subset(diet, LifeForm == "shrub"))/nrow(diet)
  # shrub = 0.086
nrow(subset(diet, LifeForm == "tree"))/nrow(diet)
  # tree = 0.054
nrow(diet)
table(diet$LifeForm)

lffrm <- de.dat %>%
  group_by(Class) %>%
  dplyr::summarise(DE = mean(DEcure, DEemerg, DEflwr, DEfrt)) %>%
  ungroup() %>%
  left_join()

# nutrition results
library(raster)
de2014 <- raster("../Vegetation/de2014.tif")
de2015 <- raster("../Vegetation/de2015.tif")
summary(de2014)
summary(de2015)

##############################################~#
## diffs in FQ exposure per migratory status ####


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
  

###############################################################~#
## diffs in ndays exposure to ea nute category per mig status ####

## EXCELLENT ##

# avg de exposure per day
edfq <- aov(nExc ~ MigStatus, data = mignute.ndays)
summary(edfq)
# significant

# tukey hsd multiple comparison 
edfqt <- TukeyHSD(aov(nExc ~ MigStatus, data = mignute.ndays))
edfqt  
# mig sig less than res & int. Res/int no diff


## GOOD ##

# avg de exposure per day
gdfq <- aov(nGood ~ MigStatus, data = mignute.ndays)
summary(gdfq)
# significant, but only 0.02

# tukey hsd multiple comparison 
gdfqt <- TukeyHSD(aov(nGood ~ MigStatus, data = mignute.ndays))
gdfqt  
# only sig diff is bt migrants and residents


## MARGINAL ##

# avg de exposure per day
mdfq <- aov(nMarg ~ MigStatus, data = mignute.ndays)
summary(mdfq)
#super significant

# tukey hsd multiple comparison 
mdfqt <- TukeyHSD(aov(nMarg ~ MigStatus, data = mignute.ndays))
mdfqt   
# sig diffs mig-res and mig-int. p=0.0508 for int-res


## POOR ##

# avg de exposure per day
pdfq <- aov(nPoor ~ MigStatus, data = mignute.ndays)
summary(pdfq)
#super significant

# tukey hsd multiple comparison 
pdfqt <- TukeyHSD(aov(nPoor ~ MigStatus, data = mignute.ndays))
pdfqt   
# sig diffs mig-res and mig-int. p=0.0508 for int-res


############################################~#
## diffs in abundance per migratory status ####
  
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

#######################################~#
## diffs in ifbf per migratory status ####

# mixed effects anova - lactstat random; migstatus fixed
mx <- lmer(IFBF ~ NewStatus + (1|LactStatus), data = alllac)
mx
confint(mx)

# confidence intervals overlap 0, so no dig diff
#but may have low power to detect diff due to small sample size

# just looking at average IFBF correcting for lact
il <- glm(IFBF ~ LactStatus, data = alllac)
il



#####################################~#
## summarizing ppns r/i/m across years ####
allppns <- migstatus %>%
  summarize(ppnRes = length(which(MigStatus == "Resident"))/n(),
            ppnInt = length(which(MigStatus == "Intermediate"))/n(),
            ppnMig = length(which(MigStatus == "Migrant"))/n()) %>%
  ungroup()

#####################################~#
## diffs migstatus ppns 2014 - 2015 ####

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
  


##############################################~#
## diffs in home range area by mig status ####

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


  #############################################~#
## diffs in fecal nitrogen per mig/nonmig ####

fnt <- t.test(PctN ~ MigStatus, data = fn)
fnt
# insignificant difference


##############################################~#
## summary info about migratory behavior ####

summary(migstatus$VI95)
hist(migstatus$VI95)
summary(migstatus$VI50)

##############################################~#
## DE per each landcover type ####

dedat <- read.csv("../Vegetation/DE-model-data.csv") %>%
  dplyr::select(DE, landcov, class_name)
sub1 <- filter(dedat, landcov == 1)
sub2 <- filter(dedat, landcov == 2)
sub3 <- filter(dedat, landcov == 3)
sub4 <- filter(dedat, landcov == 4)
sub5 <- filter(dedat, landcov == 5)
sub6 <- filter(dedat, landcov == 6)
sub7 <- filter(dedat, landcov == 7)
sub8 <- filter(dedat, landcov == 8)
sub9 <- filter(dedat, landcov == 9)
sub10 <- filter(dedat, landcov == 10)
sub11 <- filter(dedat, landcov == 11)
sub12 <- filter(dedat, landcov == 12)

par(mfrow=c(3,4))
hist(sub1$DE, xlab = "Mesic Forest (Burn >15)")
hist(sub2$DE, xlab = "Dry Forest (Burn >15)")
hist(sub3$DE, xlab = "Grass/Shrub/Open Woodland")
hist(sub4$DE, xlab = "Dry Ag")
hist(sub5$DE, xlab = "Valley Bottom Riparian")
hist(sub6$DE, xlab = "Montane Riparian")
hist(sub7$DE, xlab = "Irrigated Ag")
hist(sub8$DE, xlab = "Dry Forest (Burn 0-5)")
hist(sub9$DE, xlab = "Dry Forest (Burn 6-15)")
hist(sub10$DE, xlab = "Mesic Forest (Burn 0-5)")
hist(sub11$DE, xlab = "Mesic Forest (Burn 6-15)")
hist(sub12$DE, xlab = "Rx Dry Forest (Burn 0-5)")

lctab <- ddply(dedat, "class_name", summarise,
                N = length(DE),
                mean = mean(DE),
               median = median(DE),
                sd = sd(DE),
                se = sd/sqrt(N))
arrange(lctab, desc(median))



#####################################~#
## sig diffs bt de per landcover type ####
lcnute <- aov(DE ~ class_name, data = dedat)
summary(lcnute)



#####################################~#
## use of irrig ag by diff behavs####

table(mignute.ndays$MigStatus, mignute.ndays$nDaysAg)

dag <- aov(nDaysAg ~ MigStatus, data = mignute.ndays)
summary(dag)
# significant

# tukey hsd multiple comparison 
dagt <- TukeyHSD(aov(nDaysAg ~ MigStatus, data = mignute.ndays))
dagt  
# mig sig less than res & int. Res/int no diff


############################################################~##
#### CUTS AND MISC ####

#############################################
## reln bt avgdailyDE and migrank (strength) ##
smoothed = loess(AvgDayDE~MigRank,
                 data = avgday.indiv,
                 model = TRUE)
summary(smoothed)
# oh oops, turns out you can't get an eqn from loess