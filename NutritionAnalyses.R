### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#           -DATA ANALYSES AND VISUALIZATIONS-            #
#                NSERP - KRISTIN BARKER                   #
#                  NOV 2016 - JAN 2017                    #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###


#### packages ####

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2) # graphics
library(reshape2)
library(gridExtra) # >1 plot per display
library(pscl)
library(lme4)


#### working directory ####

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {setwd(wd_laptop)}
rm(wd_workcomp, wd_laptop)




#### original data (from MigNuteData.R) ####


# average DE exposure per indiv per day
mignute.avg <- read.csv("mig-avgforage-GENERALMODEL.csv") %>%
  within(Date <- as.POSIXlt(Date, format = "%Y-%m-%d")) %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
mignute.avg$DOY <- mignute.avg$Date$yday #day of year

# number days exposure to each nutrition category per indiv
mignute.ndays <- read.csv("mig-ndaysDE-GENERALMODEL.csv") %>%
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




# new dataframes from original data #


# PER MIG STATUS average forage values per day
avgday <- mignute.avg %>%
  dplyr::select(-Date) %>%
  dplyr::group_by(DOY, MigStatus) %>%
  dplyr::summarise(AvgDayDE = mean(AvgDE, na.rm=T),
            AvgDayGHerb = mean(AvgGHerb, na.rm=T),
            AvgDayGShrub = mean(AvgGShrub, na.rm=T),
            AvgDayGForage = mean(AvgGForage, na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(DEclass = ifelse(AvgDayDE >= 2.9, "Excellent", 
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



### ### ### ### ###
####  |STATS|  ####
### ### ### ### ###


### ### ### ### ### ### ### ### ### 
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



### ### ### ### ### ### ### ### ### #### ### ### 
## diffs in FQ exposure per migratory status ####


## NDAYS EXPOSURE ##

# ndays exposure adequate fq
nadfq <- aov(nAdequate ~ MigStatus, data = mignute.ndays)
summary(nadfq)
#super significant
#checking pairwise comparisons

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

  # tukey hsd multiple comparison - avgde
  aadt <- TukeyHSD(aov(AvgDayDE ~ MigStatus, data = avgday.indiv))
  aadt
  #says residents and intermediates not significantly diff (barely)
  #migrants significantly diff from both

  

### ### ### ### ### ### ### ### ### #### ### ### ### ### ### ### #
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



### ### ### ### ### ### ### ### ### #### ##
## summarizing ppns r/i/m across years ####
allppns <- migstatus %>%
  summarize(ppnRes = length(which(MigStatus == "Resident"))/n(),
            ppnInt = length(which(MigStatus == "Intermediate"))/n(),
            ppnMig = length(which(MigStatus == "Migrant"))/n()) %>%
  ungroup()



### ### ### ### ### ### ### ### ### #### ### #
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
  

    
### ### ### ### ### ### ### ### ### #### ### 
## summary info about migratory behavior ####

summary(migstatus$VI95)
hist(migstatus$VI95)
summary(migstatus$VI50)



### ### ### ### ### ### ### #
## de per landcover type ####

# [see code below this part for general model version] #
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
write.csv(lctab, "de-by-landcover.csv", row.names=F)


### ### ### ### ### ### ### ### ### #
## use of irrig ag by diff behavs####

table(mignute.ndays$MigStatus, mignute.ndays$nDaysAg)

dag <- aov(nDaysAg ~ MigStatus, data = mignute.ndays)
summary(dag)
# significant

# tukey hsd multiple comparison 
dagt <- TukeyHSD(aov(nDaysAg ~ MigStatus, data = mignute.ndays))
dagt  
# mig sig less than res & int. Res/int no diff





### ### ### ### ### #
####  |VISUALS|  ####
### ### ### ### ### #

##### Violinplots - # Days Exposure ####
## to adequate/marginal/poor FQ
## for residents, intermediates, and migrants

  # dataframe
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


#### timeplot - de by day ####
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
                   y = "Forage quality (kcal/g of forage)") +
              theme(legend.title=element_blank(),
                    text = element_text(size=12))
tp
ggsave("timeplot-bw.jpg", plot = tp, device = "jpeg",
       dpi = 300)