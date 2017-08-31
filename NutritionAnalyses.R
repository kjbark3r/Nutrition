### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#           -DATA ANALYSES AND VISUALIZATIONS-            #
#                NSERP - KRISTIN BARKER                   #
#                   UPDATED JUNE 2017                     #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### ###
####  |SETUP|  ####
### ### ### ### ###




#### packages ####

library(RODBC)
library(reshape2)
library(gridExtra) # >1 plot per display
library(grid) # plot title for grid plot
library(pscl)
library(lme4)
library(MASS) # for negbin model
library(raster)
library(ggplot2) # graphics
library(plyr)
library(dplyr)
library(tidyr)





#### working directories and database connection ####

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Nutrition"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {setwd(wd_laptop)
  } else {
	setwd(wd_worklaptop)
      }
    }
if(file.exists(wd_worklaptop)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kristin/Documents/DatabasesEtc/ForagePlantDatabase.accdb")
} else {  cat("Maybe you shouldn't have been so lazy when you made this code") }
rm(wd_workcomp, wd_laptop, wd_worklaptop)




#### original data (from MigNuteData.R and Vegetation repo) ####


# average DE exposure per indiv per day
mignute.avg <- read.csv("mig-avgforage-GENERALMODEL.csv") %>%
  within(Date <- as.POSIXlt(Date, format = "%Y-%m-%d")) %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
mignute.avg$DOY <- mignute.avg$Date$yday #add day of year


# number days exposure to each nutrition category per indiv
mignute.ndays <- read.csv("mig-ndaysDE-GENERALMODEL.csv") %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) %>%
  mutate(nAdequate = nExc+nGood) %>%
  mutate(nInadequate = nMarg+nPoor)


# continuous and categorical classifications of migratory behavior
migstatus <- read.csv("migstatus.csv")


# DE values from sampled forage plants
de <- sqlQuery(channel, paste("select StudyArea, PlantCode, NameGenus, Stage, Class, DE
                                from Data_DMD where NOT (StudyArea = 'ELKHORNS')"))

# bitterroot valley DE model
de14 <- raster("pred_DE_SUMR_2014.tif")
de15 <- raster("pred_DE_SUMR_2015.tif")


# de per plot
fq.plot <- read.csv("../Vegetation/ALL_DE_data.csv") %>%
  filter(Area == "Nsapph") %>%
  within(Date <- as.Date(Date, format = "%m/%d/%Y"))


# nsapph landcover types
lc14 <- raster("../Vegetation/writtenrasters/uncropped/landcov_14.tif")
lc15 <- raster("../Vegetation/writtenrasters/uncropped/landcov_15.tif")
lcstk <- stack(lc14, lc15)


# landcover definitions
lc.raw <- read.csv("../Vegetation/landcov-classes.csv") 

# projection definition
latlong <- CRS("+init=epsg:4326") # WGS84

# matching extent of DE and landcover rasters using smallest value from both rasters
extent(de14) <- c(197541.9, 284211.9, 143428.1, 302308.1)
extent(lc14) <- extent(de14)
extent(lc15) <- extent(de14)




#### new dataframes from original data ####


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



# DE per landcover type

# combine valley bottom and montane riparian (lots of valley classified as montane)
lc <- lc.raw
lc$class_name <- as.factor(ifelse(grepl("Riparian", lc$class_name), "Riparian", 
                                  as.character(lc$class_name)))


# make plot data spatial, match raster projection, extract landcover
xy.plot <- data.frame("x" = fq.plot$Longitude, "y" = fq.plot$Latitude)
sp.ll.plot <- SpatialPointsDataFrame(xy.plot, fq.plot, proj4string = latlong)
sp.plot <- spTransform(sp.ll.plot, lc14@crs)
ext <- as.data.frame(raster::extract(lcstk, sp.plot))

# select correct year per plot
de.plot <- cbind(fq.plot, ext) %>%
  mutate(Landcov = ifelse(Date < "2015-01-01", landcov_14, landcov_15)) %>%
  select(PlotID, Date, Longitude, Latitude, DE, Landcov) %>%
  left_join(lc, by = c("Landcov" = "landcov")) %>%
  rename(LcNum = Landcov, Landcover = class_name) 
write.csv(de.plot, file = "de-per-plot.csv", row.names=F)




### ### ### ### #### ### ### ### ### #
## |SAMPLE SIZES, SUMMARIES, ETC| ####
### ### ### ### ###### ### ### ### ###


#### elk sample size per year ####
nmig <- migstatus %>%
      mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015))
count(nmig, Year == 2014)
nrow(nmig) # total number of elk-years


#### number and ppn res, int, mig ####
  # total #
table(migstatus$MigStatus) 
length(which(migstatus$MigStatus == "Resident"))/nrow(migstatus)
length(which(migstatus$MigStatus == "Intermediate"))/nrow(migstatus)
length(which(migstatus$MigStatus == "Migrant"))/nrow(migstatus)
  # yearly #
table(nmig$MigStatus, nmig$Year)
10/38 # res&mig, 2014
18/38 # int, 2014
8/37 # res 2015
19/37 # int 2015
10/37 # mig 2015


#### avg summer HR area ####
any(is.na(mignute.ndays$HRarea))
sumtab.hr <- ddply(mignute.ndays, "MigStatus", summarise,
                N = length(HRarea),
                mean = mean(HRarea),
                sd = sd(HRarea),
                se = sd/sqrt(N))
sumtab.hr


#### home range volume intersections #### 
summary(migstatus$VI95)
summary(migstatus$VI50)


#### distance bt centroids ####
summary(migstatus$Dist)
sd(migstatus$Dist)
summary(migstatus$Dist[migstatus$MigStatus == "Migrant"])
sd(migstatus$Dist[migstatus$MigStatus == "Migrant"])
summary(migstatus$Dist[migstatus$MigStatus == "Intermediate"])
sd(migstatus$Dist[migstatus$MigStatus == "Intermediate"])
summary(migstatus$Dist[migstatus$MigStatus == "Resident"])
sd(migstatus$Dist[migstatus$MigStatus == "Resident"])


#### de per lifeform ####

de.lf <- de %>%
  group_by(Class) %>%
  summarise(StDev = sd(DE, na.rm=TRUE), DE = mean(DE, na.rm=TRUE)) %>%
  ungroup()
write.csv(de.lf, "../Vegetation/de-by-lifeform-GENERALMODEL.csv")


#### avg de in measured plots ####
de.ns <- de %>%
  filter(StudyArea == "NS") %>%
  summarise(StDev = sd(DE), Mean = mean(DE))


#### de per landcover type ####
de.lc <- de.plot %>%
  group_by(Landcover) %>%
  summarise(MeanDE = mean(DE, na.rm=TRUE),
            StDev = sd(DE, na.rm=TRUE)) %>%
  ungroup()
write.csv(de.lc, "../Vegetation/de-landcov-NSapph-GENERALMODEL.csv", row.names=F)


#### de per migstatus ####
any(is.na(mignute.avg$AvgDE))
mignute.avg.t <- dplyr::select(mignute.avg, -Date)
sumtab <- ddply(mignute.avg.t, "MigStatus", summarise,
                N = length(AvgDE),
                mean = mean(AvgDE),
                sd = sd(AvgDE),
                se = sd/sqrt(N))
write.csv(sumtab, "de-per-migstatus.csv", row.names=F)





### ### ### ### ### ### ### #
####  |STATS & MODELS|  ####
### ### ### ### ### ### ### #



#### de per landcover type ####

  # set irrigated ag as reference level
  str(de.plot)  # sanity check - make sure factor is unordered
  de.plot$Landcover <- relevel(de.plot$Landcover, "Irrigated Ag")

  # model de per landcover type
  lcmod <- lm(DE ~ Landcover, data = de.plot)
  summary(lcmod)
  par(mfrow=c(2,2)); plot(lcmod)
  
  # extract relevant info for manuscript table/figure
  lctab <- data.frame(cbind(Estimate = coef(lcmod), 
                            confint(lcmod), 
                            p = summary(lcmod)$coefficients[,4])) %>%
    add_rownames("Landcover") %>%
    arrange(desc(Estimate))
  write.csv(lctab, file = "lctab.csv")
  


#### avg daily de per migstatus ####

  # set resident as reference level AND MAKE FACTOR UNORDERED
  avgday.indiv2 <- avgday.indiv %>%
    transform(MigStatus = factor(MigStatus, ordered=F)) 
  avgday.indiv2$MigStatus <- relevel(avgday.indiv2$MigStatus, "Resident")
  demod <- glm(AvgDayDE ~ MigStatus, data = avgday.indiv2)
  summary(demod)  
  par(mfrow=c(2,2)); plot(demod)

  # set intermediate as reference level 
  avgday.indiv3 <- avgday.indiv %>%
    transform(MigStatus = factor(MigStatus, ordered=F)) %>%
    transform(MigStatus = relevel(MigStatus, "Intermediate"))
  demod2 <- glm(AvgDayDE ~ MigStatus, data = avgday.indiv3)
  summary(demod2) 


  
 #### number of days access per migstatus ####
  
  # dataframe with resident as reference level
  ndays <- mignute.ndays %>%
    within(MigStatus <- factor(MigStatus, ordered = FALSE)) %>%
    transform(MigStatus = relevel(MigStatus, "Resident"))

  # dataframe with intermediate as reference level
  ndays2 <- mignute.ndays %>%
    within(MigStatus <- factor(MigStatus, ordered = FALSE)) %>%
    transform(MigStatus = relevel(MigStatus, "Intermediate"))
  
  # dataframe with migrant as reference level
  ndays3 <- mignute.ndays %>%
    within(MigStatus <- factor(MigStatus, ordered = FALSE)) %>%
    transform(MigStatus = relevel(MigStatus, "Migrant"))
  
  
  ## ADEQUATE FQ ##
  
    # determine whether to use poisson or negbin model #
    admod.p <- glm(nAdequate ~ MigStatus, data = ndays, family = poisson(link = log))
    admod.nb <- glm.nb(nAdequate ~ MigStatus, data = ndays)
    pchisq(2 * (logLik(admod.nb) - logLik(admod.p)), df = 1, lower.tail = FALSE)
        # strong significance suggests negbin more appropriate
    summary(admod.nb)
    
    # check whether migstatus is a significant predictor of ndays access
    admod.nbnull <- update(admod.nb, . ~ . - MigStatus)
    anova(admod.nbnull, admod.nb)
      # chi-square indicates migstatus is statistically significant predictor
    
    # now with intermediates as reference level
    admod.nb2 <- glm.nb(nAdequate ~ MigStatus, data = ndays2)
    summary(admod.nb2)
    
    # and migrants as reference level
    admod.nb3 <- glm.nb(nAdequate ~ MigStatus, data = ndays3)
    summary(admod.nb3)

    # back-transformed coefficients for summaries
    (est <- cbind(Estimate = exp(coef(admod.nb)), exp(confint(admod.nb))))
    (est <- cbind(Estimate = exp(coef(admod.nb2)), exp(confint(admod.nb2))))
    (est <- cbind(Estimate = exp(coef(admod.nb3)), exp(confint(admod.nb3))))

    
    
  ## MARGINAL FQ ##
    
    # determine whether to use poisson or negbin model #
    margmod.p <- glm(nMarg ~ MigStatus, data = ndays, family = poisson(link = log))
    margmod.nb <- glm.nb(nMarg ~ MigStatus, data = ndays)
    pchisq(2 * (logLik(margmod.nb) - logLik(margmod.p)), df = 1, lower.tail = FALSE)
      # strong significance suggests negbin more appropriate
    summary(margmod.nb)
    
    # check whether migstatus is a significant predictor of ndays access
    margmod.nbnull <- update(margmod.nb, . ~ . - MigStatus)
    anova(margmod.nbnull, margmod.nb)
      # chi-square indicates migstatus is statistically significant predictor
    
    # now with intermediates as reference level
    margmod.nb2 <- glm.nb(nMarg ~ MigStatus, data = ndays2)
    summary(margmod.nb2)
    
    # and migrants as reference level
    margmod.nb3 <- glm.nb(nMarg ~ MigStatus, data = ndays3)
    summary(margmod.nb3)

    # back-transformed coefficients for summaries
    (est <- cbind(Estimate = exp(coef(margmod.nb)), exp(confint(margmod.nb))))
    (est <- cbind(Estimate = exp(coef(margmod.nb2)), exp(confint(margmod.nb2))))
    (est <- cbind(Estimate = exp(coef(margmod.nb3)), exp(confint(margmod.nb3))))
    

    
  ## POOR FQ ##
    
    # determine whether to use poisson or negbin model #
    prmod.p <- glm(nPoor ~ MigStatus, data = ndays, family = poisson(link = log))
    prmod.nb <- glm.nb(nPoor ~ MigStatus, data = ndays)
    pchisq(2 * (logLik(prmod.nb) - logLik(prmod.p)), df = 1, lower.tail = FALSE)
    # strong significance suggests negbin more appropriate
    summary(prmod.nb)
    
    # check whether migstatus is a significant predictor of ndays access
    prmod.nbnull <- update(prmod.nb, . ~ . - MigStatus)
    anova(prmod.nbnull, prmod.nb)
    # chi-square indicates migstatus is statistically significant predictor
    
    # now with intermediates as reference level
    prmod.nb2 <- glm.nb(nPoor ~ MigStatus, data = ndays2)
    summary(prmod.nb2)
    
    # and migrants as reference level
    prmod.nb3 <- glm.nb(nPoor ~ MigStatus, data = ndays3)
    summary(prmod.nb3)
    
    # back-transformed coefficients for summaries
    (est <- cbind(Estimate = exp(coef(prmod.nb)), exp(confint(prmod.nb))))
    (est <- cbind(Estimate = exp(coef(prmod.nb2)), exp(confint(prmod.nb2))))
    (est <- cbind(Estimate = exp(coef(prmod.nb3)), exp(confint(prmod.nb3))))    
    
    
        
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






## comparing counts per group ####

ndays <- mignute.ndays %>%
  within(MigStatus <- factor(MigStatus, ordered = FALSE))

## AD ##

# first try poisson model #
nad <- glm(nAdequate ~ MigStatus, data = ndays, family = poisson(link = log))
nad
summary(nad)

# check for issues with using this model type
# using chi-square test to compare residual (unexplained) deviance
# to a model where residuals align well with model assumptions
with(nad, cbind(res.deviance = deviance, df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# probably overdispersed based on this significant result
# and the fact that residual deviance:DOF >>>1 (641:72)


nad2 <- glm.nb(nAdequate ~ MigStatus, data = ndays)
nad2
summary(nad2)
85/72 # seems to have handled overdispersion issue well

# check whether migstatus is a significant predictor of ndays access
nad2.1 <- update(nad2, . ~ . - MigStatus)
anova(nad2, nad2.1)

# compare negbin to poisson to verify data better meets negbin assumptions
pchisq(2 * (logLik(nad2) - logLik(nad)), df = 1, lower.tail = FALSE)
# strong significance suggests negbin more appropriate

# CIs for negbin
est <- cbind(Estimate = exp(coef(nad2)), exp(confint(nad2)))
est
# note exponentiated vals 
est.notexp <- cbind(Estimate = coef(nad2), confint(nad2))
est.notexp

# now with intermediates as reference level
# to get sig values (or not) bt mig and int
ndays.int <- ndays
#ndays.int$MigStatus <- factor(ndays.int$MigStatus, ordered = TRUE)
ndays.int$MigStatus <- relevel(ndays.int$MigStatus, ref = "Intermediate")
str(ndays.int)

nad2.int <- glm.nb(nAdequate ~ MigStatus, data = ndays.int)
nad2.int
summary(nad2.int)



## MARG

# first try poisson model #
nm <- glm(nMarg ~ MigStatus, data = ndays, family = poisson(link = log))
nm
summary(nm)

# check for issues with using this model type
# using chi-square test to compare residual (unexplained) deviance
# to a model where residuals align well with model assumptions
with(nm, cbind(res.deviance = deviance, df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# probably overdispersed based on this significant result
# and the fact that residual deviance:DOF >>>1 (641:72)


nm2 <- glm.nb(nMarg ~ MigStatus, data = ndays)
nm2
summary(nm2)
85/72 # seems to have handled overdispersion issue well

# check whether migstatus is a significant predictor of ndays access
nm2.1 <- update(nm2, . ~ . - MigStatus)
anova(nm2, nm2.1)

# compare negbin to poisson to verify data better meets negbin assumptions
pchisq(2 * (logLik(nm2) - logLik(nm)), df = 1, lower.tail = FALSE)
# strong significance suggests negbin more appropriate

# CIs for negbin
estm <- cbind(Estimate = exp(coef(nm2)), exp(confint(nm2)))
estm
# note exponentiated vals 

## intermediate as referenc elevel
nm2.int <- glm.nb(nMarg~MigStatus, data = ndays.int)
summary(nm2.int)
(estm.int <- cbind(Estimate = exp(coef(nm2.int)), exp(confint(nm2.int))))


## POOR 


# first try poisson model #
np <- glm(nPoor ~ MigStatus, data = ndays, family = poisson(link = log))
np
summary(np)

# check for issues with using this model type
# using chi-square test to compare residual (unexplained) deviance
# to a model where residuals align well with model assumptions
with(np, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# probably overdispersed based on this significant result
# and the fact that residual deviance:DOF >>>1 (641:72)


np2 <- glm.nb(nPoor ~ MigStatus, data = ndays)
np2
summary(np2)
54/72 # hm, underdispersed?

# compare negbin to poisson to verify data better meets negbin assumptions
pchisq(2 * (logLik(np2) - logLik(np)), df = 1, lower.tail = FALSE)
# ok, strong significance suggests negbin more appropriate


# check whether migstatus is a significant predictor of ndays access
np2.1 <- update(np2, . ~ . - MigStatus)
anova(np2, np2.1)
# coool, it's not

# CIs for negbin
estp <- cbind(Estimate = exp(coef(np2)), exp(confint(np2)))
estp
# note exponentiated vals 




#### diffs in FQ exposure per migratory status ####


## AVG DE EXPOSURE PER DAY ##

# avg de exposure per day
aadfq <- aov(AvgDayDE ~ MigStatus, data = avgday.indiv)
summary(aadfq)
#very significant

  # tukey hsd multiple comparison - avgde
  aadt <- TukeyHSD(aov(AvgDayDE ~ MigStatus, data = avgday.indiv))
  aadt
  #says residents and intermediates not significantly diff (barely)
  #migrants significantly diff from both
  



## NDAYS EXPOSURE ##

# adequate fq
nadfq <- aov(nAdequate ~ MigStatus, data = mignute.ndays)
summary(nadfq)
#very significant

  # tukey hsd multiple comparison - ndays de
  nadt <- TukeyHSD(aov(nAdequate ~ MigStatus, data = mignute.ndays))
  nadt
  #migrants significantly diff from both
  #residents and intermediates on the cusp


# marginal fq
mdfq <- aov(nMarg ~ MigStatus, data = mignute.ndays)
summary(mdfq)
# significant

# tukey hsd multiple comparison 
mdfqt <- TukeyHSD(aov(nMarg ~ MigStatus, data = mignute.ndays))
mdfqt   



## POOR ##

# avg de exposure per day
pdfq <- aov(nPoor ~ MigStatus, data = mignute.ndays)
summary(pdfq)
#super significant

# tukey hsd multiple comparison 
pdfqt <- TukeyHSD(aov(nPoor ~ MigStatus, data = mignute.ndays))
pdfqt   
# sig diffs mig-res and mig-int. p=0.0508 for int-res





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






### ### ### ### ### #
####  |VISUALS|  ####
### ### ### ### ### #

wcol = 85
wpg = 180


##### Violinplots - # Days Exposure [manu fig.1] ####
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
  aes(x = MigStatus, y = nAdequate))  +
  theme_bw() +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Adequate",
       x = "", y = "Number of days access") +
  theme(legend.position="none",
        text = element_text(size=15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 15)) + 
  ylim(0,50)
marg <- ggplot(data = mignute.ndays.rn, 
  aes(x = MigStatus, y = nMarg))  +
  theme_bw() +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Marginal", x="", y="") +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)
pr <- ggplot(data = mignute.ndays.rn, 
  aes(x = MigStatus, y = nPoor))  +
  theme_bw() +
  geom_violin(fill="grey") +
  geom_boxplot(width=.1, outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", 
               fill="black", shape=21, size=2.5) +
  labs(title = "Poor", x="", y="") + 
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5)) + 
  ylim(0,50)
  #create plot title
  ndaystitle <- textGrob("Forage Quality", 
                         gp=gpar(fontsize=19))
  #plot all together with title
ndaysplot <- grid.arrange(ad, marg, pr, ncol = 3,
                          top = ndaystitle)
  #export
ggsave("ndaysaccess.jpg", 
       plot = ndaysplot, 
       device = "jpeg",
       dpi = 300,
       units = "mm",
       width = wpg,
       height = wcol)



#### timeplot - de by day [manu fig.2] ####
avgday.date <- avgday %>%
  mutate(Date = as.Date(DOY, origin = "2014-01-01"))
tp <-  ggplot(avgday.date, 
              aes(Date, AvgDayDE, 
                  shape = MigStatus,
                  linetype = MigStatus)) +
              geom_point() +
  theme_bw() +
  geom_smooth(color = "black") +
            geom_hline(yintercept=2.75) +
            labs(x = "", 
                 y = "Forage quality \n(kcal/g of forage)") +
            theme(legend.title=element_blank(),
                  text = element_text(size=20)) +
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         shape = guide_legend(override.aes = list(linetype = 0,
                                                  size = 5)))
tp
ggsave("timeplot-bw.jpg", 
       plot = tp, 
       device = "jpeg",
       dpi = 300,
       units = "mm",
       width = wpg,
       height = wcol)

#### daily nute ~ mig continuum [manu fig.3] ####
fqmig <- ggplot(avgday.indiv,
                aes(x = MigRank, y = AvgDayDE)) +
  theme_bw() +
  labs(x = "Resident                                                   Migrant", 
       y = "Forage quality (kcal/g)") +
  geom_smooth(color = "black")+
  geom_point() +
  geom_hline(yintercept=2.75) +
  theme(text = element_text(size=20),
        axis.text.x=element_blank())
fqmig
ggsave("nute-continuum.jpg", 
       plot = fqmig, 
       device = "jpeg",
       dpi = 300,
       units = "mm",
       width = wpg,
       height = wcol)


#### DE by landcover [manu fig.4] ####
de.lc <- de.plot %>%
  transform(Landcover = ifelse(Landcover == "Irrigated Ag",
                               "Irrigated agriculture", 
                               ifelse(Landcover == "Rx Dry Forest Burn 0-5",
                                      "Dry forest, prescribed burn 0-5 yrs ago",
                                      ifelse(Landcover == "Dry Forest Burn 0-5",
                                             "Dry forest, burn 0-5 yrs ago",
                                             ifelse(Landcover == "Dry Ag",
                                                    "Non-irrigated agriculture",
                                                    ifelse(Landcover == "Mesic Forest Burn 0-5",
                                                           "Wet forest, burn 0-5 yrs ago",
                                                           ifelse(Landcover == "Mesic Forest Burn 6-15",
                                                                  "Wet forest, burn 6-15 yrs ago",
                                                                  ifelse(Landcover == "Dry Forest Burn 6-15",
                                                                         "Dry forest, burn 6-15 yrs ago",
                                                                         ifelse(Landcover == "Mesic Forest (Burn >15)",
                                                                                "Wet forest, burn >15 yrs ago",
                                                                                ifelse(Landcover == "Dry Forest (Burn >15)",
                                                                                       "Dry forest, burn >15 yrs ago",
                                                                                       ifelse(Landcover == "Grass/Shrub/Open Woodland",
                                                                                              "Grassland/shrubland",
                                                                                       paste(Landcover)))))))))))) %>%
  group_by(Landcover) %>%
  summarise(Mean = mean(DE), Median = median(DE), n = n(), SD = sd(DE)) %>%
  ungroup()
de.lc$Landcover <- factor(de.lc$Landcover,
                          levels = de.lc$Landcover[order(de.lc$Mean,
                                                         decreasing = TRUE)],
                          ordered = TRUE)

vert <- ggplot(data = de.lc, 
               aes(x = Landcover, y = Mean,
                   ymin = Mean-2*SD,
                   ymax = Mean+2*SD)) +
  theme_bw() +
  geom_point(size = 3) +
  geom_errorbar(width = 0.1) +
  geom_hline(yintercept = 2.75) +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18,
                                   angle = 65,
                                   hjust = 1)) +
  labs(y = "Forage quality (kcal/g)", x = "") 
vert  
ggsave("de-landcov-vert.jpg", 
       plot = vert, 
       device = "jpeg",
       dpi = 300,
       units = "mm",
       width = wpg)
  

# horizontal plot of the above (for presentations)
horiz <- ggplot(data = de.lc, 
                aes(y = Landcover, x = Mean,
                    xmin = Mean-2*SD,
                    xmax = Mean+2*SD)) +
  geom_point(size = 3) +
  geom_errorbarh() +
  geom_vline(xintercept = 2.75) +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18)) +
  labs(x = "Forage Quality (kcal/g)", y = "") 
horiz
ggsave("de-landcov-horiz.jpg", 
       plot = horiz, 
       device = "jpeg",
       dpi = 300,
       units = "mm",
       width = 180)
