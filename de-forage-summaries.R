### ### ### ### ### ### ### ### ### ### ### ### ### ##
#        NUTRITION OF FORAGE PLANTS - SUMMARIES      #
#               NSERP - KRISTIN BARKER               #
#                     JUNE 2017                      #
### ### ### ### ### ### ### ### ### ### ### ### ### ##


## pertinent portions of this code have been incorporated
## into NutritionAnalyses.R along with all other data work
## related to the mignute manuscript

## ## ## ## ## ## 
####  SETUP  ####
## ## ## ## ## ## 



# packages #
  library(RODBC)
  library(tidyr)
  library(dplyr)



# working directories & database connection #
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
  
  
 
# "raw" data #
  de <- sqlQuery(channel, paste("select StudyArea, PlantCode, NameGenus, Stage, Class, DE
                                from Data_DMD where NOT (StudyArea = 'ELKHORNS')"))
  #de.phen <- read.csv("../Vegetation/DE-bylifeform")
 
  
  
## ## ## ## ## ## ## #
####  DOIN STUFF  ####
## ## ## ## ## ## ## #  
  
 
  ## de per lifeform ##
  
  de.lf <- de %>%
    group_by(Class) %>%
    summarise(StDev = sd(DE, na.rm=TRUE), DE = mean(DE, na.rm=TRUE)) %>%
    ungroup()
  write.csv(de.lf, "../Vegetation/de-by-lifeform-GENERALMODEL.csv")
  
  
  
  ## de per phenophase ##
  de.phen <- de %>%
    group_by(Class, Stage) %>%
    summarize(meanDE = mean(DE)) %>%
    spread(Stage, meanDE) %>%
    rename(EM.cls.de=emergent, 
           FL.cls.de=flowering, 
           FR.cls.de=fruiting, 
           MS.cls.de=mature, 
           SE.cls.de=cured)
  write.csv(de.phen, "../Vegetation/de-by-lifeformphenophase-GENERALMODEL.csv")
  
length(which(de$Stage == "mature"))
length(which(de$Stage == "emergent"))


  ## avg de in measured north sapphire plots ##
  de.ns <- de %>%
    filter(StudyArea == "NS") %>%
    summarise(StDev = sd(DE), Mean = mean(DE))
  
  
  