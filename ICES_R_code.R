
## Functions used in the analysis
source("functions.R")

################
## STILLS
################

################################################################
## Read in the data and do some sorting out
################################################################

stills = read.csv("stills.csv", header=TRUE, na.strings="")

## Get rid of the 296 stills with $Quality value = "Zero" 
table(stills$Quality)               # 296 zero stills to be removed
use = stills$Quality != "Zero"
table(use)
stills = stills[use,]

## Put 0s in LitterCount column. They had been left blank and so
## became coded as NA

stills$LitterCount[is.na(stills$LitterCount)] = 0
table(stills$LitterCount)

## 40 instances where litter was found
## General categories are: A=Plastic, B=Metal, C=Rubber, D=Glass,
##                         E=Natural, F=Misc

table(stills$LitterCats)
sum(table(stills$LitterCats))                 # 40

## There are 44 litter items seen. 37 singles, 2 doubles and 1 triple
table(stills$LitterCount)
sum(stills$LitterCount, na.rm=T)              # 44
sum(table(stills$LitterCats1))                # 40
sum(table(stills$LitterCats2))                # 3
sum(table(stills$LitterCats3))                # 1

## survey.list contains the names of the 8 original surveys

survey.list = as.character(as.data.frame(table(stills$Survey))$Var1)

#######################################################
## Create the four new surveys used in the paper (based
## on combining the old surveys)
#######################################################

## Goodwin Sands
newsurveys = ifelse(stills$Survey=="Goodwin Sands rMCZ 2014",
                 "Goodwin Sands 2014", stills$Survey)

## North Norfolk
newsurveys = ifelse(newsurveys=="North Norfolk Sandbanks and Saturn Reef 2013" |
                    newsurveys=="North Norfolk Sandbanks and Saturn Reef SAC 2016",
                  "North Norfolk", newsurveys)

## Swallow Sands
newsurveys = ifelse(newsurveys=="Swallow Sand 2012" |
                    newsurveys=="Swallow Sand 2014" |
                    newsurveys=="Swallow Sand 2016",
                  "Swallow Sands", newsurveys)

table(newsurveys)

stills$Survey2 = newsurveys

#############################################################
## Calculations on litter items
#############################################################

## Used below in calculating density based on video areas
tapply(stills$LitterCount, stills$Survey2, sum)

## Litter was found in 40 stills
table(stills$Litter)

## One item found 37 times, two items 2 times, 3 items 1 time.
## That makes 44 litter items in total
table(stills$LitterCount)

## Find the categories of the 44 litter items found

temp = c(stills$LitterCats1, stills$LitterCats2, stills$LitterCats3)
LitterCatsAll = temp[!is.na(temp)]
table(LitterCatsAll)
sum(table(LitterCatsAll))                      # 44

## Divide the litter into the bigger categories (SuperCats).
# A=Plastic, B=Metal, C=Rubber, D=Glass, E=Natural, F=Misc

## Define these SuperCats by their first letter
SuperCats = substring(LitterCatsAll, 1, 1)
table(SuperCats)

temp = ifelse(SuperCats=="A", "Plastic", SuperCats)
temp = ifelse(SuperCats=="B", "Metal", temp)
temp = ifelse(SuperCats=="D", "Glass", temp)
temp = ifelse(SuperCats=="E", "Natural", temp)
temp = ifelse(SuperCats=="F", "Misc", temp)
SuperCats = temp
table(SuperCats)
#Glass   Metal    Misc Natural Plastic 
#6       3      20       5      10 

## Percentage of each SuperCat
round(100 * table(SuperCats) / sum(table(SuperCats)),1)
#Glass   Metal    Misc Natural Plastic 
#13.6     6.8    45.5    11.4    22.7 

###########################################################
## How does still quality affect the number of litter
## items seen?
###########################################################

## For the quality comparison for stills, we need to calculate
## the area of the stills. The problem is that there are missing
## values (1858 of them) and that we have to impute these from the
## observed values

area = as.numeric(stills$Area)
sum(is.na(area))

## Find number of missing values per survey
misspersurvey = rep(0, length(survey.list))
for (j in 1:8) {
  misspersurvey[j] = sum(is.na(area[stills$Survey==survey.list[j]]))
}
misspersurvey

## Impute these missing areas. Use the mean area for that survey.

## There are no observed areas for the North Norfolk 2016 survey
##. So use 2013 instead.

meanareas = tapply(area, stills$Survey, mean, na.rm=T)
meanareas[4] = meanareas[3]
meanareas

nobs = length(stills$Area)                      # 7316
area2 = area

for (j in 1:nobs) {
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[1])
    area[j]=meanareas[1]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[2])
    area[j]=meanareas[2]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[3])
    area[j]=meanareas[3]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[4])
    area[j]=meanareas[4]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[5])
    area[j]=meanareas[5]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[6])
    area[j]=meanareas[6]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[7])
    area[j]=meanareas[7]
  if (is.na(area2[j]) & stills$Survey[j]==survey.list[8])
    area[j]=meanareas[8]
}

stills$Area2 = area

## Litter density

table(stills$Quality)
(litterperquality = tapply(stills$LitterCount, stills$Quality, sum))
(areaperquality = tapply(stills$Area2, stills$Quality, sum))
(density = 1000000 * litterperquality / areaperquality)

## Combine (poor and very poor) and (good and excellent) and compare
## the difference in densities for the two groups. Use this as the
## test statistic in the randomisation procedure. Call the first group
## "ExGood" and the second group "Rubbish".

Quality2 = ifelse(stills$Quality=="Excellent" |
                    stills$Quality=="Good", "ExGood", "Rubbish")
table(Quality2)
(litterperquality = tapply(stills$LitterCount, Quality2, sum))
(areaperquality = tapply(stills$Area2, Quality2, sum))
(density = 1000000 * litterperquality / areaperquality)
## This is the observed difference in density between the two groups
(obsdiff = density[1] - density[2])

## DO RANDOMAISATION TEST
## Randomly allocate the 44 litter items to the total area
## Then allocate these to one of the two classes based on areas

bounds = c(0, cumsum(areaperquality))
totalarea = bounds[3]
nsims = 9999
simdiff = rep(0, nsims)
set.seed(99)

for (j in 1:nsims) {
  
  rands = runif(44, 0, totalarea)
  simcount = cut(rands, bounds)
  temp = table(simcount)
  simdens = temp / areaperquality
  simdiff[j] = simdens[1] - simdens[2]
  
}

bigger = sum(simdiff >= obsdiff)
pvalue = (bigger + 1)/(nsims + 1)
pvalue                               # 0.0001

################
## VIDEOS
################

videos = read.csv("videos.csv", header=TRUE, na.strings="")

## Create an average width value for each survey.

nvideos = dim(videos)[1]
width = rep(0, nvideos)

for (j in 1: nvideos) {
  if (videos$Survey[j] == "North Norfolk Sandbanks and Saturn Reef SAC 2013")
        width[j] = 56.8
  if (videos$Survey[j] == "North Norfolk Sandbanks and Saturn Reef SAC 2016")
    width[j] = 115.9
  if (videos$Survey[j] == "Offshore Overfalls MCZ 2019")
    width[j] = 101.5
  if (videos$Survey[j] == "Swallow Sands MCZ 2012")
    width[j] = 137
  if (videos$Survey[j] == "Swallow Sands MCZ 2014")
    width[j] = 137
  if ((videos$Survey[j] == "Swallow Sands MCZ 2016") &
      (videos$Method[j] == "Camera Sledge")) {width[j] = 137}
  if ((videos$Survey[j] == "Swallow Sands MCZ 2016") &
      (videos$Method[j] == "Drop Camera")) {width[j] = 158.9}
  if (videos$Survey[j] == "Goodwin Sands MCZ 2014")
    width[j] = 56.0
  if (videos$Survey[j] == "Goodwin Sands rMCZ 2014")
    width[j] = 52.7
}
videos$Width = width / 100                     # in metres

## Calculate distance of each video. This uses a function that has
## a correction based on earth curvature. Probably not needed for
## these short trawls but used it anyway.

videos$Distance = JB.dis.two.pts(videos$LatStart,videos$LatEnd,
                                 videos$LongStart,videos$LongEnd)
videos$Distance

## Calculate area of videos

videos$Area = videos$Width *  videos$Distance

## Sum over all videos in that survey
sumvidareas = tapply(videos$Area, videos$Survey, sum)
sumvidareas

###############################################################
## Calculate STILLS densities for each of the four new surveys
## Table 2 in paper.
###############################################################

## This represents all videos for NN 2016, OO 2016 and SS 2016
## And 30% for the rest.

## Generate areas the 7 old surveys (combining the two Goodwin
## Sands surveys)

vidareasforstills = rep(0, 7)
vidareasforstills[1] = (sumvidareas[1] + sumvidareas[2]) * 100 / 30
vidareasforstills[2] = sumvidareas[3] * 100 / 30
vidareasforstills[3] = sumvidareas[4]
vidareasforstills[4] = sumvidareas[5]
vidareasforstills[5] = sumvidareas[6] * 100 / 30
vidareasforstills[6] = sumvidareas[7] * 100 / 30
vidareasforstills[7] = sumvidareas[8]

round(vidareasforstills)

## For the 4 areas
vidareasforstills2 = rep(0,4)

vidareasforstills2[1] = vidareasforstills[1]
vidareasforstills2[2] = vidareasforstills[2] + vidareasforstills[3]
vidareasforstills2[3] = vidareasforstills[4]
vidareasforstills2[4] = vidareasforstills[5] +
                        vidareasforstills[6] +
                        vidareasforstills[7]

## Video areas for each of the four surveys
round(vidareasforstills2)

## videoareasforstills2 is in m2, so we need to multiply by
## 1000,000 to get density in km2
littercountsforstills = c(4, 8, 13, 19)
round(1000000*littercountsforstills / vidareasforstills2)

## Litter density for stills over all surveys
44 * 1000000 / sum(vidareasforstills2)        # 557

## Bootstrap confidence intervals for total stills density
## (used in Table 2)

set.seed(4)
CI_mean = bootstrap(stills$LitterCount)$percentile
lstills = length(stills$LitterCount)
(CI_tot = CI_mean * lstills)
CI_tot * 1000000 / sum(vidareasforstills) # 392, 759

## Number of stills in each of the four new surveys
## (used in Table 1)

(table(stills$Survey2))

###############################################################
## End of STILLS stuff
###############################################################

###############################
## Back to the VIDEO analysis
###############################

## Combining the two Goodwin Sands surveys and redefining
## videos$Survey.

survey = ifelse(videos$Survey=="Goodwin Sands rMCZ 2014",
      "Goodwin Sands MCZ 2014", videos$Survey)
table(survey)
videos$Survey = survey

## ********************************************
## Calculations on litter counts
## ********************************************

temp = c(videos$LitterCats1, videos$LitterCats2, videos$LitterCats3)
LitterCatsAll = temp[!is.na(temp)]

## Categories of the 29 litter items found
table(LitterCatsAll)
sum(table(LitterCatsAll))

table(videos$LitterCats)
sum(table(videos$LitterCats)) - 348                # 25

## There are 29 litter items seen. 20 singles, 3 doubles and 1 triple
## The count is 24 below becasue there are 24 times that at least one
## litter item is found. Similarly, there are 4 occasions on at which
## two litter items are found.

table(videos$LitterCount)
sum(videos$LitterCount, na.rm=T)              # 29
sum(table(videos$LitterCats1))                # 24
sum(table(videos$LitterCats2))                # 4
sum(table(videos$LitterCats3))                # 1

## Replace NA by 0 in LitterCount column

videos$LitterCount[is.na(videos$LitterCount)] = 0
table(videos$LitterCount)

## Peel off the first letter to define the super categories.
## A=Plastic, B=Metal, C=Rubber, D=Glass, E=Natural, F=Misc

table(LitterCatsAll)
SuperCats = substring(LitterCatsAll, 1, 1)
SuperCats

temp2 = ifelse(SuperCats=="A", "Plastic", SuperCats)
temp2 = ifelse(SuperCats=="B", "Metal", temp2)
temp2 = ifelse(SuperCats=="D", "Glass", temp2)
temp2 = ifelse(SuperCats=="E", "Natural", temp2)
temp2 = ifelse(SuperCats=="F", "Misc", temp2)
SuperCats = temp2
(table(SuperCats))
#Glass   Metal    Misc Natural Plastic 
#6       7       4       2      10 
# Rubber = 0

## Percentage in each category
round(100 * table(SuperCats) / sum(table(SuperCats)),1)

## Create the definitions of the four surveys used

surveysnew = videos$Survey
table(surveysnew)

surveysnew = ifelse(surveysnew=="North Norfolk Sandbanks and Saturn Reef SAC 2013" |
                  surveysnew=="North Norfolk Sandbanks and Saturn Reef SAC 2016",
                  "North Norfolk", surveysnew)

surveysnew = ifelse(surveysnew=="Swallow Sands MCZ 2012" |
                    surveysnew=="Swallow Sands MCZ 2014" |
                    surveysnew=="Swallow Sands MCZ 2016",
                  "Swallow Sands", surveysnew)
table(surveysnew)
videos$Survey2 = surveysnew

litterpersurvey = tapply(videos$LitterCount, videos$Survey2, sum)
litterpersurvey; sum(litterpersurvey)
table(videos$Survey2); sum(table(videos$Survey2))
areapersurvey = tapply(videos$Area, videos$Survey2, sum)
round(areapersurvey); sum(areapersurvey)
density = litterpersurvey/areapersurvey*1000000

## These are the values given in Table 2 for the density
## per survey

round(density)

## Total density over all the 4 surveys
1000000 * sum(litterpersurvey) / sum(areapersurvey)    # 740

## Bootstrap estimate for total density (also in Table 2)
set.seed(2)
boot = bootstrap.mod(videos$LitterCount, videos$Area,
                     nreps=1000)$percentile
round(boot)                         # (395, 1226)

##############################################################
## How does video quality affect the density of litter seen?
##############################################################

## As for STILLS, combine (poor and very poor) and
## (good and excellent) and calculate the difference in
## densities for the two groups. Use this as your test statistic
## in the randomisation test procedure. Call the two new categories
## "ExGood" and "Rubbish".

Quality2 = ifelse(videos$Quality=="Excellent" |
                    videos$Quality=="Good", "ExGood", "Rubbish")
table(Quality2)
(litterperquality = tapply(videos$LitterCount, Quality2, sum))
(areaperquality = tapply(videos$Area, Quality2, sum))
(density = 1000000 * litterperquality / areaperquality)
(obsdiff = density[1] - density[2])

## Randomisation test.
## Randomly allocate the 29 litter items to the total area
## Then allocate these to one of the two classes based on areas

bounds = c(0, cumsum(areaperquality))
totalarea = bounds[3]
nsims = 9999
simdiff = rep(0, nsims)
set.seed(99)

for (j in 1:nsims) {
  
  rands = runif(29, 0, totalarea)
  simcount = cut(rands, bounds)
  temp = table(simcount)
  simdens = temp / areaperquality
  simdiff[j] = simdens[1] - simdens[2]
  
}

bigger = sum(simdiff >= obsdiff)
pvalue = (bigger + 1)/(nsims + 1)
pvalue                               # 0.0001

###################################################################
## TRAWLS
###################################################################

trawls = read.csv("trawls.csv", header=TRUE, na.strings="")

## Make some better names for the variables
names(trawls)[1:20] = c("survey","quarter","year","country","gear",
                        "haul","stat","haullat","haullong","shootlat",
                        "shootlong","distance","depth","duration",
                        "region","door","wing","warplen","netopen",
                        "sizecat")
names(trawls)

## Define which of the four new survey areas the start of the
## trawl occurs in.

area = ifelse(trawls$shootlong < (-0.3), "Overfalls", " ")
area = ifelse(trawls$shootlat>51 & trawls$shootlat<52, "Goodwin",
              area)
area = ifelse(trawls$shootlat>52 & trawls$shootlat<54.5, "Norfolk",
              area)
area = ifelse(trawls$shootlat>55, "Swallow", area)
table(area)
trawls$area = area

## 90 beam trawls and 99 GOV
table(trawls$gear)

## beam and GOV trawls by new survey (Table 1)
table(trawls$gear, trawls$area)

## Convert various numbers in the data to NA. This gives us a
## single code for missing values.

change = trawls$wing == -9
trawls$wing[change] = NA

change = trawls$door == -9
trawls$door[change] = NA

table(trawls$sizecat)
change = trawls$sizecat == "-9" | trawls$sizecat == "12" |
         trawls$sizecat == "13" | trawls$sizecat == "14" |
         trawls$sizecat == "15"
trawls$sizecat[change] = NA
table(trawls$sizecat)

trawls$distance = as.integer(trawls$distance)
sum(trawls$distance==-9, na.rm=T)         # 1
trawls$distance[trawls$distance==-9] = NA
sum(is.na(trawls$distance))               # 14 missing distances

## Calculate trawl distances from latitude and longitude to
## fill in some missing values

distance.new = JB.dis.two.pts(trawls$shootlat, trawls$haullat,
               trawls$shootlong, trawls$haullong)
discrep = trawls$distance - distance.new

## All distances for NAs seem OK EXCEPT 175. This has a low
## calculated distance (277m) so I'm going to exclude it.

cbind(discrep, trawls$distance, distance.new)
trawls[175,]

insert = is.na(trawls$distance)
trawls$distance[insert] = distance.new[insert]
trawls$distance

## Remove row 175 from the trawls data frame and create
## new data frame.
trawls2 = trawls[-175,]

## Code below shows that we have 42 missing WINGSPAN values

table(trawls2$gear)
dim(trawls2)

gov = trawls2$gear == "GOV"

trawls2$wing = as.numeric(trawls2$wing)
trawls2$door = as.numeric(trawls2$door)

sum(is.na(trawls2$wing[gov]))                       # 42

## The missing values are from the NS-IBTS and the FR-CGFS surveys
trawls2$survey[gov][is.na(trawls2$wing[gov])]

## Impute missing values with the mean wingspan from these surveys

FRwingmean = mean(trawls2$wing[trawls2$survey=="FR-CGFS"], na.rm=T)
NSwingmean = mean(trawls2$wing[trawls2$survey=="NS-IBTS"], na.rm=T)

use = trawls2$survey == "NS-IBTS" & is.na(trawls2$wing)
trawls2$wing[use] = NSwingmean

use = trawls2$survey == "FR-CGFS" & is.na(trawls2$wing)
trawls2$wing[use] = FRwingmean

##########################################################
## Generate area swept for each trawl
## BT trawls do not have a wingspan allocated to them
##########################################################

## Create a new variable "width". This is the value of wing for GOV
## and the beam trawl width for any beam trawl.

table(trawls2$gear)
width = rep(0, dim(trawls2)[1])

width = ifelse(trawls2$gear=="BT4A", 4, width)
width = ifelse(trawls2$gear=="BT4AI", 4, width)
width = ifelse(trawls2$gear=="BT8", 8, width)
width = ifelse(trawls2$gear=="BTS", 25, width)

width = ifelse(trawls2$gear=="GOV", trawls2$wing, width)
width

trawls2$width = width

## Area swept (i.e. area of the trawl) is width * distance
trawls2$swept = trawls2$distance * trawls2$width / 1000000

## How many surveys do we have that are the correct year?

## For Goodwin Sands, we don't have any trawls when the videos
## were taken in 2014. There are 7 between 2012 and 2016, We
## will use those - though this makes the comparison worse.
use = trawls2$area=="Goodwin"
table(trawls2$year[use])

## 9 in 2013 and 7 in 2016
use = trawls2$area=="Norfolk"
table(trawls2$year[use])

## 4 in 2019
use = trawls2$area=="Overfalls"
table(trawls2$year[use])

## 4 in 2012; 7 in 2014; 7 in 2016
use = trawls2$area=="Swallow"
table(trawls2$year[use])

#################################################################
## TRAWLS ANALYSIS
## Here we only do the trawl densities for the years that match
## with the video surveys (except for Goodwin Sands)
#################################################################

## Create small data set that contains only the trawls to analyse

use = (trawls2$area=="Goodwin" & (trawls2$year==2012 |
      trawls2$year==2013 | trawls2$year==2015 | trawls2$year==2016)) |
      (trawls2$area=="Norfolk" & (trawls2$year==2013 |
                          trawls2$year==2016)) |
      (trawls2$area=="Overfalls" & trawls2$year==2019) |
      (trawls2$area=="Swallow" & (trawls2$year==2012 |
         trawls2$year==2014 | trawls2$year==2016))

trawls_small = trawls2[use,]
table(trawls_small$area, trawls_small$year)
dim(trawls_small)                       # 45 trawls being used

## Calculate the number of different trawl types. There are
## 19 Beam and 26 GOV trawls.
table(trawls_small$gear)

## Create an expanded area variable (e.g. 3 for Swallow)

areasurv = rep("g", dim(trawls_small)[1])
areasurv = ifelse(trawls_small$area=="Goodwin", "Goodwin", areasurv)
areasurv = ifelse(trawls_small$area=="Norfolk" &
                  trawls_small$year==2013, "Norfolk13", areasurv)
areasurv = ifelse(trawls_small$area=="Norfolk" &
                    trawls_small$year==2016, "Norfolk16", areasurv)
areasurv = ifelse(trawls_small$area=="Overfalls", "Overfalls", areasurv)
areasurv = ifelse(trawls_small$area=="Swallow" &
                    trawls_small$year==2012, "Swallow12", areasurv)
areasurv = ifelse(trawls_small$area=="Swallow" &
                    trawls_small$year==2014, "Swallow14", areasurv)
areasurv = ifelse(trawls_small$area=="Swallow" &
                    trawls_small$year==2016, "Swallow16", areasurv)
trawls_small$areasurv = areasurv

table(trawls_small$areasurv, trawls_small$gear)

## Information for Table 1 and for the densities in Table 2

## Number of trawls per new survey area (Table 1)
(freqs = table(trawls_small$area))

## Trawl types by new survey area
table(trawls_small$area, trawls_small$gear)

## Total litter in each new survey area
(littertots = tapply(trawls_small$litter.tot, trawls_small$area, sum))

## Trawl areas in km^2
(trawlareas = tapply(trawls_small$swept, trawls_small$area, sum))
round(trawlareas*1000000)       # in m^2

meantrawlareas = tapply(trawls_small$swept, trawls_small$area, mean)
round(meantrawlareas*1000000)
mean(trawls_small$swept)

## Densities per km^2 by new survey area (Table 2)
densities = littertots / trawlareas
round(densities,1)

## Total density = 70.3 (Table 2)
sum(littertots) / sum(trawlareas)

## Bootstrap confidence interval for total density of trawls

set.seed(1)
CI_trawl = bootstrap.mod(trawls_small$litter.tot,
                         trawls_small$swept)$percentile / 1000000
CI_trawl   # 46.8, 100.6

########################################
## Comparing trawl counts by gear type

trawls_small$geartype <- ifelse(trawls_small$gear=="GOV", "GOV", "BEAM")
table(trawls_small$geartype, trawls_small$area)

## Area swept for each type
(gearareas = tapply(trawls_small$swept, trawls_small$geartype, sum))

## Litter item count by gear type
(gearcounts = tapply(trawls_small$litter.tot, trawls_small$geartype, sum))

## Density by gear type
(geardens <- gearcounts / gearareas)
geardens[1] / geardens[2]               # 4.8 higher for Beam

#############################################################
## Calculations on litter categories
#############################################################

tapply(trawls_small$litter.tot, trawls_small$area, sum)
tapply(trawls_small$plastic.tot, trawls_small$area, sum)
tapply(trawls_small$metal.tot, trawls_small$area, sum)
tapply(trawls_small$rubber.tot, trawls_small$area, sum)
tapply(trawls_small$glass.tot, trawls_small$area, sum)
tapply(trawls_small$natural.tot, trawls_small$area, sum)
tapply(trawls_small$misc.tot, trawls_small$area, sum)

## ttt is the total number of litter items found
(ttt = sum(trawls_small$litter.tot))

## Break total down into categories
sum(trawls_small$plastic.tot)
sum(trawls_small$metal.tot)
sum(trawls_small$rubber.tot)
sum(trawls_small$glass.tot)
sum(trawls_small$natural.tot)
sum(trawls_small$misc.tot)

## Percentages
100 * sum(trawls_small$plastic.tot) / ttt
100 * sum(trawls_small$metal.tot) / ttt
100 * sum(trawls_small$rubber.tot) / ttt
100 * sum(trawls_small$glass.tot) / ttt
100 * sum(trawls_small$natural.tot) / ttt
100 * sum(trawls_small$misc.tot) / ttt

## Create data frame containing frequency and percentage for each
## category

cats = data.frame(cat=rep("A",6), freq=rep(0,6), perc=rep(0,6))

plastic.df = trawls_small[,28:42]
temp = colSums(plastic.df); sum(temp); temp
cats[1,2] = sum(temp)

metal.df = trawls_small[,43:51]
temp = colSums(metal.df); sum(temp); temp
cats[2,2] = sum(temp)

rubber.df = trawls_small[,52:58]
temp = colSums(rubber.df); sum(temp); temp
cats[3,2] = sum(temp)

glass.df = trawls_small[,59:63]
temp = colSums(glass.df); sum(temp); temp
cats[4,2] = sum(temp)

natural.df = trawls_small[,64:69]
temp = colSums(natural.df); sum(temp); temp
cats[5,2] = sum(temp)

misc.df = trawls_small[,70:73]
temp = colSums(misc.df); sum(temp); temp
cats[6,2] = sum(temp)

cats[,1] = c("plastic", "metal", "rubber", "glass",
             "natural", "misc")
cats[,3] = round(100 * cats[,2] / sum(cats[,2]), 1)
cats

##############################################################
##############################################################
## Chi-squared test on the frequencies of litter
## categories (percentages are displayed in Figure 2).
## These values were calculated above for stills, videos
## and trawls.
## The simulated p-value should get around the issue of low
## counts in some of the cells (which maeans that the standard
## Chi-square p-value is unlikely to be correct).
##############################################################

cont.table = matrix(0, ncol=3, nrow=6)
cont.table[,1] = c(10, 3, 0, 6, 5, 20)
cont.table[,2] = c(10, 7, 0, 6, 2, 4)
cont.table[,3] = c(109, 2, 3, 0, 26, 19)
cont.table

## Compare all 3 methods
set.seed(1)
## Standard chi-square test
chisq.test(cont.table)$p.value   # 10^-15

## Simulated p-value: p = 0.000499
chisq.test(cont.table, simulate.p.value=TRUE)$p.value

## Compare Stills and videos (remove 0 rubber row)
set.seed(1)
temp = cont.table[-3,-3]; temp
## p = 0.025 (due to high miscellaneous row)
chisq.test(temp, simulate.p.value=TRUE)$p.value

## Compare Stills and Trawls
set.seed(1)
temp = cont.table[,-2]; temp
## p = 0.0004999
chisq.test(temp, simulate.p.value=TRUE)$p.value

## Compare Videos and Trawls
set.seed(1)
temp = cont.table[,-1]; temp
## p = 0.0004999
chisq.test(temp, simulate.p.value=TRUE)$p.value

## Which surveys does GOV gear cover?

table(trawls2$gear, trawls2$area)

################################################################
################################################################
## Do we find different-sized litter items in stills compared to
## trawls? Use the reduced set of trawls used in the density
## comparisons with stills and videos.
################################################################

table(trawls_small$sizecat)
sum(is.na(trawls_small$sizecat))                    # 13 NAs

## Strip out the missing values. Leaves us with 32 sizes for
## the trawls
use = !is.na(trawls_small$sizecat)
trawlssizecat = trawls_small$sizecat[use]
trawlssizecat; length(trawlssizecat)

## What are the size categories for the 44 stills?
ss = read.csv("still_sizes.csv", header=TRUE, na.strings="")
names(ss)
ss$size

## Remove an NA from both the ssizevat and ssize variables. This
## leaves us with 43 non-missing observations
stillssizecat = ss$size[-19]
stillssize = as.numeric(ss$Area..cm2)[-19]

(nt = length(trawlssizecat))                     # 32
(ns = length(stillssizecat))                     # 43

table(stillssizecat)
table(trawlssizecat)

## Percentages for stills and trawls
round(table(stillssizecat)*100 / ns, 1)
round(table(trawlssizecat)*100 / nt, 1)

median(stillssize)
sort(trawlssizecat)[16:17]        # median is B
sort(trawlssizecat)

sort(stillssizecat)                    # median is A

## Mid points (in 1d) of categories

midcats = c(2.5, 7.5, 15, 35, 75, 150)^2

## Perform randomisation test.
## Use maximum difference in distribution function (K-S test)
## between the stills and trawls' size distributions for the
## objective function.

tnum = rep(0, nt)
tnum = ifelse(trawlssizecat=="A", 1, tnum)
tnum = ifelse(trawlssizecat=="B", 2, tnum)
tnum = ifelse(trawlssizecat=="C", 3, tnum)
tnum = ifelse(trawlssizecat=="D", 4, tnum)
tnum = ifelse(trawlssizecat=="E", 5, tnum)
tnum = ifelse(trawlssizecat=="F", 6, tnum)
tnum

snum = rep(0, ns)
snum = ifelse(stillssizecat=="A", 1, snum)
snum = ifelse(stillssizecat=="B", 2, snum)
snum = ifelse(stillssizecat=="C", 3, snum)
snum = ifelse(stillssizecat=="D", 4, snum)
snum

nsims = 9999
sim.diff = rep(0,nsims)
stnum = c(snum, tnum)
nst = length(stnum)

Ft = ecdf(tnum)
Fs = ecdf(snum)
obs.diff = max(abs(Ft(1:6) - Fs(1:6)))
Ft(1:6)
Fs(1:6)

set.seed(44)

for (j in 1:nsims) {
  stnum.sim = sample(stnum, nst)
  s.sim = stnum.sim[1:ns]
  t.sim = stnum.sim[(ns+1):nst]
  Ft.sim = ecdf(t.sim)
  Fs.sim = ecdf(s.sim)
  sim.diff[j] = max(abs(Ft.sim(1:6) - Fs.sim(1:6)))
}
bigger = sum(abs(sim.diff >= obs.diff))
pvalue = (bigger + 1)/(nsims + 1)
pvalue                                   # 0.32

####################################################################
## AUV stills analysis for Celtic Deep
####################################################################

AUV = read.csv("AUV.csv", header=TRUE, na.strings="")
dim(AUV)                      # 3000 images
table(AUV$Litter)

(nitems = sum(AUV$Litter=="Litter"))

(totArea = sum(AUV$Area) / 1000000)

(density = nitems / totArea)       # 2374.1

## Types of litter found
table(AUV$Type)

