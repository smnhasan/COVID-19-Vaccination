library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)


##Descriptive
##WORLD SUMMARY

#2022 CFR
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022w <- read.csv("owid-covid-data.csv")

COVID2022w <- subset(COVID2022w, COVID2022w$date == "2022-12-31") #5/29/2021 2/24/2020

#Remove World and International information
COVID2022w<-COVID2022w[!(COVID2022w$iso_code=="OWID_AFR" | COVID2022w$iso_code=="OWID_ASI" | COVID2022w$iso_code == "OWID_EUN" | COVID2022w$iso_code=="OWID_EUR" | COVID2022w$iso_code=="OWID_INT" | 
                         COVID2022w$iso_code=="OWID_HIC" | COVID2022w$iso_code=="OWID_KOS" | COVID2022w$iso_code=="OWID_LIC" | COVID2022w$iso_code=="OWID_LMC" | COVID2022w$iso_code=="OWID_NAM" | 
                         COVID2022w$iso_code == "OWID_OCE" | COVID2022w$iso_code=="OWID_SAM"| COVID2022w$iso_code=="OWID_UMC" | COVID2022w$iso_code=="OWID_WRL" | COVID2022w$iso_code=="PRK"),]

COVID2022w <- COVID2022w[ which(COVID2022w$population >= 1000000), ]
NROW(COVID2022w)
#Creating CFR
COVID2022w$CFR2022w <- (COVID2022w$total_deaths/COVID2022w$total_cases)*100
t.test(COVID2022w$CFR2022w)
library("plotrix")
op <- std.error(COVID2022w$CFR2022w)
op


t.test(COVID2022w$total_vaccinations_per_hundred)

library("plotrix")
op <- std.error(COVID2022w$total_vaccinations_per_hundred)
op

#highest CFR
COVID2022w <- COVID2022w[order(-COVID2022w$CFR2022w),]
COVID2022w

#2022 Excess Mortality
setwd('D:\\Update3\\Update4\\Update5\\update6')
COVID2022wem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID2022wem$date <-  COVID2022wem$Day
COVID2022wem$iso_code <-  COVID2022wem$Code 
COVID2022wem$location <-  COVID2022wem$Entity

COVID2022wem <- subset(COVID2022wem, COVID2022wem$date == "2022-12-30") #5/29/2021 2/24/2020



#Remove World and International information
COVID2022wem<-COVID2022wem[!(COVID2022wem$iso_code=="OWID_AFR" | COVID2022wem$iso_code=="OWID_ASI" | COVID2022wem$iso_code == "OWID_EUN" | COVID2022wem$iso_code=="OWID_EUR" | COVID2022wem$iso_code=="OWID_INT" | 
                         COVID2022wem$iso_code=="OWID_HIC" | COVID2022wem$iso_code=="OWID_KOS" | COVID2022wem$iso_code=="OWID_LIC" | COVID2022wem$iso_code=="OWID_LMC" | COVID2022wem$iso_code=="OWID_NAM" | 
                         COVID2022wem$iso_code == "OWID_OCE" | COVID2022wem$iso_code=="OWID_SAM"| COVID2022wem$iso_code=="OWID_UMC" | COVID2022wem$iso_code=="OWID_WRL" | COVID2022wem$iso_code=="PRK"),]

COVID2022wemm <- merge(COVID2022wem, COVID2022w, by=c("location"))

COVID2022wemm <- COVID2022wemm[ which(COVID2022wemm$population >= 1000000), ]
nrow(COVID2022wemm)
#Creating CFR
t.test(COVID2022wemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVID2022wemm$estimated_daily_excess_deaths_per_100k)
op

#2021
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDw <- read.csv("owid-covid-data.csv")

COVIDw <- subset(COVIDw, COVIDw$date == "2021-01-05") #5/29/2021

#Remove World and International information
COVIDw<-COVIDw[!(COVIDw$iso_code=="OWID_AFR" | COVIDw$iso_code=="OWID_ASI" | COVIDw$iso_code == "OWID_EUN" | COVIDw$iso_code=="OWID_EUR" | COVIDw$iso_code=="OWID_INT" | 
                 COVIDw$iso_code=="OWID_HIC" | COVIDw$iso_code=="OWID_KOS" | COVIDw$iso_code=="OWID_LIC" | COVIDw$iso_code=="OWID_LMC" | COVIDw$iso_code=="OWID_NAM" | 
                 COVIDw$iso_code == "OWID_OCE" | COVIDw$iso_code=="OWID_SAM"| COVIDw$iso_code=="OWID_UMC" | COVIDw$iso_code=="OWID_WRL"| COVIDw$iso_code=="PRK"),]


COVIDw <- COVIDw[ which(COVIDw$population >= 1000000), ]

#Creating CFR
COVIDw$CFR2021w <- (COVIDw$total_deaths/COVIDw$total_cases)*100
t.test(COVIDw$CFR2021w)
library("plotrix")
op <- std.error(COVIDw$CFR2021w)
op

t.test(COVIDw$CFR2021w[1:159], COVID2022w$CFR2022w, paired = TRUE, alternative = "two.sided")


#2021 Excess Mortality
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDwem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVIDwem$date <-  COVIDwem$Day
COVIDwem$iso_code <-  COVIDwem$Code 
COVIDwem$location <-  COVIDwem$Entity

COVIDwem <- subset(COVIDwem, COVIDwem$date == "2021-01-04") #5/29/2021 2/24/2020

#Remove World and International information
COVIDwem<-COVIDwem[!(COVIDwem$iso_code=="OWID_AFR" | COVIDwem$iso_code=="OWID_ASI" | COVIDwem$iso_code == "OWID_EUN" | COVIDwem$iso_code=="OWID_EUR" | COVIDwem$iso_code=="OWID_INT" | 
                       COVIDwem$iso_code=="OWID_HIC" | COVIDwem$iso_code=="OWID_KOS" | COVIDwem$iso_code=="OWID_LIC" | COVIDwem$iso_code=="OWID_LMC" | COVIDwem$iso_code=="OWID_NAM" | 
                       COVIDwem$iso_code == "OWID_OCE" | COVIDwem$iso_code=="OWID_SAM"| COVIDwem$iso_code=="OWID_UMC" | COVIDwem$iso_code=="OWID_WRL" | COVIDwem$iso_code=="PRK"),]

COVIDwemm <- merge(COVIDwem, COVIDw, by=c("location"))

COVIDwemm <- COVIDwemm[ which(COVIDwemm$population >= 1000000), ]
nrow(COVIDwemm)
#Creating CFR
t.test(COVIDwemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVIDwemm$estimated_daily_excess_deaths_per_100k)
op

t.test(COVIDwemm$estimated_daily_excess_deaths_per_100k[1:156], COVID2022wemm$estimated_daily_excess_deaths_per_100k, paired = TRUE, alternative = "two.sided")

#selection of Top-20 countries
#Data Management

setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDT20 <- read.csv("owid-covid-data.csv")

COVIDT20 <- subset(COVIDT20, COVIDT20$date >= "2020-01-01") #1/1/2020 2020-08-08
COVIDT20 <- subset(COVIDT20, COVIDT20$date <= "2022-12-31") #5/29/2021

#Remove World and International information
COVIDT20<-COVIDT20[!(COVIDT20$iso_code=="OWID_AFR" | COVIDT20$iso_code=="OWID_ASI" | COVIDT20$iso_code == "OWID_EUN" | COVIDT20$iso_code=="OWID_EUR" | COVIDT20$iso_code=="OWID_INT" | 
                 COVIDT20$iso_code=="OWID_HIC" | COVIDT20$iso_code=="OWID_KOS" | COVIDT20$iso_code=="OWID_LIC" | COVIDT20$iso_code=="OWID_LMC" | COVIDT20$iso_code=="OWID_NAM" | 
                 COVIDT20$iso_code == "OWID_OCE" | COVIDT20$iso_code=="OWID_SAM"| COVIDT20$iso_code=="OWID_UMC" | COVIDT20$iso_code=="OWID_WRL" |  COVIDT20$iso_code=="PRK"),]
COVIDT20 <- COVIDT20[ which(COVIDT20$population >= 1000000), ]

#Creating CFR
COVIDT20$CFR <- (COVIDT20$total_deaths/COVIDT20$total_cases)


options(scipen = 999)

#Week transformation (daily to weekly)
library(lubridate)
COVIDT20$date2 <- as.Date(as.character(COVIDT20$date),format="%Y-%m-%d")
COVIDT20$date2
COVIDT20$Week <- week(as.Date(as.character(COVIDT20$date2),format="%Y-%m-%d"))
COVIDT20$Week

x <- nrow(COVIDT20)
COVIDT20$Week2 <- COVIDT20$Week
for (i in 1:x) {
  if (COVIDT20$date[i] >= "2021-01-01")
    COVIDT20$Week2[i] = COVIDT20$Week[i]+53
  
}

for (i in 1:x) {
  if (COVIDT20$date[i] >= "2022-01-01")
    COVIDT20$Week2[i] = COVIDT20$Week[i]+106
  
}

print(COVIDT20$Week2)
summary(COVIDT20$Week2)


#Aggregate by weekly

world_CFR <- aggregate(COVIDT20$CFR, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_CFR

colnames(world_CFR) <- c("Weeks", "location", "CFR")
world_CFR

world_total_cases_pm <- aggregate(COVIDT20$total_cases_per_million, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_cases_pm 

colnames(world_total_cases_pm) <- c("Weeks", "location", "TotalCasepm")
world_total_cases_pm

world_new_cases <- aggregate(COVIDT20$new_cases, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_new_cases

colnames(world_new_cases) <- c("Weeks", "location", "NewCases")
world_new_cases

world_aged_65_older <- aggregate(COVIDT20$aged_65_older, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_aged_65_older

colnames(world_aged_65_older) <- c("Weeks", "location", "Age65Older")
world_aged_65_older

world_population_density <- aggregate(COVIDT20$population_density, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_population_density

colnames(world_population_density) <- c("Weeks", "location", "PopulationDensity")
world_population_density

world_total_tests_per_thousand <- aggregate(COVIDT20$total_tests_per_thousand, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_total_tests_per_thousand

colnames(world_total_tests_per_thousand) <- c("Weeks", "location", "TotalTestpt")
world_total_tests_per_thousand


world_gdp_per_capita <- aggregate(COVIDT20$gdp_per_capita, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_gdp_per_capita

colnames(world_gdp_per_capita) <- c("Weeks", "location", "GDP")
world_gdp_per_capita

world_Vac <- aggregate(COVIDT20$total_vaccinations_per_hundred, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_Vac

colnames(world_Vac) <- c("Weeks", "location", "Vaccinationph")
world_Vac

world_SI <- aggregate(COVIDT20$stringency_index, by= list(COVIDT20$Week2, COVIDT20$location), FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_SI

colnames(world_SI) <- c("Weeks", "location", "SI")
world_SI

#Merge all
ModelData1 <- merge(world_CFR, world_total_cases_pm, by=c("location", "Weeks"))

ModelData2 <- merge(ModelData1, world_new_cases, by=c("location", "Weeks"))

ModelData3 <- merge(ModelData2, world_aged_65_older, by=c("location", "Weeks"))

ModelData4 <- merge(ModelData3, world_population_density, by=c("location", "Weeks"))

ModelData5 <- merge(ModelData4, world_total_tests_per_thousand, by=c("location", "Weeks"))

ModelData6 <- merge(ModelData5, world_gdp_per_capita, by=c("location", "Weeks"))

ModelData7 <- merge(ModelData6, world_Vac, by=c("location", "Weeks"))

ModelData <- merge(ModelData7, world_SI, by=c("location", "Weeks"))

GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

Obesity <- read.csv("Obesity.csv")

#Merge all data with GHSI and WGI

finaldt1 <- merge(GHSI, WGI,  by="location")

finaldt2 <- merge(finaldt1, Obesity,  by="location")

ModelData2 <- merge(ModelData, finaldt2,  by="location")

x <- nrow(ModelData2)
ModelData2$Vaccinationph2 <- ModelData2$Vaccinationph
for (i in 1:x) {
  if (ModelData2$Weeks[i] < 49)
    ModelData2$Vaccinationph2[i] = 0
  
}
print(ModelData2)

write.csv(ModelData2,"ModelData.csv")

World <- read.csv("ModelData159.csv")

COVIDT20 <- World[order(World$Weeks,-World$Vaccinationph2),]
COVIDT20

write.csv(COVIDT20,"COVIDT20Sort159.csv")

COVIDT20 <- read.csv("COVIDT20Sort159Count.csv")

# for (i in 1:length(COVIDT20$Weeks)) { 
#   for (j in 1:length(COVIDT20$Index)) { 
#     if (COVIDT20$Index[j]<30) {
#       COVIDT20$t20 <-  1
#     } else {
#       COVIDT20$t20 <-  0 
#     }
#     }
#   }
# View(COVIDT20)
# COVIDT20$t20


data_agg <- aggregate(COVIDT20$Count, by= list(COVIDT20$location), FUN=sum, na.rm=TRUE)
data_agg

data_agg <- data_agg[order(-data_agg$x),]
data_agg

# Group.1   x
# 144         United Arab Emirates 106
# 27                         Chile 101
# 66                         Italy 101
# 120                    Singapore  98
# 112                        Qatar  97
# 10                       Bahrain  96
# 110                     Portugal  93
# 147                      Uruguay  93
# 24                        Canada  89
# 28                         China  84
# 13                       Belgium  75
# 36                       Denmark  72
# 34                          Cuba  71
# 64                       Ireland  64
# 125                  South Korea  63
# 145               United Kingdom  63
# 22                      Cambodia  61
# 65                        Israel  53
# 68                         Japan  47
# 5                      Argentina  46

##TOP20 SUMMARY
#2022

# #Top20 CFR Countries in last week
# Top20 <- subset(World, World$Weeks == 123) #1/1/2020
# 
# Top20$CFRprcnt <- Top20$CFR*100
# 
# write.csv(Top20,"Weeks103Only.csv")
# 
# Top20$CFR
# sort_Top20<- Top20[order(-Top20$CFRprcnt),]
# sort_Top20$location


#Top20 CFR Vaccination

setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022t <- read.csv("owid-covid-data.csv")


COVID2022t <- subset(COVID2022t, COVID2022t$date == "2022-12-31") #5/29/2021
COVID2022t <- COVID2022t[ which(COVID2022t$population >= 1000000), ]

#Creating CFR
COVID2022t$CFR <- (COVID2022t$total_deaths/COVID2022t$total_cases)*100
data <- COVID2022t[with(COVID2022t,order(-CFR)),]
data <- COVID2022t[with(COVID2022t,order(-total_vaccinations_per_hundred)),]

# Group.1   x
# 144         United Arab Emirates 106
# 27                         Chile 101
# 66                         Italy 101
# 120                    Singapore  98
# 112                        Qatar  97
# 10                       Bahrain  96
# 110                     Portugal  93
# 147                      Uruguay  93
# 24                        Canada  89
# 28                         China  84
# 13                       Belgium  75
# 36                       Denmark  72
# 34                          Cuba  71
# 64                       Ireland  64
# 125                  South Korea  63
# 145               United Kingdom  63
# 22                      Cambodia  61
# 65                        Israel  53
# 68                         Japan  47
# 5                      Argentina  46

COVID2022t<-COVID2022t[(COVID2022t$location=="United Arab Emirates" |
                          COVID2022t$location=="Chile" | COVID2022t$location=="Italy" |
                          COVID2022t$location=="Singapore" | COVID2022t$location=="Qatar" |
                          COVID2022t$location=="Bahrain" | COVID2022t$location=="Portugal" |
                          COVID2022t$location=="Uruguay" | COVID2022t$location=="Canada" |
                          COVID2022t$location=="China" | COVID2022t$location=="Belgium" | 
                          COVID2022t$location=="Denmark" | COVID2022t$location=="Cuba" | 
                          COVID2022t$location=="Ireland" | COVID2022t$location=="South Korea" | 
                          COVID2022t$location=="United Kingdom" | COVID2022t$location=="Cambodia" |
                          COVID2022t$location=="Israel" | COVID2022t$location=="Japan"|
                          COVID2022t$location=="Argentina" ),]
NROW(COVID2022t)
View(COVID2022t)
COVID2022t$CFR[COVID2022t$location == "Argentina"]/100
COVID2022t$total_vaccinations_per_hundred[COVID2022t$location == "Argentina"]
t.test(COVID2022t$CFR)
library("plotrix")
op <- std.error(COVID2022t$CFR)
op


t.test(COVID2022t$total_vaccinations_per_hundred)
library("plotrix")
op <- std.error(COVID2022t$total_vaccinations_per_hundred)
op

#2021
##TOP20 SUMMARY CFR Vaccination
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDt <- read.csv("owid-covid-data.csv")

COVIDt <- subset(COVIDt, COVIDt$date == "2021-01-05") #5/29/2021
COVIDt <- COVIDt[ which(COVIDt$population >= 1000000), ]

#Creating CFR
COVIDt$CFR <- (COVIDt$total_deaths/COVIDt$total_cases)*100
# data <- COVID2021[with(COVID2021,order(-CFR)),]
# data <- COVID2021[with(COVID2021,order(-total_vaccinations_per_hundred)),]

COVIDt<-COVIDt[(COVIDt$location=="United Arab Emirates" |
                          COVIDt$location=="Chile" | COVIDt$location=="Italy" |
                          COVIDt$location=="Singapore" | COVIDt$location=="Qatar" |
                          COVIDt$location=="Bahrain" | COVIDt$location=="Portugal" |
                          COVIDt$location=="Uruguay" | COVIDt$location=="Canada" |
                          COVIDt$location=="China" | COVIDt$location=="Belgium" | 
                          COVIDt$location=="Denmark" | COVIDt$location=="Cuba" | 
                          COVIDt$location=="Ireland" | COVIDt$location=="South Korea" | 
                          COVIDt$location=="United Kingdom" | COVIDt$location=="Cambodia" |
                          COVIDt$location=="Israel" | COVIDt$location=="Japan"|
                          COVIDt$location=="Argentina" ),]
COVIDt$location
str(COVIDt)
t.test(COVIDt$CFR)
library("plotrix")
op <- std.error(COVIDt$CFR)
op

t.test(COVIDt$CFR, COVID2022t$CFR, paired = TRUE, alternative = "two.sided")

#Top20 em

setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022tem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID2022tem$date <-  COVID2022tem$Day
COVID2022tem$iso_code <-  COVID2022tem$Code 
COVID2022tem$location <-  COVID2022tem$Entity


COVID2022tem <- subset(COVID2022tem, COVID2022tem$date == "2022-12-30") #5/29/2021

#Remove World and International information
COVID2022tem<-COVID2022tem[!(COVID2022tem$iso_code=="OWID_AFR" | COVID2022tem$iso_code=="OWID_ASI" | COVID2022tem$iso_code == "OWID_EUN" | COVID2022tem$iso_code=="OWID_EUR" | COVID2022tem$iso_code=="OWID_INT" | 
                             COVID2022tem$iso_code=="OWID_HIC" | COVID2022tem$iso_code=="OWID_KOS" | COVID2022tem$iso_code=="OWID_LIC" | COVID2022tem$iso_code=="OWID_LMC" | COVID2022tem$iso_code=="OWID_NAM" | 
                             COVID2022tem$iso_code == "OWID_OCE" | COVID2022tem$iso_code=="OWID_SAM"| COVID2022tem$iso_code=="OWID_UMC" | COVID2022tem$iso_code=="OWID_WRL" | COVID2022tem$iso_code=="PRK"),]

COVID2022temm <- merge(COVID2022tem, COVID2022t, by=c("location"))

COVID2022temm <- COVID2022temm[ which(COVID2022temm$population >= 1000000), ]

COVID2022temm<-COVID2022temm[(COVID2022temm$location=="United Arab Emirates" |
                                COVID2022temm$location=="Chile" | COVID2022temm$location=="Italy" |
                                COVID2022temm$location=="Singapore" | COVID2022temm$location=="Qatar" |
                                COVID2022temm$location=="Bahrain" | COVID2022temm$location=="Portugal" |
                                COVID2022temm$location=="Uruguay" | COVID2022temm$location=="Canada" |
                                COVID2022temm$location=="China" | COVID2022temm$location=="Belgium" | 
                                COVID2022temm$location=="Denmark" | COVID2022temm$location=="Cuba" | 
                                COVID2022temm$location=="Ireland" | COVID2022temm$location=="South Korea" | 
                                COVID2022temm$location=="United Kingdom" | COVID2022temm$location=="Cambodia" |
                                COVID2022temm$location=="Israel" | COVID2022temm$location=="Japan"|
                                COVID2022temm$location=="Argentina" ),]


nrow(COVID2022temm)
#Creating CFR
t.test(COVID2022temm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVID2022temm$estimated_daily_excess_deaths_per_100k)
op

#Top20 2021 EM
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDtem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVIDtem$date <-  COVIDtem$Day
COVIDtem$iso_code <-  COVIDtem$Code 
COVIDtem$location <-  COVIDtem$Entity


COVIDtem <- subset(COVIDtem, COVIDtem$date == "2021-01-04") #5/29/2021

#Remove World and International information
COVIDtem<-COVIDtem[!(COVIDtem$iso_code=="OWID_AFR" | COVIDtem$iso_code=="OWID_ASI" | COVIDtem$iso_code == "OWID_EUN" | COVIDtem$iso_code=="OWID_EUR" | COVIDtem$iso_code=="OWID_INT" | 
                               COVIDtem$iso_code=="OWID_HIC" | COVIDtem$iso_code=="OWID_KOS" | COVIDtem$iso_code=="OWID_LIC" | COVIDtem$iso_code=="OWID_LMC" | COVIDtem$iso_code=="OWID_NAM" | 
                               COVIDtem$iso_code == "OWID_OCE" | COVIDtem$iso_code=="OWID_SAM"| COVIDtem$iso_code=="OWID_UMC" | COVIDtem$iso_code=="OWID_WRL" | COVIDtem$iso_code=="PRK"),]

COVIDtemm <- merge(COVIDtem, COVIDt, by=c("location"))

COVIDtemm <- COVIDtemm[ which(COVIDtemm$population >= 1000000), ]


COVIDtemm <-COVIDtemm[(COVIDtemm$location=="United Arab Emirates" |
                         COVIDtemm$location=="Chile" | COVIDtemm$location=="Italy" |
                         COVIDtemm$location=="Singapore" | COVIDtemm$location=="Qatar" |
                         COVIDtemm$location=="Bahrain" | COVIDtemm$location=="Portugal" |
                         COVIDtemm$location=="Uruguay" | COVIDtemm$location=="Canada" |
                         COVIDtemm$location=="China" | COVIDtemm$location=="Belgium" | 
                         COVIDtemm$location=="Denmark" | COVIDtemm$location=="Cuba" | 
                         COVIDtemm$location=="Ireland" | COVIDtemm$location=="South Korea" | 
                         COVIDtemm$location=="United Kingdom" | COVIDtemm$location=="Cambodia" |
                         COVIDtemm$location=="Israel" | COVIDtemm$location=="Japan"|
                         COVIDtemm$location=="Argentina" ),]

nrow(COVIDtemm)
#Creating CFR
View(COVIDtemm)
t.test(COVIDtemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVIDtemm$estimated_daily_excess_deaths_per_100k)
op

#Creating CFR
# COVID$CFR <- (COVID$total_deaths/COVID$total_cases)*100
# data <- COVID[with(COVID,order(-CFR)),]
# data <- COVID[with(COVID,order(-total_vaccinations_per_hundred)),]



# t.test(COVID$CFR)
# t.test(COVID$total_vaccinations_per_hundred)


t.test(COVIDtemm$estimated_daily_excess_deaths_per_100k, COVID2022temm$estimated_daily_excess_deaths_per_100k, paired = TRUE, alternative = "two.sided")

#2022
##Rest World SUMMARY CFR Vaccine
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022rw <- read.csv("owid-covid-data.csv")


COVID2022rw <- subset(COVID2022rw, COVID2022rw$date == "2022-12-31") #5/29/2021
COVID2022rw <- COVID2022rw[ which(COVID2022rw$population >= 1000000), ]

#Remove World and International information
COVID2022rw<-COVID2022rw[!(COVID2022rw$iso_code=="OWID_AFR" | COVID2022rw$iso_code=="OWID_ASI" | COVID2022rw$iso_code == "OWID_EUN" | COVID2022rw$iso_code=="OWID_EUR" | COVID2022rw$iso_code=="OWID_INT" | 
                             COVID2022rw$iso_code=="OWID_HIC" | COVID2022rw$iso_code=="OWID_KOS" | COVID2022rw$iso_code=="OWID_LIC" | COVID2022rw$iso_code=="OWID_LMC" | COVID2022rw$iso_code=="OWID_NAM" | 
                             COVID2022rw$iso_code == "OWID_OCE" | COVID2022rw$iso_code=="OWID_SAM"| COVID2022rw$iso_code=="OWID_UMC" | COVID2022rw$iso_code=="OWID_WRL" | COVID2022rw$iso_code=="PRK"),]


COVID2022rw <- COVID2022rw[!(COVID2022rw$location=="United Arab Emirates" |
                               COVID2022rw$location=="Chile" | COVID2022rw$location=="Italy" |
                               COVID2022rw$location=="Singapore" | COVID2022rw$location=="Qatar" |
                               COVID2022rw$location=="Bahrain" | COVID2022rw$location=="Portugal" |
                               COVID2022rw$location=="Uruguay" | COVID2022rw$location=="Canada" |
                               COVID2022rw$location=="China" | COVID2022rw$location=="Belgium" | 
                               COVID2022rw$location=="Denmark" | COVID2022rw$location=="Cuba" | 
                               COVID2022rw$location=="Ireland" | COVID2022rw$location=="South Korea" | 
                               COVID2022rw$location=="United Kingdom" | COVID2022rw$location=="Cambodia" |
                               COVID2022rw$location=="Israel" | COVID2022rw$location=="Japan"|
                               COVID2022rw$location=="Argentina" ),]

# COVID2022rw<-COVID2022rw[(COVID2022rw$location=="Angola" | COVID2022rw$location=="Benin" | COVID2022rw$location=="Botswana" | COVID2022rw$location=="Burkina Faso" |
#                                  COVID2022rw$location=="Burundi" | COVID2022rw$location=="Cameroon" | COVID2022rw$location=="Central African Republic" | COVID2022rw$location=="Chad" |
#                                  COVID2022rw$location=="Democratic Republic of Congo" | COVID2022rw$location == "Congo" | COVID2022rw$location == "Cote d'Ivoire" | COVID2022rw$location=="Djibouti" |
#                                  COVID2022rw$location=="Equatorial Guinea" | COVID2022rw$location=="Eritrea" | COVID2022rw$location=="Ethiopia" |
#                                  COVID2022rw$location=="Gabon" | COVID2022rw$location=="Gambia" | COVID2022rw$location=="Ghana" | COVID2022rw$location=="Guinea" |
#                                  COVID2022rw$location=="Guinea-Bissau" | COVID2022rw$location == "Kenya" | COVID2022rw$location == "Lesotho" | COVID2022rw$location=="Liberia" |
#                                  COVID2022rw$location=="Madagascar" | COVID2022rw$location=="Malawi" | COVID2022rw$location=="Mali" |
#                                  COVID2022rw$location=="Mauritania" | COVID2022rw$location=="Mauritius" | COVID2022rw$location=="Mozambique" | COVID2022rw$location=="Namibia" |
#                                  COVID2022rw$location=="Niger" | COVID2022rw$location == "Nigeria" | COVID2022rw$location == "Rwanda" | COVID2022rw$location=="Senegal" |
#                                  COVID2022rw$location=="Sierra Leone" | COVID2022rw$location=="Somalia" | COVID2022rw$location=="South Africa" |
#                                  COVID2022rw$location=="South Sudan" | COVID2022rw$location=="Sudan" | COVID2022rw$location=="Tanzania" | COVID2022rw$location=="Togo" |
#                                  COVID2022rw$location=="Uganda" | COVID2022rw$location == "Zambia" | COVID2022rw$location == "Zimbabwe"),]





#Creating CFR
COVID2022rw$CFR <- (COVID2022rw$total_deaths/COVID2022rw$total_cases)*100
t.test(COVID2022rw$CFR)
library("plotrix")
op <- std.error(COVID2022rw$CFR)
op

t.test(COVID2022rw$total_vaccinations_per_hundred)

library("plotrix")
op <- std.error(COVID2022rw$total_vaccinations_per_hundred)
op


#Rest World em 2022

setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022rwem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID2022rwem$date <-  COVID2022rwem$Day
COVID2022rwem$iso_code <-  COVID2022rwem$Code 
COVID2022rwem$location <-  COVID2022rwem$Entity


COVID2022rwem <- subset(COVID2022rwem, COVID2022rwem$date == "2022-12-30") #5/29/2021

#Remove World and International information
COVID2022rwem<-COVID2022rwem[!(COVID2022rwem$iso_code=="OWID_AFR" | COVID2022rwem$iso_code=="OWID_ASI" | COVID2022rwem$iso_code == "OWID_EUN" | COVID2022rwem$iso_code=="OWID_EUR" | COVID2022rwem$iso_code=="OWID_INT" | 
                               COVID2022rwem$iso_code=="OWID_HIC" | COVID2022rwem$iso_code=="OWID_KOS" | COVID2022rwem$iso_code=="OWID_LIC" | COVID2022rwem$iso_code=="OWID_LMC" | COVID2022rwem$iso_code=="OWID_NAM" | 
                               COVID2022rwem$iso_code == "OWID_OCE" | COVID2022rwem$iso_code=="OWID_SAM"| COVID2022rwem$iso_code=="OWID_UMC" | COVID2022rwem$iso_code=="OWID_WRL" | COVID2022rwem$iso_code=="PRK"),]


COVID2022rwem <- COVID2022rwem[!(COVID2022rwem$location=="United Arab Emirates" |
                                   COVID2022rwem$location=="Chile" | COVID2022rwem$location=="Italy" |
                                   COVID2022rwem$location=="Singapore" | COVID2022rwem$location=="Qatar" |
                                   COVID2022rwem$location=="Bahrain" | COVID2022rwem$location=="Portugal" |
                                   COVID2022rwem$location=="Uruguay" | COVID2022rwem$location=="Canada" |
                                   COVID2022rwem$location=="China" | COVID2022rwem$location=="Belgium" | 
                                   COVID2022rwem$location=="Denmark" | COVID2022rwem$location=="Cuba" | 
                                   COVID2022rwem$location=="Ireland" | COVID2022rwem$location=="South Korea" | 
                                   COVID2022rwem$location=="United Kingdom" | COVID2022rwem$location=="Cambodia" |
                                   COVID2022rwem$location=="Israel" | COVID2022rwem$location=="Japan"|
                                   COVID2022rwem$location=="Argentina" ),]

# COVID2022rwem<-COVID2022rwem[!(COVID2022rwem$location=="Angola" | COVID2022rwem$location=="Benin" | COVID2022rwem$location=="Botswana" | COVID2022rwem$location=="Burkina Faso" | 
#                                 COVID2022rwem$location=="Burundi" | COVID2022rwem$location=="Cameroon" | COVID2022rwem$location=="Central African Republic" | COVID2022rwem$location=="Chad" |
#                                 COVID2022rwem$location=="Democratic Republic of Congo" | COVID2022rwem$location == "Congo" | COVID2022rwem$location == "Cote d'Ivoire" | COVID2022rwem$location=="Djibouti" | 
#                                 COVID2022rwem$location=="Equatorial Guinea" | COVID2022rwem$location=="Eritrea" | COVID2022rwem$location=="Ethiopia" | 
#                                 COVID2022rwem$location=="Gabon" | COVID2022rwem$location=="Gambia" | COVID2022rwem$location=="Ghana" | COVID2022rwem$location=="Guinea" |
#                                 COVID2022rwem$location=="Guinea-Bissau" | COVID2022rwem$location == "Kenya" | COVID2022rwem$location == "Lesotho" | COVID2022rwem$location=="Liberia" | 
#                                 COVID2022rwem$location=="Madagascar" | COVID2022rwem$location=="Malawi" | COVID2022rwem$location=="Mali" | 
#                                 COVID2022rwem$location=="Mauritania" | COVID2022rwem$location=="Mauritius" | COVID2022rwem$location=="Mozambique" | COVID2022rwem$location=="Namibia" |
#                                 COVID2022rwem$location=="Niger" | COVID2022rwem$location == "Nigeria" | COVID2022rwem$location == "Rwanda" | COVID2022rwem$location=="Senegal" | 
#                                 COVID2022rwem$location=="Sierra Leone" | COVID2022rwem$location=="Somalia" | COVID2022rwem$location=="South Africa" | 
#                                 COVID2022rwem$location=="South Sudan" | COVID2022rwem$location=="Sudan" | COVID2022rwem$location=="Tanzania" | COVID2022rwem$location=="Togo" |
#                                 COVID2022rwem$location=="Uganda" | COVID2022rwem$location == "Zambia" | COVID2022rwem$location == "Zimbabwe"),]


COVID2022rwemm <- merge(COVID2022rwem, COVID2022rw, by=c("location"))

COVID2022rwemm <- COVID2022rwemm[ which(COVID2022rwemm$population >= 1000000), ]

nrow(COVID2022rwemm)
#Creating CFR
t.test(COVID2022rwemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVID2022rwemm$estimated_daily_excess_deaths_per_100k)
op


#2021
##Rest World SUMMARY CFR Vaccine
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDrw <- read.csv("owid-covid-data.csv")


COVIDrw <- subset(COVIDrw, COVIDrw$date == "2021-01-05") #5/29/2021
COVIDrw <- COVIDrw[ which(COVIDrw$population >= 1000000), ]

#Remove World and International information
COVIDrw<-COVIDrw[!(COVIDrw$iso_code=="OWID_AFR" | COVIDrw$iso_code=="OWID_ASI" | COVIDrw$iso_code == "OWID_EUN" | COVIDrw$iso_code=="OWID_EUR" | COVIDrw$iso_code=="OWID_INT" | 
                             COVIDrw$iso_code=="OWID_HIC" | COVIDrw$iso_code=="OWID_KOS" | COVIDrw$iso_code=="OWID_LIC" | COVIDrw$iso_code=="OWID_LMC" | COVIDrw$iso_code=="OWID_NAM" | 
                             COVIDrw$iso_code == "OWID_OCE" | COVIDrw$iso_code=="OWID_SAM"| COVIDrw$iso_code=="OWID_UMC" | COVIDrw$iso_code=="OWID_WRL" | COVIDrw$iso_code=="PRK"),]


COVIDrw <- COVIDrw[!(COVIDrw$location=="United Arab Emirates" |
                       COVIDrw$location=="Chile" | COVIDrw$location=="Italy" |
                       COVIDrw$location=="Singapore" | COVIDrw$location=="Qatar" |
                       COVIDrw$location=="Bahrain" | COVIDrw$location=="Portugal" |
                       COVIDrw$location=="Uruguay" | COVIDrw$location=="Canada" |
                       COVIDrw$location=="China" | COVIDrw$location=="Belgium" | 
                       COVIDrw$location=="Denmark" | COVIDrw$location=="Cuba" | 
                       COVIDrw$location=="Ireland" | COVIDrw$location=="South Korea" | 
                       COVIDrw$location=="United Kingdom" | COVIDrw$location=="Cambodia" |
                       COVIDrw$location=="Israel" | COVIDrw$location=="Japan"|
                       COVIDrw$location==" Argentina"),]

# COVIDrw<-COVIDrw[!(COVIDrw$location=="Angola" | COVIDrw$location=="Benin" | COVIDrw$location=="Botswana" | COVIDrw$location=="Burkina Faso" | 
#                              COVIDrw$location=="Burundi" | COVIDrw$location=="Cameroon" | COVIDrw$location=="Central African Republic" | COVID2022rwem$location=="Chad" |
#                              COVIDrw$location=="Democratic Republic of Congo" | COVIDrw$location == "Congo" | COVIDrw$location == "Cote d'Ivoire" | COVID2022rwem$location=="Djibouti" | 
#                              COVIDrw$location=="Equatorial Guinea" | COVIDrw$location=="Eritrea" | COVIDrw$location=="Ethiopia" | 
#                              COVIDrw$location=="Gabon" | COVIDrw$location=="Gambia" | COVIDrw$location=="Ghana" | COVIDrw$location=="Guinea" |
#                              COVIDrw$location=="Guinea-Bissau" | COVIDrw$location == "Kenya" | COVIDrw$location == "Lesotho" | COVIDrw$location=="Liberia" | 
#                              COVIDrw$location=="Madagascar" | COVIDrw$location=="Malawi" | COVIDrw$location=="Mali" | 
#                              COVIDrw$location=="Mauritania" | COVIDrw$location=="Mauritius" | COVIDrw$location=="Mozambique" | COVIDrw$location=="Namibia" |
#                              COVIDrw$location=="Niger" | COVIDrw$location == "Nigeria" | COVIDrw$location == "Rwanda" | COVIDrw$location=="Senegal" | 
#                              COVIDrw$location=="Sierra Leone" | COVIDrw$location=="Somalia" | COVIDrw$location=="South Africa" | 
#                              COVIDrw$location=="South Sudan" | COVIDrw$location=="Sudan" | COVIDrw$location=="Tanzania" | COVIDrw$location=="Togo" |
#                              COVIDrw$location=="Uganda" | COVIDrw$location == "Zambia" | COVIDrw$location == "Zimbabwe"),]
# 



#Creating CFR
COVIDrw$CFR <- (COVIDrw$total_deaths/COVIDrw$total_cases)*100
t.test(COVIDrw$CFR)
library("plotrix")
op <- std.error(COVIDrw$CFR)
op

COVIDrw$CFR[is.na(COVIDrw$CFR)] <- 0

t.test(COVIDrw$CFR[1:136], COVID2022rwemm$CFR, paired = TRUE, alternative = "two.sided")

#Rest World em 2021

setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDrwem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVIDrwem$date <-  COVIDrwem$Day
COVIDrwem$iso_code <-  COVIDrwem$Code 
COVIDrwem$location <-  COVIDrwem$Entity


COVIDrwem <- subset(COVIDrwem, COVIDrwem$date == "2021-01-04") #5/29/2021

#Remove World and International information
COVIDrwem<-COVIDrwem[!(COVIDrwem$iso_code=="OWID_AFR" | COVIDrwem$iso_code=="OWID_ASI" | COVIDrwem$iso_code == "OWID_EUN" | COVIDrwem$iso_code=="OWID_EUR" | COVIDrwem$iso_code=="OWID_INT" | 
                                 COVIDrwem$iso_code=="OWID_HIC" | COVIDrwem$iso_code=="OWID_KOS" | COVIDrwem$iso_code=="OWID_LIC" | COVIDrwem$iso_code=="OWID_LMC" | COVIDrwem$iso_code=="OWID_NAM" | 
                                 COVIDrwem$iso_code == "OWID_OCE" | COVIDrwem$iso_code=="OWID_SAM"| COVIDrwem$iso_code=="OWID_UMC" | COVIDrwem$iso_code=="OWID_WRL" | COVIDrwem$iso_code=="PRK"),]


COVIDrwem <- COVIDrwem[!(COVIDrwem$location=="United Arab Emirates" |
                       COVIDrwem$location=="Chile" | COVIDrwem$location=="Italy" |
                       COVIDrwem$location=="Singapore" | COVIDrwem$location=="Qatar" |
                       COVIDrwem$location=="Bahrain" | COVIDrwem$location=="Portugal" |
                       COVIDrwem$location=="Uruguay" | COVIDrwem$location=="Canada" |
                       COVIDrwem$location=="China" | COVIDrwem$location=="Belgium" | 
                       COVIDrwem$location=="Denmark" | COVIDrwem$location=="Cuba" | 
                       COVIDrwem$location=="Ireland" | COVIDrwem$location=="South Korea" | 
                       COVIDrwem$location=="United Kingdom" | COVIDrwem$location=="Cambodia" |
                       COVIDrwem$location=="Israel" | COVIDrwem$location=="Japan"|
                       COVIDrwem$location=="Argentina"),]

# COVIDrwem<-COVIDrwem[!(COVIDrwem$location=="Angola" | COVIDrwem$location=="Benin" | COVIDrwem$location=="Botswana" | COVIDrwem$location=="Burkina Faso" | 
#                                  COVIDrwem$location=="Burundi" | COVIDrwem$location=="Cameroon" | COVIDrwem$location=="Central African Republic" | COVIDrwem$location=="Chad" |
#                                  COVIDrwem$location=="Democratic Republic of Congo" | COVIDrwem$location == "Congo" | COVIDrwem$location == "Cote d'Ivoire" | COVIDrwem$location=="Djibouti" | 
#                                  COVIDrwem$location=="Equatorial Guinea" | COVIDrwem$location=="Eritrea" | COVIDrwem$location=="Ethiopia" | 
#                                  COVIDrwem$location=="Gabon" | COVIDrwem$location=="Gambia" | COVIDrwem$location=="Ghana" | COVIDrwem$location=="Guinea" |
#                                  COVIDrwem$location=="Guinea-Bissau" | COVIDrwem$location == "Kenya" | COVIDrwem$location == "Lesotho" | COVIDrwem$location=="Liberia" | 
#                                  COVIDrwem$location=="Madagascar" | COVIDrwem$location=="Malawi" | COVIDrwem$location=="Mali" | 
#                                  COVIDrwem$location=="Mauritania" | COVIDrwem$location=="Mauritius" | COVIDrwem$location=="Mozambique" | COVIDrwem$location=="Namibia" |
#                                  COVIDrwem$location=="Niger" | COVIDrwem$location == "Nigeria" | COVIDrwem$location == "Rwanda" | COVIDrwem$location=="Senegal" | 
#                                  COVIDrwem$location=="Sierra Leone" | COVIDrwem$location=="Somalia" | COVIDrwem$location=="South Africa" | 
#                                  COVIDrwem$location=="South Sudan" | COVIDrwem$location=="Sudan" | COVIDrwem$location=="Tanzania" | COVIDrwem$location=="Togo" |
#                                  COVIDrwem$location=="Uganda" | COVIDrwem$location == "Zambia" | COVIDrwem$location == "Zimbabwe"),]
# 


COVIDrwemm <- merge(COVIDrwem, COVIDrw, by=c("location"))

COVIDrwemm <- COVIDrwemm[ which(COVIDrwemm$population >= 1000000), ]

nrow(COVIDrwemm)
#Creating CFR
t.test(COVIDrwemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVIDrwemm$estimated_daily_excess_deaths_per_100k)
op

t.test(COVIDrwemm$estimated_daily_excess_deaths_per_100k[1:136], COVID2022rwemm$estimated_daily_excess_deaths_per_100k, paired = TRUE, alternative = "two.sided")






#2022
#SSA
##Descriptive CFR 
setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022ssa <- read.csv("owid-covid-data.csv")

#Remove World and International information
COVID2022ssa<-COVID2022ssa[(COVID2022ssa$location=="Angola" | COVID2022ssa$location=="Benin" | COVID2022ssa$location=="Botswana" | COVID2022ssa$location=="Burkina Faso" | 
                COVID2022ssa$location=="Burundi" | COVID2022ssa$location=="Cameroon" | COVID2022ssa$location=="Central African Republic" | COVID2022ssa$location=="Chad" |
                COVID2022ssa$location=="Democratic Republic of Congo" | COVID2022ssa$location == "Congo" | COVID2022ssa$location == "Cote d'Ivoire" | COVID2022ssa$location=="Djibouti" | 
                COVID2022ssa$location=="Equatorial Guinea" | COVID2022ssa$location=="Eritrea" | COVID2022ssa$location=="Ethiopia" | 
                COVID2022ssa$location=="Gabon" | COVID2022ssa$location=="Gambia" | COVID2022ssa$location=="Ghana" | COVID2022ssa$location=="Guinea" |
                COVID2022ssa$location=="Guinea-Bissau" | COVID2022ssa$location == "Kenya" | COVID2022ssa$location == "Lesotho" | COVID2022ssa$location=="Liberia" | 
                COVID2022ssa$location=="Madagascar" | COVID2022ssa$location=="Malawi" | COVID2022ssa$location=="Mali" | 
                COVID2022ssa$location=="Mauritania" | COVID2022ssa$location=="Mauritius" | COVID2022ssa$location=="Mozambique" | COVID2022ssa$location=="Namibia" |
                COVID2022ssa$location=="Niger" | COVID2022ssa$location == "Nigeria" | COVID2022ssa$location == "Rwanda" | COVID2022ssa$location=="Senegal" | 
                COVID2022ssa$location=="Sierra Leone" | COVID2022ssa$location=="Somalia" | COVID2022ssa$location=="South Africa" | 
                COVID2022ssa$location=="South Sudan" | COVID2022ssa$location=="Sudan" | COVID2022ssa$location=="Tanzania" | COVID2022ssa$location=="Togo" |
                COVID2022ssa$location=="Uganda" | COVID2022ssa$location == "Zambia" | COVID2022ssa$location == "Zimbabwe"),]

write.csv(COVID2022ssa,"COVID_SSA.csv")

COVID2022ssa <- read.csv("COVID_SSAU.csv") # Updated with previous reported data

View(COVID2022ssa)

COVID2022ssa <- subset(COVID2022ssa, COVID2022ssa$date == "2022-12-31") #5/29/2021
COVID2022ssa <- COVID2022ssa[ which(COVID2022ssa$population >= 1000000), ]
COVID2022ssa$total_vaccinations_per_hundred
#COVID <- read.csv("COVID_SSAU.csv")

#Creating CFR
COVID2022ssa$CFR <- (COVID2022ssa$total_deaths/COVID2022ssa$total_cases)*100

NROW(COVID2022ssa$CFR)

t.test(COVID2022ssa$CFR)
library("plotrix")
op <- std.error(COVID2022ssa$CFR)
op

t.test(COVID2022ssa$total_vaccinations_per_hundred)
library("plotrix")
op <- std.error(COVID2022ssa$total_vaccinations_per_hundred)
op



setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVID2022ssaem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID2022ssaem$date <-  COVID2022ssaem$Day
COVID2022ssaem$iso_code <-  COVID2022ssaem$Code 
COVID2022ssaem$location <-  COVID2022ssaem$Entity


COVID2022ssaem <- subset(COVID2022ssaem, COVID2022ssaem$date == "2022-12-30") #5/29/2021

#Remove World and International information
COVID2022ssaem<-COVID2022ssaem[(COVID2022ssaem$location=="Angola" |COVID2022ssaem$location=="Benin" |COVID2022ssaem$location=="Botswana" | COVID2022ssaem$location=="Burkina Faso" | 
                                  COVID2022ssaem$location=="Burundi" | COVID2022ssaem$location=="Cameroon" | COVID2022ssaem$location=="Central African Republic" | COVID2022ssaem$location=="Chad" |
                                  COVID2022ssaem$location=="Democratic Republic of Congo" | COVID2022ssaem$location == "Congo" | COVID2022ssaem$location == "Cote d'Ivoire" | COVID2022ssaem$location=="Djibouti" | 
                                  COVID2022ssaem$location=="Equatorial Guinea" | COVID2022ssaem$location=="Eritrea" | COVID2022ssaem$location=="Ethiopia" | 
                                  COVID2022ssaem$location=="Gabon" | COVID2022ssaem$location=="Gambia" | COVID2022ssaem$location=="Ghana" | COVID2022ssaem$location=="Guinea" |
                                  COVID2022ssaem$location=="Guinea-Bissau" | COVID2022ssaem$location == "Kenya" | COVID2022ssaem$location == "Lesotho" | COVID2022ssaem$location=="Liberia" | 
                                  COVID2022ssaem$location=="Madagascar" | COVID2022ssaem$location=="Malawi" |COVID2022ssaem$location=="Mali" | 
                                  COVID2022ssaem$location=="Mauritania" | COVID2022ssaem$location=="Mauritius" |COVID2022ssaem$location=="Mozambique" | COVID2022ssaem$location=="Namibia" |
                                  COVID2022ssaem$location=="Niger" | COVID2022ssaem$location == "Nigeria" | COVID2022ssaem$location == "Rwanda" | COVID2022ssaem$location=="Senegal" | 
                                  COVID2022ssaem$location=="Sierra Leone" | COVID2022ssaem$location=="Somalia" | COVID2022ssaem$location=="South Africa" | 
                                  COVID2022ssaem$location=="South Sudan" | COVID2022ssaem$location=="Sudan" | COVID2022ssaem$location=="Tanzania" | COVID2022ssaem$location=="Togo" |
                                  COVID2022ssaem$location=="Uganda" | COVID2022ssaem$location == "Zambia" | COVID2022ssaem$location == "Zimbabwe"),]



COVID2022ssaemm <- merge(COVID2022ssaem, COVID2022ssa, by=c("location"))

COVID2022ssaemm <- COVID2022ssaemm[ which(COVID2022ssaemm$population >= 1000000), ]
COVID2022ssaemm
nrow(COVID2022ssaemm)
#Creating CFR
t.test(COVID2022ssaemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVID2022ssaemm$estimated_daily_excess_deaths_per_100k)
op


#2021
#SSA
##Descriptive CFR 
setwd('D:\\Update3\\Update4\\Update5\\Update6')

COVIDssa <- read.csv("COVID_SSAU.csv") # Updated with previous reported data

COVIDssa <- subset(COVIDssa, COVIDssa$date == "2021-01-05") #5/29/2021
COVIDssa <- COVIDssa[which(COVIDssa$population >= 1000000), ]

#Remove World and International information
COVIDssa<-COVIDssa[(COVIDssa$location=="Angola" | COVIDssa$location=="Benin" | COVIDssa$location=="Botswana" | COVIDssa$location=="Burkina Faso" | 
                              COVIDssa$location=="Burundi" | COVIDssa$location=="Cameroon" | COVIDssa$location=="Central African Republic" | COVIDssa$location=="Chad" |
                              COVIDssa$location=="Democratic Republic of Congo" | COVIDssa$location == "Congo" | COVIDssa$location == "Cote d'Ivoire" | COVIDssa$location=="Djibouti" | 
                              COVIDssa$location=="Equatorial Guinea" | COVIDssa$location=="Eritrea" | COVIDssa$location=="Ethiopia" | 
                              COVIDssa$location=="Gabon" | COVIDssa$location=="Gambia" | COVIDssa$location=="Ghana" | COVIDssa$location=="Guinea" |
                              COVIDssa$location=="Guinea-Bissau" | COVIDssa$location == "Kenya" | COVIDssa$location == "Lesotho" | COVIDssa$location=="Liberia" | 
                              COVIDssa$location=="Madagascar" | COVIDssa$location=="Malawi" | COVIDssa$location=="Mali" | 
                              COVIDssa$location=="Mauritania" | COVIDssa$location=="Mauritius" | COVIDssa$location=="Mozambique" | COVIDssa$location=="Namibia" |
                              COVIDssa$location=="Niger" | COVIDssa$location == "Nigeria" | COVIDssa$location == "Rwanda" | COVIDssa$location=="Senegal" | 
                              COVIDssa$location=="Sierra Leone" | COVIDssa$location=="Somalia" | COVIDssa$location=="South Africa" | 
                              COVIDssa$location=="South Sudan" | COVIDssa$location=="Sudan" | COVIDssa$location=="Tanzania" | COVIDssa$location=="Togo" |
                              COVIDssa$location=="Uganda" | COVIDssa$location == "Zambia" | COVIDssa$location == "Zimbabwe"),]






#COVID <- read.csv("COVID_SSAU.csv")

#Creating CFR
COVIDssa$CFR <- (COVIDssa$total_deaths/COVIDssa$total_cases)*100

t.test(COVIDssa$CFR)
library("plotrix")
op <- std.error(COVIDssa$CFR)
op


t.test(COVIDssa$CFR, COVID2022ssa$CFR, paired = TRUE, alternative = "two.sided")





setwd('D:\\Update3\\Update4\\Update5\\Update6')
COVIDssaem <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVIDssaem$date <-  COVIDssaem$Day
COVIDssaem$iso_code <-  COVIDssaem$Code 
COVIDssaem$location <-  COVIDssaem$Entity

COVIDssaem <- subset(COVIDssaem, COVIDssaem$date == "2021-01-04") #5/29/2021

#Remove World and International information
COVIDssaem<-COVIDssaem[(COVIDssaem$location=="Angola" |COVIDssaem$location=="Benin" |COVIDssaem$location=="Botswana" | COVIDssaem$location=="Burkina Faso" | 
                                  COVIDssaem$location=="Burundi" | COVIDssaem$location=="Cameroon" | COVIDssaem$location=="Central African Republic" | COVIDssaem$location=="Chad" |
                                  COVIDssaem$location=="Democratic Republic of Congo" | COVIDssaem$location == "Congo" | COVIDssaem$location == "Cote d'Ivoire" | COVIDssaem$location=="Djibouti" | 
                                  COVIDssaem$location=="Equatorial Guinea" | COVIDssaem$location=="Eritrea" | COVIDssaem$location=="Ethiopia" | 
                                  COVIDssaem$location=="Gabon" | COVIDssaem$location=="Gambia" | COVIDssaem$location=="Ghana" | COVIDssaem$location=="Guinea" |
                                  COVIDssaem$location=="Guinea-Bissau" | COVIDssaem$location == "Kenya" | COVIDssaem$location == "Lesotho" | COVIDssaem$location=="Liberia" | 
                                  COVIDssaem$location=="Madagascar" | COVIDssaem$location=="Malawi" |COVIDssaem$location=="Mali" | 
                                  COVIDssaem$location=="Mauritania" | COVIDssaem$location=="Mauritius" |COVIDssaem$location=="Mozambique" | COVIDssaem$location=="Namibia" |
                                  COVIDssaem$location=="Niger" | COVIDssaem$location == "Nigeria" | COVIDssaem$location == "Rwanda" | COVIDssaem$location=="Senegal" | 
                                  COVIDssaem$location=="Sierra Leone" | COVIDssaem$location=="Somalia" | COVIDssaem$location=="South Africa" | 
                                  COVIDssaem$location=="South Sudan" | COVIDssaem$location=="Sudan" | COVIDssaem$location=="Tanzania" | COVIDssaem$location=="Togo" |
                                  COVIDssaem$location=="Uganda" | COVIDssaem$location == "Zambia" | COVIDssaem$location == "Zimbabwe"),]



COVIDssaemm <- merge(COVIDssaem, COVIDssa, by=c("location"))

COVIDssaemm <- COVIDssaemm[ which(COVIDssaemm$population >= 1000000), ]

nrow(COVIDssaemm)
#Creating CFR
t.test(COVIDssaemm$estimated_daily_excess_deaths_per_100k)
library("plotrix")
op <- std.error(COVIDssaemm$estimated_daily_excess_deaths_per_100k)
op


t.test(COVIDssaemm$estimated_daily_excess_deaths_per_100k, COVID2022ssaemm$estimated_daily_excess_deaths_per_100k, paired = TRUE, alternative = "two.sided")

#GLMM
#Data Management

World <- read.csv("ModelData159.csv")
#GLMM Model

library(glmmTMB)
World$Vaccinationph2
World$vml <- World$Vaccinationph2
World$vml2 <- scale(World$Vaccinationph2)

World$lw <- scale(World$Weeks)

World$TTs <- World$TotalTestpt
World$TTs2 <- scale(World$TTs)

World$GDP2 <- scale(World$GDP)

World$PopulationDensity2 <- World$PopulationDensity
World$PopulationDensity22 <- scale(World$PopulationDensity2)

World$GHSI2 <- scale(World$GHSI)

World$WGI2 <- scale(World$WGI)

World$Age65Older2 <- World$Age65Older
World$Age65Older22 <- scale(World$Age65Older2)

World$Obesity_rate2 <- World$Obesity_rate
World$Obesity_rate22 <- scale(World$Obesity_rate2)

World$CFR2 <- World$CFR/100
World$CFR22 <- scale(World$CFR2)

World$SI2 <- scale(World$SI)

World$location <- as.factor(World$location)
nrow(World)

library(glmmTMB)
library(DHARMa)
library(performance)

summary(World$CFR2)
World$CFR2[World$CFR2 == 1] <- NA
World$CFR2[World$CFR2 == 0] <- NA

f <- subset(World, !is.na(World$TotalTestpt))
f
f <- factor(f$location)
f
World$CFR
fit <- glmmTMB(CFR2~ vml2  +Age65Older22 + PopulationDensity22 
               + scale(TotalTestpt) + GDP2 + GHSI2 
               + WGI2 + Obesity_rate22 + SI2 + lw + (1|location) + (1| lw), na.action=na.omit, family = beta_family(link = "logit"),  data = World)


library(car)
summary(fit)

check_collinearity(fit)
round(exp(confint(fit)),3)
options(scipen = 999)
performance::performance(fit)
performance::performance(fit)$AIC
performance::performance(fit)$BIC
performance::performance(fit)$RMSE

tiff("CFRD.tiff", units="in", width=8, height=8, res=300)

CM <- subset(World$CFR2, !is.na(World$CFR2))
e <- density(CM) # returns the density data
plot(e,main="Density plot of reported case-fatality rate",col="red",xlim = c(0, 0.00165),ylim = c(0, 3700),lwd = 2, xlab("Case-fatality rate"))

d <- density(fitted(fit)) # returns the density data
lines(d, col="green",lwd = 2)

legend("topright",                                  # Add legend to density
       legend = c("Observed", "Predicted"),
       col = c("red", "green"),
       lty = 1,lwd = 2)

dev.off()



#Time series


#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

COVID <- read.csv("owid-covid-data.csv")

COVID <- subset(COVID, COVID$date >= "2020-01-01") #1/1/2020
COVID <- subset(COVID, COVID$date <= "2022-12-31") #5/29/2021

# World information
COVID_WRL<-COVID[(COVID$iso_code=="OWID_WRL"),]
str(COVID_WRL)
#Creating CFR
COVID_WRL$CFR <- (COVID_WRL$total_deaths/COVID_WRL$total_cases)*100
COVID_WRL$CFR
NROW(COVID_WRL$CFR)
myts <- ts(COVID_WRL$CFR,frequency=365, start=c(2020,6))

auto.arima(myts)
Fit<-Arima(myts,order=c(4,1,3))
summary(Fit)
fcast <- forecast(Fit, h=10)

z <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Months") + ylab("Reported CFR (%)") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
z

#R2
SSE <- sum((resid(Fit[1:1075]))^2)
SST <- sum((COVID_WRL$CFR[1:1075] - mean(COVID_WRL$CFR[1:1075]))^2)
R_square <- 1 - SSE / SST
R_square


####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(myts,  
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank()) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
x

# library(ggplot2)
# library(ggpubr)
# theme_set(theme_pubr())
# figure <- ggarrange(y, x, z,
#                     labels = c("A", "", ""),
#                     ncol = 1, nrow = 3)
# figure


#R2
SSE <- sum((resid(ses.goog[1:1075]))^2)
SST <- sum((COVID_WRL$CFR[1:1075] - mean(COVID_WRL$CFR[1:1075]))^2)
R_square <- 1 - SSE / SST
R_square


#Prophet
library(prophet)

history <- data.frame(ds = seq(as.Date('2020-01-22'), as.Date('2022-12-31'), by = 'd'),
                      y = COVID_WRL$CFR)
NROW(COVID_WRL$CFR)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
summary(m3)
y <-plot(m3, fcst3, xlab="Months", ylab="Reported CFR (%)") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12), axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank())
plot(y)



SSE <- sum((history$y[1:1075] - fcst3$yhat[c(1:1075)])^2)
SST <- sum((history$y[1:1075] - mean(history$y[1:1075]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[1075,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:1075)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:1075)])))
final <- cbind(last_fcst3, rmse, mae)
final


tiff("CFR.tiff", units="in", width=4, height=8, res=300)
gridExtra::grid.arrange(y,x,z)
dev.off()

#Menn kendal
library(Kendall)
library(trend)
world_CFR <- aggregate(ModelData$CFR, by= list(ModelData$Weeks), FUN=mean, na.rm=TRUE)

myts <- ts(world_CFR$x[4:159])
t.test(world_CFR$x[4:159])$"conf.int"
mean(world_CFR$x[4:159])

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)





#GLMM Excess mortality
#Data Management
setwd('D:\\Update3\\Update4\\Update5\\Update6')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID$date <-  COVID$Day
COVID$iso_code <-  COVID$Code 
COVID$location <-  COVID$Entity

#Week transformation (daily to weekly)
library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week

x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
    COVID$Week2[i] = COVID$Week[i]+53
  
}

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+106
  
}

print(COVID$Week2)
summary(COVID$Week2)

world_em <- aggregate(COVID$estimated_daily_excess_deaths_per_100k, by= list(COVID$Week2, COVID$location), FUN=sum, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_em

colnames(world_em) <- c("Weeks", "location", "EM")
world_em

# COVID2 <- read.csv("owid-covid-data.csv")
World <- read.csv("ModelData159.csv")

COVID <- merge(world_em, World, by=c("location", "Weeks"))

# #Remove World and International information
# COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code == "OWID_EUN" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
#                  COVID$iso_code=="OWID_HIC" | COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_LIC" | COVID$iso_code=="OWID_LMC" | COVID$iso_code=="OWID_NAM" | 
#                  COVID$iso_code == "OWID_OCE" | COVID$iso_code=="OWID_SAM"| COVID$iso_code=="OWID_UMC" | COVID$iso_code=="OWID_WRL"),]
# COVID <- COVID[ which(COVID$population >= 1000000), ]
# #Creating CFR
# # COVID$CFR <- (COVID$estimated_daily_excess_deaths/COVID$new_cases)

options(scipen = 999)

#GLMM Model

World <- COVID



library(glmmTMB)
World$Vaccinationph2
World$vml <- World$Vaccinationph2
World$vml2 <- scale(World$Vaccinationph2)

World$lw <- scale(World$Weeks)

World$TTs <- World$TotalTestpt
World$TTs2 <- scale(World$TTs)

World$GDP2 <- scale(World$GDP)

World$PopulationDensity2 <- World$PopulationDensity
World$PopulationDensity22 <- scale(World$PopulationDensity2)

World$GHSI2 <- scale(World$GHSI)

World$WGI2 <- scale(World$WGI)

World$Age65Older2 <- World$Age65Older
World$Age65Older22 <- scale(World$Age65Older2)

World$Obesity_rate2 <- World$Obesity_rate
World$Obesity_rate22 <- scale(World$Obesity_rate2)

# World$EMs <- scale(World$EM)

World$SI2 <- scale(World$SI)


library(glmmTMB)
library(DHARMa)
library(performance)



World$EMs <- (World$EM-min(World$EM))/(max(World$EM)-min(World$EM))
World$EMs
World$EMs[World$EMs == 0] <- NA
World$EMs[World$EMs == 1] <- NA

fit <- glmmTMB(EMs~ vml2 + Age65Older22 + PopulationDensity22 
               + TTs2 + GDP2 + GHSI2 
               + WGI2 + Obesity_rate22 + SI2 + lw + (1|location) + (1| lw), na.action=na.omit, family = beta_family(link = "logit"), data = World)
library(car)
nobs(fit)
summary(fit)

check_collinearity(fit)

tiff("EMD.tiff", units="in", width=8, height=8, res=300)

CM <- subset(World$EMs, !is.na(World$EMs))
e <- density(CM) # returns the density data
plot(e,main="Density plot of excess mortality",col="red",xlim = c(0, 0.5),lwd = 2, xlab("Excess mortality"))


d <- density(fitted(fit)) # returns the density data
lines(d, col="green",lwd = 2)

legend("topright",                                  # Add legend to density
       legend = c("Observed", "Predicted"),
       col = c("red", "green"),
       lty = 1,lwd = 2)

dev.off()

#MAP EM


World <- read.csv("ModelData159.csv")

COVID2022 <- merge(world_em, World, by=c("location", "Weeks"))
summary(world_em)
COVID2022 <- subset(COVID2022, COVID2022$Weeks == 159) #5/29/20

COVID2022$location[COVID2022$location == 'United States'] <- 'USA'
COVID2022$location[COVID2022$location == 'United Kingdom'] <- 'UK'
COVID2022$location[COVID2022$location == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
COVID2022$location[COVID2022$location == 'Congo'] <- 'Republic of Congo'


library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")

worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID2022$emlog <- log10(COVID2022$EM)+1
COVID2022$emlog[COVID2022$emlog == 'NaN'] <- 0


worldgovt <- dplyr::select(COVID2022, region = location, emlog = emlog, "CC" =  ï..iso_code)
head(worldgovt)




#diff <- setdiff(world$region, worldgovt$region)

# ## Clean the dataset accordingly
# worldgovt <- worldgovt %>%
#   mutate(region = recode(str_trim(region), "United States" = "USA",
#                          "United Kingdom" = "UK",
#                          "Korea (Rep.)" = "South Korea",
#                          "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
#                          "Congo (Rep.)" = "Republic of Congo"))

#worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$em <- as.numeric(as.character(worldgovt$emlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = em)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Excess mortality") +
  plain
x <- plot(worldCFR)
x


####MAP Vaccination

COVID22 <- subset(COVID2022, COVID2022$Weeks == 159) #5/29/2021
#Remove World and International information
# COVID22<-COVID22[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code == "OWID_EUN" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
#                      COVID$iso_code=="OWID_HIC" | COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_LIC" | COVID$iso_code=="OWID_LMC" | COVID$iso_code=="OWID_NAM" | 
#                      COVID$iso_code == "OWID_OCE" | COVID$iso_code=="OWID_SAM"| COVID$iso_code=="OWID_UMC" | COVID$iso_code=="OWID_WRL"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID22$cfrlog <- log10(COVID22$Vaccinationph2)+1

worldgovt <- dplyr::select(COVID22, region = location, cfrlog = cfrlog, "CC" =  ï..iso_code)
head(worldgovt)

# diff <- setdiff(world$region, worldgovt$region)
# 
# ## Clean the dataset accordingly
# worldgovt <- worldgovt %>% 
#   mutate(region = recode(str_trim(region), "United States" = "USA",
#                          "United Kingdom" = "UK",
#                          "Korea (Rep.)" = "South Korea",
#                          "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
#                          "Congo (Rep.)" = "Republic of Congo")) %>%
#   ## Editing the "North Korea" entry is a little trickier for some reason
#   mutate(region = case_when((CC == "PRK") ~ "North Korea",
#                             TRUE ~ as.character(.$region)))
# 
# worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Vaccination \n(doses/100 people)") +
  plain
y <- plot(worldCFR)
y




#MAP CFR 

# setwd('E:\\Update3')
# COVID22 <- read.csv("ModelData123.csv")
# COVID22$Weeks
# 
# COVID22$Weeks <- subset(COVID22, COVID22$Weeks == 123) #5/29/2021
#Remove World and International information
# COVID22<-COVID22[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code == "OWID_EUN" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
#                      COVID$iso_code=="OWID_HIC" | COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_LIC" | COVID$iso_code=="OWID_LMC" | COVID$iso_code=="OWID_NAM" | 
#                      COVID$iso_code == "OWID_OCE" | COVID$iso_code=="OWID_SAM"| COVID$iso_code=="OWID_UMC" | COVID$iso_code=="OWID_WRL"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID2022$cfrlog <- log10(COVID22$CFR*100) +1

worldgovt <- dplyr::select(COVID2022, region = location, cfrlog = cfrlog, "CC" =  ï..iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

# ## Clean the dataset accordingly
# worldgovt <- worldgovt %>% 
#   mutate(region = recode(str_trim(region), "United States" = "USA",
#                          "United Kingdom" = "UK",
#                          "Korea (Rep.)" = "South Korea",
#                          "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
#                          "Congo (Rep.)" = "Republic of Congo")) %>%
#   ## Editing the "North Korea" entry is a little trickier for some reason
#   mutate(region = case_when((CC == "PRK") ~ "North Korea",
#                             TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Reported CFR (%)") +
  plain
z <- plot(worldCFR)
z


tiff("Map.tiff", units="in", width=6, height=6, res=300)
g <- gridExtra::grid.arrange(y,z,x)
ggsave(file="Map.pdf", g,width = 6, height = 6, dpi = 300, units = "in") #saves g

dev.off()



cor (COVID22$CFR , COVID22$Vaccinationph2, method = c("pearson"), use = "complete.obs")
cor.test(COVID22$CFR , COVID22$Vaccinationph2, method = c("pearson"), use = "complete.obs")

cor (COVID22$EM , COVID22$Vaccinationph2, method = c("pearson"), use = "complete.obs")
cor.test(COVID22$EM , COVID22$Vaccinationph2, method = c("pearson"), use = "complete.obs")

#2022
##TOP20 SUMMARY
COVID <- read.csv("ModelData159.csv")

COVID <- subset(COVID, COVID$Weeks == 159) #5/29/2021

data <- COVID[with(COVID,order(-CFR)),]
data$location
data$CFR
data <- COVID[with(COVID,order(-Vaccinationph2)),]
data$location
# data <- COVID[with(COVID,order(-EM)),]
# data$location
# data$EM
COVID <-COVID[(COVID$location=="United Arab Emirates" |
                 COVID$location=="Chile" | COVID$location=="Italy" |
                 COVID$location=="Singapore" | COVID$location=="Qatar" |
                 COVID$location=="Bahrain" | COVID$location=="Portugal" |
                 COVID$location=="Uruguay" | COVID$location=="Canada" |
                 COVID$location=="China" | COVID$location=="Belgium" | 
                 COVID$location=="Denmark" | COVID$location=="Cuba" | 
                 COVID$location=="Ireland" | COVID$location=="South Korea" | 
                 COVID$location=="United Kingdom" | COVID$location=="Cambodia" |
                 COVID$location=="Israel" | COVID$location=="Japan"|
                 COVID$location=="Portugal"),]


COVID$location
COVID$CFR
COVID$Vaccinationph2


library(plotly)
library(dplyr)

library(ggplot2)
library(tidyr)
library(dplyr)

x <- c("Bahrain",              "Belgium"       ,       "Cambodia"    ,         "Canada"     ,          "Chile"      ,         
       "China"  ,              "Cuba"            ,     "Denmark"      ,        "Ireland"       ,       "Israel"       ,       
       "Italy"    ,            "Japan"          ,      "Portugal"      ,       "Qatar"          ,      "Singapore"       ,    
       "South Korea"  ,        "United Arab Emirates" ,"United Kingdom"  ,     "Uruguay" , "Argentina")
rCFR <- c(0.002202654, 0.007117874, 0.022052085, 0.010929274 ,0.012544443, 0.002687230 ,0.007670984 ,0.002291310,
          0.004913881, 0.002527070, 0.007343468,
          0.001957713, 0.004629768, 0.001399730, 0.000777093, 0.001106555, 0.002242750, 0.008835644 ,0.007472421, 0.01315561)*100
Vaccination <- c(236.1500 ,253.8200 ,268.9800, 250.4250, 319.4500, 243.9000, 380.6980, 223.8600 ,220.8250, 196.9000, 
                 242.8600, 299.9800, 270.3000, 282.0400,
                261.1960, 250.2075, 263.9700, 224.0267, 256.3500,247.41)
data <- data.frame(x,rCFR,Vaccination)


# > edit(data)
# x      rCFR Vaccination
# 1               Bahrain 0.2571374     196.630
# 2               Belgium 0.7698530     217.750
# 3                Canada 1.0457357     222.130
# 4                 Chile 1.6086617     279.240
# 5               Denmark 0.2000705     227.390
# 6               Finland 0.3941993     211.930
# 7                France 0.5057469     213.320
# 8               Germany 0.5371222     213.560
# 9               Ireland 0.4692509     218.420
# 10               Israel 0.2623091     195.250
# 11                Italy 0.9763215     227.220
# 12               Norway 0.2104012     206.860
# 13             Portugal 0.5659659     234.280
# 14                Qatar 0.1852283     231.300
# 15            Singapore 0.1106917     257.110
# 16                Spain 0.8717470     202.130
# 17               Sweden 0.7520305     217.240
# 18 United Arab Emirates 0.2555608     247.270
# 19       United Kingdom 0.7958403     208.672
# 20              Uruguay 0.7991890     236.010

data$x <- factor(data$x, levels = data[["x"]])

datalong <- data |> pivot_longer(cols = -x,names_to = "Type") |> 
  mutate(scaled_value=ifelse(Type=="rCFR",value,value/100))
head(datalong)


p <- ggplot(datalong,aes(x=reorder(x, -value), y = scaled_value,fill= Type)) + 
  geom_col(position="dodge") + 
  scale_y_continuous(sec.axis = sec_axis(~ . * 100, name = "Vaccination \n(doses/100 people)"))+
  labs(y="Reported CFR (%)") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p <- p + labs( x = "Top-20 vaccinated countries ")
p <- p + labs(fill = " ")
p

tiff("Top.tiff", units="in", width=8, height=5, res=300)
p
ggsave(file="Top.pdf", p,width = 8, height = 5, dpi = 300, units = "in") #saves g
dev.off()

# 
# ay <- list(
#   overlaying = "y",
#   side = "right",
#   title = "Vaccination \n(doses/100 people)"
# )
# 
# p <- plot_ly() %>% 
#   add_bars(data, x = ~x, y = ~y1, name = 'Reported CFR', 
#            marker = list(color = 'rgb(49,130,189)'), offsetgroup = 1) %>%
#   add_bars(data, x = ~x, y = ~y2, name = 'Vaccination', 
#            marker = list(color = 'rgb(204,204,204)'), yaxis = "y2", offsetgroup = 2) %>%
#   layout(yaxis2 = ay,
#          xaxis = list(title = "Top-20 vaccinated countries ", tickangle = -45),
#          yaxis = list(title = "Reported CFR (%)"),
#          margin = list(b = 100),
#          barmode = 'group',
#          legend = list(x = 1.1, y = 0.5))
# 
# p  




# Angola, Benin, Botswana, Burkina Faso, Burundi, 
# Cameroon, Central African Republic, Chad, Democratic Republic of Congo, Congo, Cote d'Ivoire, 
# Djibouti, Equatorial Guinea, Eritrea, Ethiopia, Gabon, Gambia, Ghana, Guinea, Guinea-Bissau, 
# Kenya, Lesotho, Liberia, Madagascar, Malawi, Mali, Mauritania, Mauritius, Mozambique, Namibia, 
# Niger, Nigeria, Rwanda, Senegal, Sierra Leone, Somalia, South Africa, South Sudan, Sudan, 
# Tanzania, Togo, Uganda, Zambia, Zimbabwe
# 
# 
# 
# round(exp(confint(fit)),3)
# options(scipen = 999) 
# performance::performance(fit)



# 
# #GLMM Excess mortality
# #Data Management
# setwd('E:\\Update3')
# COVID <- read.csv("excess-deaths-cumulative-economist-single-entity.csv")
# 
# COVID <- subset(COVID, COVID$date >= "2021-01-02") #1/1/2020 2020-08-08
# COVID <- subset(COVID, COVID$date <= "2022-05-10") #5/29/2021
# 
# 
# #Week transformation (daily to weekly)
# library(lubridate)
# COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
# COVID$date2
# COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
# COVID$Week
# 
# x <- nrow(COVID)
# COVID$Week2 <- COVID$Week
# for (i in 1:x) {
#   if (COVID$date[i] >= "2021-01-01")
#     COVID$Week2[i] = COVID$Week[i]+53
#   
# }
# 
# for (i in 1:x) {
#   if (COVID$date[i] >= "2022-01-01")
#     COVID$Week2[i] = COVID$Week[i]+106
#   
# }
# 
# print(COVID$Week2)
# summary(COVID$Week2)
# 
# world_em <- aggregate(COVID$cumulative_estimated_daily_excess_deaths, by= list(COVID$Week2, COVID$location), FUN=sum, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
# world_em
# 
# colnames(world_em) <- c("Weeks", "location", "EM")
# world_em
# 
# # COVID2 <- read.csv("owid-covid-data.csv")
# World <- read.csv("ModelData125.csv")
# 
# COVID <- merge(world_em, World, by=c("location", "Weeks"))
# View(COVID)
# # #Remove World and International information
# # COVID<-COVID[!(COVID$iso_code=="OWID_AFR" | COVID$iso_code=="OWID_ASI" | COVID$iso_code == "OWID_EUN" | COVID$iso_code=="OWID_EUR" | COVID$iso_code=="OWID_INT" | 
# #                  COVID$iso_code=="OWID_HIC" | COVID$iso_code=="OWID_KOS" | COVID$iso_code=="OWID_LIC" | COVID$iso_code=="OWID_LMC" | COVID$iso_code=="OWID_NAM" | 
# #                  COVID$iso_code == "OWID_OCE" | COVID$iso_code=="OWID_SAM"| COVID$iso_code=="OWID_UMC" | COVID$iso_code=="OWID_WRL"),]
# # COVID <- COVID[ which(COVID$population >= 1000000), ]
# # #Creating CFR
# # # COVID$CFR <- (COVID$estimated_daily_excess_deaths/COVID$new_cases)
# 
# options(scipen = 999)
# 
# #GLMM Model
# 
# World <- COVID
# 
# library(glmmTMB)
# World$Vaccinationph2
# World$vml <- World$Vaccinationph2
# World$vml2 <- scale(World$Vaccinationph2)
# 
# World$lw <- log(World$Weeks)
# 
# World$TTs <- World$TotalTestpt
# World$TTs2 <- scale(World$TTs)
# 
# World$GDP2 <- scale(World$GDP)
# 
# World$PopulationDensity2 <- World$PopulationDensity
# World$PopulationDensity22 <- scale(World$PopulationDensity2)
# 
# World$GHSI2 <- scale(World$GHSI)
# 
# World$WGI2 <- scale(World$WGI)
# 
# World$Age65Older2 <- World$Age65Older
# World$Age65Older22 <- scale(World$Age65Older2)
# 
# World$Obesity_rate2 <- World$Obesity_rate
# World$Obesity_rate22 <- scale(World$Obesity_rate2)
# 
# World$EM2 <- scale(World$EM)
# 
# World$SI2 <- scale(World$SI)
# 
# World$location <- as.factor(World$location)
# nrow(World)
# 
# 
# library(glmmTMB)
# library(DHARMa)
# library(performance)
# 
# summary(World$EM)
# 
# 
# World$EM <- (World$EM-min(World$EM))/(max(World$EM)-min(World$EM))
# World$EM
# World$EM[World$EM == 0] <- NA
# World$EM[World$EM == 1] <- NA
# 
# world_em$Weeks
# 
# World125 <- subset(world_em, World$Weeks == 125) 
# nrow(World125$EM)
# 
# View(World125)
# 
# fit <- glmmTMB(EM~ vml2*lw +Age65Older22 + PopulationDensity22 
#                + TTs2 + GDP2 + GHSI2 
#                + WGI2 + Obesity_rate22 + SI2 + (1|location) + (1| lw), na.action=na.omit, family = beta_family(link = "logit"), data = World)
# library(car)
# summary(fit)
# round(exp(confint(fit)),3)
# options(scipen = 999) 
# performance::performance(fit)
# 


#Time series

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)


setwd('E:\\Update3\\Update4\\Update5\\Update6')

COVID <- read.csv("excess-deaths-daily-per-100k-economist.csv")

COVID$date <-  COVID$Day
COVID$iso_code <-  COVID$Code 
COVID$location <-  COVID$Entity

#Week transformation (daily to weekly)
library(lubridate)
COVID$date2 <- as.Date(as.character(COVID$date),format="%Y-%m-%d")
COVID$date2
COVID$Week <- week(as.Date(as.character(COVID$date2),format="%Y-%m-%d"))
COVID$Week

x <- nrow(COVID)
COVID$Week2 <- COVID$Week
for (i in 1:x) {
  if (COVID$date[i] >= "2021-01-01")
    COVID$Week2[i] = COVID$Week[i]+53
  
}

for (i in 1:x) {
  if (COVID$date[i] >= "2022-01-01")
    COVID$Week2[i] = COVID$Week[i]+106
  
}

print(COVID$Week2)
summary(COVID$Week2)

world_em <- aggregate(COVID$estimated_daily_excess_deaths_per_100k ~ COVID$Week2, FUN=mean, na.rm=TRUE) # Seasonal mean mean EIP of 22,006 Farms (3 seasons)
world_em

colnames(world_em) <- c("Weeks", "EM")
world_em

world_em$EM <- (world_em$EM-min(world_em$EM))/(max(world_em$EM)-min(world_em$EM))

myts <- ts(world_em$EM)

auto.arima(myts)
Fit<-Arima(myts,order=c(1,1,1))
summary(Fit)
fcast <- forecast(Fit, h=10)
NROW(myts)
z <- autoplot(fcast, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Weeks") + ylab("Excess mortality") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
z

#R2
SSE <- sum((resid(Fit[1:157]))^2)
SST <- sum((world_em$EM[1:157] - mean(world_em$EM[1:157]))^2)
R_square <- 1 - SSE / SST
R_square


####SES########

library(tidyverse) 
library(fpp2) 
library(ggfortify)
ses.goog <- ses(myts,  
                h = 10) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=10)

x <- autoplot(ses.goog, size = 2,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Excess mortality") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom",
                                                                
                                                                axis.text.x=element_blank(),
                                                                axis.ticks.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8))
x

# library(ggplot2)
# library(ggpubr)
# theme_set(theme_pubr())
# figure <- ggarrange(y, x, z,
#                     labels = c("A", "", ""),
#                     ncol = 1, nrow = 3)
# figure


#R2
SSE <- sum((resid(ses.goog[1:157]))^2)
SST <- sum((world_em$EM[1:157] - mean(world_em$EM[1:157]))^2)
R_square <- 1 - SSE / SST
R_square


#Prophet

history <- data.frame(ds = seq(as.Date('2022-01-06'), as.Date('2022-06-11'), by = 'd'),
                      y = world_em$EM)

m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 10)
fcst3 <- predict(m3, future)
y <-plot(m3, fcst3, xlab="Months", ylab="Excess mortality") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12), axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))
plot(y)



SSE <- sum((history$y[1:157] - fcst3$yhat[c(1:157)])^2)
SST <- sum((history$y[1:157] - mean(history$y[1:157]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[157,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:157)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:157)])))
final <- cbind(last_fcst3, rmse, mae)
final

options(scipen=0)
tiff("EM.tiff", units="in", width=4, height=8, res=300)
gridExtra::grid.arrange(y,x,z)
dev.off()

#Menn kendal
library(Kendall)
library(trend)
# world_CFR <- aggregate(ModelData2$CFR, by= list(ModelData2$Weeks), FUN=mean, na.rm=TRUE)

myts <- ts(World$EM[1:139])
t.test(World$EM[1:139])$"conf.int"
mean(World$EM[1:139])

MannKendall(myts)
sens.slope(myts, conf.level = 0.95)



