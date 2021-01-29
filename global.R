library(tidyverse)
library(raster)
library(leaflet)
library(ggplot2)
source("helper.R")

vax_illness=read.csv("./data/vax_illness.csv")

# create variable with colnames as choice 
choice_illness <- unique(vax_illness$Illness_Friendly)
choice_vaccine <- unique(vax_illness$Vaccine)

vax_coverage = read.csv("./data/vaccine coverage.csv")
choice_regions = c("All", unique(vax_coverage$WHO_REGION))
#filter vax coverage data to include population
vax_2010_2019 = vax_coverage %>% filter(Year>=2010) 
colnames(vax_2010_2019)[colnames(vax_2010_2019)=="Cname"]="Country"
#DTP 
dtp1_by_region=vax_coverage %>% mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
  filter(Year>=2010 & Vaccine=="DTP1") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(Percent_covrage,na.rm=T)) 
#diphtheria data
diphtheria = read.csv("./data/diphtheria.csv")
colnames(diphtheria)[colnames(diphtheria)=="Cname"]="Country"
diphtheria_by_region_2010_2019 = flatten_illness_data(diphtheria) %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),year=as.numeric(year)) %>%  
  filter(year>=2010) %>% 
  group_by(WHO_REGION,year) %>% 
  summarise(total_cases=sum(reported_cases,na.rm=T)) 

