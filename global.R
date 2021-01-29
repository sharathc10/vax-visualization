library(tidyverse)
library(raster)
library(leaflet)
library(ggplot2)
source("helper.R")

vax_illness=read.csv("./data/vax_illness.csv")

# create variable with colnames as choice 
choice_illness <- unique(vax_illness$Illness_Friendly)
choice_vaccine <- unique(vax_illness$Vaccine)
choice_groupings <- c("Region")
vax_coverage = read.csv("./data/vaccine coverage.csv")
choice_regions = c("All", unique(vax_coverage$WHO_REGION))

#filter vax coverage data to include population
pop=read.csv("./data/pop.csv")
colnames(vax_coverage)[colnames(vax_coverage)=="Cname"]="Country"
total_population_by_year = pop %>% group_by(Year) %>% summarise(total_pop=sum(pop,na.rm=T))

#Vaccine breakdown 
vax_coverage_final = inner_join(vax_coverage,pop) %>% inner_join(.,total_population_by_year) %>% 
  mutate(final_coverage=(Percent_covrage*pop)/total_pop)

dtp_by_region=vax_coverage_final %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
  filter(Vaccine=="DTP1") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(final_coverage,na.rm=T)) 

ipv_by_region=vax_coverage_final %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
  filter(Vaccine=="IPV1") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(final_coverage,na.rm=T)) 

mcv_by_region=vax_coverage_final %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
  filter(Vaccine=="MCV1") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(final_coverage,na.rm=T)) 

rcv_by_region=vax_coverage_final %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
  filter(Vaccine=="RCV1") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(final_coverage,na.rm=T)) 

japenc_by_region=vax_coverage_final %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
  filter(Vaccine=="JapEnc") %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(avg=mean(final_coverage,na.rm=T)) 

#diphtheria data
diphtheria = read.csv("./data/diphtheria.csv")
colnames(diphtheria)[colnames(diphtheria)=="Cname"]="Country"
diphtheria_by_region_2010_2019 = flatten_illness_data(diphtheria) %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
  filter(Year>=2010) %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(total_cases=sum(reported_cases,na.rm=T)) 

all_diphtheria_dtp1=inner_join(diphtheria_by_region_2010_2019 ,dtp_by_region)

#tetanus data
tetanus1 = read.csv("./data/tetanus.csv")
tetanus2 = read.csv("./data/ttetanus.csv")
tetanus = rbind(tetanus1,tetanus2)
colnames(tetanus)[colnames(tetanus)=="Cname"]="Country"
tetanus_by_region_2010_2019 = flatten_illness_data(tetanus) %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
  filter(Year>=2010) %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(total_cases=sum(reported_cases,na.rm=T)) 

all_tetanus_dtp1=inner_join(tetanus_by_region_2010_2019 ,dtp_by_region)

#pertussis data
pertussis = read.csv("./data/pertussis.csv")
colnames(pertussis)[colnames(pertussis)=="Cname"]="Country"
pertussis_by_region_2010_2019 = flatten_illness_data(pertussis) %>% 
  mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
  filter(Year>=2010) %>% 
  group_by(WHO_REGION,Year) %>% 
  summarise(total_cases=sum(reported_cases,na.rm=T)) 

all_pertussis_dtp1=inner_join(pertussis_by_region_2010_2019 ,dtp_by_region)



