library(tidyverse)
library(ggplot2)
library(DT)
library(shiny)
library(shinydashboard)

source("helper.R")

vax_illness=read.csv("./data/vax_illness.csv")

# create illness and grouping choice
choice_illness <- unique(vax_illness$Illness_Friendly)
choice_illness_group=c()
idx=0;
for (illness in choice_illness) {
  idx=idx+1
  choice_illness_group[idx]=paste0(illness,"-Region")
  idx=idx+1
  choice_illness_group[idx]=paste0(illness,"-HDI")
}

vax_coverage = read.csv("./data/vaccine coverage.csv")

#filter vax coverage data to include population
pop=read.csv("./data/pop.csv")
colnames(vax_coverage)[colnames(vax_coverage)=="Cname"]="Country"
total_population_by_year = pop %>% group_by(Year) %>% summarise(total_pop=sum(pop,na.rm=T))

#Vaccine breakdown by region
vax_coverage_final = inner_join(vax_coverage,pop) %>% inner_join(.,total_population_by_year) %>% 
  mutate(final_coverage=(Percent_covrage*pop)/total_pop)

dtp_by_region=vax_filter_by_vaccine("DTP1")
ipv_by_region=vax_filter_by_vaccine("IPV1")
mcv_by_region=vax_filter_by_vaccine("MCV1")
rcv_by_region=vax_filter_by_vaccine("RCV2")
japenc_by_region=vax_filter_by_vaccine("JapEnc") 

#illness data by region
#diphtheria data
diphtheria = read.csv("./data/diphtheria.csv")
colnames(diphtheria)[colnames(diphtheria)=="Cname"]="Country"
diphtheria_by_region_2010_2019 = illness_group_by_region(diphtheria)
all_diphtheria_dtp1=inner_join(diphtheria_by_region_2010_2019 ,dtp_by_region)

#tetanus data
tetanus1 = read.csv("./data/tetanus.csv")
tetanus2 = read.csv("./data/ttetanus.csv")
tetanus = rbind(tetanus1,tetanus2)
colnames(tetanus)[colnames(tetanus)=="Cname"]="Country"
tetanus_by_region_2010_2019 = illness_group_by_region(tetanus)
all_tetanus_dtp1=inner_join(tetanus_by_region_2010_2019 ,dtp_by_region)

#pertussis data
pertussis = read.csv("./data/pertussis.csv")
colnames(pertussis)[colnames(pertussis)=="Cname"]="Country"
pertussis_by_region_2010_2019 = illness_group_by_region(pertussis)
all_pertussis_dtp1=inner_join(pertussis_by_region_2010_2019 ,dtp_by_region)

#polio
polio = read.csv("./data/polio.csv")
colnames(polio)[colnames(polio)=="Cname"]="Country"
polio_by_region_2010_2019 = illness_group_by_region(polio)
all_polio_ipv1=inner_join(polio_by_region_2010_2019 ,ipv_by_region)

#measles
measles = read.csv("./data/measles.csv")
colnames(measles)[colnames(measles)=="Cname"]="Country"
measles_by_region_2010_2019 = illness_group_by_region(measles)
all_measles_mcv1=inner_join(measles_by_region_2010_2019 ,mcv_by_region)

#Rubella
rubella1 = read.csv("./data/CRS incidence.csv")
rubella2 = read.csv("./data/rubella.csv")
rubella = rbind(rubella1,rubella2)
colnames(rubella)[colnames(rubella)=="Cname"]="Country"
rubella_by_region_2010_2019 = illness_group_by_region(rubella)
all_rubella_rcv1=inner_join(rubella_by_region_2010_2019 ,rcv_by_region)

#Japanese Enciphelitis
japenc = read.csv("./data/japenc.csv")
colnames(japenc)[colnames(japenc)=="Cname"]="Country"
japenc_by_region_2010_2019 = illness_group_by_region(japenc)
all_japenc=inner_join(japenc_by_region_2010_2019 ,japenc_by_region)
print(all_japenc)
#hdi related data
#vax data
hdi=read_csv("./data/hdi.csv")
hdi = hdi %>% dplyr::select(-contains("X"))
hdi = flatten_hdi_file(hdi) %>% mutate(Year=as.numeric(Year))

vax_coverage_hdi = inner_join(vax_coverage_final,hdi) %>% mutate(hdi_level=ifelse(
  as.numeric(hdi)>0.8, "Very High Development", ifelse(
    as.numeric(hdi) > 0.7, "High Development", ifelse(
      as.numeric(hdi) > 0.55, "Medium Development", "Low Development"
    )
  )
))

vax_coverage_hdi_avg = vax_coverage_hdi %>% filter(!is.na(hdi_level)) %>% group_by(Year,hdi_level) %>%  summarise(avg=mean(final_coverage,na.rm=T)) %>% mutate(hdi_level=as.factor(hdi_level))  

#disease data


