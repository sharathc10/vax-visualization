library(tidyverse)
library(ggplot2)
library(DT)
library(shiny)
library(shinydashboard)

source("helper.R")
country_highest="Afghanistan"
vax_illness=read.csv("./data/vax_illness.csv")

# create illness and grouping choice

choice_illness <- unique(vax_illness$Illness_Friendly)

vax_coverage = read.csv("./data/vaccine coverage.csv")

#filter vax coverage data to include population
pop=read.csv("./data/pop.csv")
colnames(vax_coverage)[colnames(vax_coverage)=="Cname"]="Country"
total_population_by_year = pop %>% group_by(Year) %>% summarise(total_pop=sum(pop,na.rm=T))

#Vaccine breakdown by region
vax_coverage_final = inner_join(vax_coverage,pop) %>% inner_join(.,total_population_by_year) %>% 
  mutate(final_coverage=(Percent_covrage*pop)/total_pop)

dtp_by_region=vax_group_by_region("DTP3")
ipv_by_region=vax_group_by_region("IPV1")
mcv_by_region=vax_group_by_region("MCV1")
rcv_by_region=vax_group_by_region("RCV1")
japenc_by_region=vax_group_by_region("JapEnc") 

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

#hdi related data
#vax data
hdi=read_csv("./data/hdi.csv")
hdi = hdi %>% dplyr::select(-contains("X"))
hdi = flatten_hdi_file(hdi) %>% mutate(Year=as.numeric(Year)) %>% 
  mutate(hdi_level=ifelse(
  as.numeric(hdi)>0.8, "Very High Development", ifelse(
    as.numeric(hdi) > 0.7, "High Development", ifelse(
      as.numeric(hdi) > 0.55, "Medium Development", "Low Development"
    )
  )
))
hdi$hdi_level <- factor(hdi$hdi_level, levels = c("Very High Development", "High Development", "Medium Development","Low Development"))


vax_coverage_hdi = inner_join(vax_coverage_final,hdi) 
vax_DTP1_hdi=vax_group_by_hdi("DTP3") 
vax_IPV1_hdi=vax_group_by_hdi("IPV1") 
vax_MCV1_hdi=vax_group_by_hdi("MCV1") 
vax_RCV1_hdi=vax_group_by_hdi("RCV1") 
vax_JapEnc_hdi=vax_group_by_hdi("JapEnc") 


diphtheria_by_hdi_2010_2019 = illness_group_by_hdi(diphtheria,hdi)
tetanus_by_hdi_2010_2019 = illness_group_by_hdi(tetanus,hdi)
pertussis_by_hdi_2010_2019 = illness_group_by_hdi(pertussis,hdi)
polio_by_hdi_2010_2019 = illness_group_by_hdi(polio,hdi)
measles_by_hdi_2010_2019 = illness_group_by_hdi(measles,hdi)
rubella_by_hdi_2010_2019 = illness_group_by_hdi(rubella,hdi)
japenc_by_hdi_2010_2019 = illness_group_by_hdi(japenc,hdi)


diphtheria_dtp_hdi=inner_join(vax_DTP1_hdi,diphtheria_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
tetanus_dtp_hdi=inner_join(vax_DTP1_hdi,tetanus_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
pertussis_dtp_hdi=inner_join(vax_DTP1_hdi,pertussis_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
polio_ipv_hdi=inner_join(vax_IPV1_hdi,polio_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
measles_mcv_hdi=inner_join(vax_MCV1_hdi,measles_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
rubella_rcv_hdi=inner_join(vax_RCV1_hdi,rubella_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))
japenc_hdi=inner_join(vax_JapEnc_hdi,japenc_by_hdi_2010_2019) %>% filter(!is.na(hdi_level))

#Data for information boxes
#diphtheria highest case country
#diphtheria year when highest case occured
diphtheria_highest=(flatten_illness_data(diphtheria) %>% arrange(desc(reported_cases)))[1,]

