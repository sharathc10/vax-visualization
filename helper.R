#all helper functions here
flatten_illness_data=function(dat) {
  dat_colnames=colnames(dat)
  year_columns= dat_colnames[startsWith(dat_colnames,"X")]
  dat %>%  pivot_longer(year_columns,names_to="Year",
                        values_to="reported_cases") %>% 
    mutate(Year=substr(Year,2,5))
}

flatten_hdi_file=function(dat) {
  dat_colnames=colnames(dat)
  year_columns= dat_colnames[startsWith(dat_colnames,"1") | startsWith(dat_colnames,"2")]
  dat %>%  pivot_longer(year_columns,names_to="Year",values_to="hdi")
}

vax_group_by_region=function(vaccine) {
  return (vax_coverage_final %>% 
            mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
            filter(Vaccine==vaccine) %>% 
            group_by(WHO_REGION,Year) %>% 
            summarise(avg=min(sum(final_coverage,na.rm=T),0.99)))
}

illness_group_by_region=function(df) {
  return (flatten_illness_data(df) %>% 
            mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
            filter(Year>=2010) %>% 
            group_by(WHO_REGION,Year) %>% 
            summarise(total_cases=sum(reported_cases,na.rm=T)) )
}

vax_group_by_hdi=function(vaccine) {
  return (vax_coverage_hdi %>% 
            filter(Vaccine==vaccine) %>% 
            group_by(hdi_level,Year) %>% 
            summarise(avg=min(sum(final_coverage,na.rm=T),0.99)))
}

illness_group_by_hdi=function(df,hdi) {
  return(flatten_illness_data(df) %>% 
           mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
           filter(Year>=2010) %>% 
           inner_join(.,hdi) %>% 
           group_by(hdi_level,Year) %>% 
           summarise(total_cases=sum(reported_cases,na.rm=T)))
}

plot_vax_disease_graph=function(df,facetresponse,graphtitle,xlabel,ylabel,scaleVal) {
  return (ggplot(df, aes(x=factor(Year), total_cases)) + 
    theme_bw() +  
    geom_bar(data=df,stat="identity",fill="#7FFFD4") + 
    geom_line(data=df, aes(y=avg*scaleVal,group = 1,linetype = "Vaccine coverage"),size=2,color="#ff7faa") + 
    scale_y_continuous(sec.axis= sec_axis(~./100, name="Vaccine coverage(%)")) +
    ggtitle(graphtitle) +
    labs(x=xlabel,y=ylabel)+
    theme(axis.title=element_text(size=16)))  
}

plot_all_vax_disease_graph=function(df,facetresponse,graphtitle,xlabel,ylabel,scaleVal) {
  test_value=100 
  test=as.formula(paste0("~./",test_value))
   return (ggplot(df, aes(x=factor(Year), total_cases)) + 
            theme_bw() +  
            geom_bar(data=df,stat="identity",width=0.7,fill="#7FFFD4") + 
            geom_line(data=df, aes(x=factor(Year),y=avg*scaleVal,group = 1,linetype = "Vaccine coverage"),color="#ff7faa") +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_y_continuous(sec.axis= sec_axis(~./100,name="Vaccine coverage(%)")) +
            facet_wrap(as.formula(paste("~", facetresponse))) +
            ggtitle(graphtitle) +
            labs(x=xlabel,y=ylabel,color="Legend")+
            theme(axis.title=element_text(size=16))) 

}

highest_cases_illness=function(illnessdf) {
  highest_case= ((flatten_illness_data(illnessdf) %>%
             mutate(Year=as.numeric(Year)) %>% 
             filter(Year >=2010) %>% 
             inner_join(.,total_population_by_year) %>% 
             mutate(reported_case_ratio=reported_cases/total_pop)  %>%  
             arrange(desc(reported_case_ratio)))[1,])
 return (highest_case)
}

filter_highest_cases=function(illness,group,groupName) {
  df=diphtheria
  if (illness=="Diphtheria") {
    df=diphtheria
  } else if (illness=="Tetanus") {
    df=tetanus
  }else if (illness=="Pertussis") {
    df=pertussis
  }else if (illness=="Polio") {
    df=polio
  }else if (illness=="Measles") {
    df=measles
  }else if (illness=="Rubella") {
    df=rubella
  }else if (illness=="Japanese Enciphelites") {
    df=japenc
  }
  if (group=="Region") {
    highest_row=(
      flatten_illness_data(df) %>% 
      mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%
      filter(Year >=2010 & WHO_REGION==groupName) %>% 
      inner_join(.,total_population_by_year) %>% 
      mutate(reported_case_ratio=reported_cases/total_pop)  %>%   
      arrange(desc(reported_case_ratio)))[1,] 
  } else {
    highest_row=(flatten_illness_data(df)%>% 
      mutate(Year=as.numeric(Year)) %>% 
      filter(Year >= 2010) %>% 
      inner_join(.,hdi) %>% 
      filter(hdi_level==groupName) %>%
      inner_join(.,total_population_by_year) %>% 
      mutate(reported_case_ratio=reported_cases/total_pop)  %>%    
      arrange(desc(reported_case_ratio)))[1,] 
  }
  return (highest_row)
}

map_hdi_value=function(hdi_abbrev) {
  return (case_when(
    hdi_abbrev == "VHD" ~ "Very High Development",
    hdi_abbrev == "HD" ~ "High Development",
    hdi_abbrev == "MD" ~ "Medium Development",
    hdi_abbrev == "LD" ~ "Low Development",
    TRUE ~ "Low Development"
  ))
}

map_region_value=function(region_abbrev) {
  return (case_when(
    region_abbrev == "AFR" ~ "Africa",
    region_abbrev == "AMR" ~ "America",
    region_abbrev == "EUR" ~ "Europe",
    region_abbrev == "EMR" ~ "Eastern Mediteranean",
    region_abbrev == "SEAR" ~ "South-East Asia",
    region_abbrev == "WPR" ~ "Western Pacific"
    ))
}