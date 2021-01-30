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

vax_filter_by_vaccine=function(vaccine) {
  return (vax_coverage_final %>% 
            mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>% 
            filter(Vaccine==vaccine) %>% 
            group_by(WHO_REGION,Year) %>% 
            summarise(avg=mean(final_coverage,na.rm=T)))
}

illness_group_by_region=function(df) {
  return (flatten_illness_data(df) %>% 
            mutate(WHO_REGION=as.factor(WHO_REGION),Year=as.numeric(Year)) %>%  
            filter(Year>=2010) %>% 
            group_by(WHO_REGION,Year) %>% 
            summarise(total_cases=sum(reported_cases,na.rm=T)) )
}