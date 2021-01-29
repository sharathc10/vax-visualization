library(dplyr)

flatten_illness_data=function(dat) {
  dat_colnames=colnames(dat)
  year_columns= dat_colnames[startsWith(dat_colnames,"X")]
  dat %>%  pivot_longer(year_columns,names_to="year",values_to="reported_cases") %>% mutate(year=substr(year,2,5))
}