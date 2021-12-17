source("preprocessing/nraux04_datasets for descriptive statistics.R")

bind_rows(child %>% dplyr::select(matches("(consent|reported|valid)")) %>% mutate(population = "child"),
          female%>% dplyr::select(matches("(consent|reported|valid)")) %>% mutate(population = "female"),
          male%>% dplyr::select(matches("(consent|reported|valid)")) %>% mutate(population = "male")) %>% 
  group_by(population) %>% 
  summarize_all(~sum(.==1,na.rm=TRUE)) %>% 
  View()
