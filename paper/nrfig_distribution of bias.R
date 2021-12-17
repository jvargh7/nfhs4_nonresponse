child_indicators <- read_csv(paste0(path_response_folder,"/working/ipw weighted child estimates at state level.csv"))  
male_indicators <- read_csv(paste0(path_response_folder,"/working/ipw weighted male estimates at state level.csv"))  
female_indicators <- read_csv(paste0(path_response_folder,"/working/ipw weighted female estimates at state level.csv"))  

nfhs4_ipw_estimates <- bind_rows(child_indicators %>% mutate(population = "Child"),
                                 male_indicators %>% mutate(population = "Men"),
                                 female_indicators %>% mutate(population = "Women")) %>% 
  dplyr::mutate(bias_ipw_orig = proportion_ipw - proportion_orig,
                bias_ipw_unadj = proportion_ipw - proportion_unadj,
                bias_orig_unadj = proportion_orig - proportion_unadj) %>% 
  dplyr::select(state,population,indicator,contains("bias")) %>% 
  mutate(indicator = case_when(indicator == "S81" ~ "Stunting",
                               indicator %in% c("S84","S86","S87") ~ "Underweight",
                               indicator %in% c("S92","S97","S95") ~ "Anemia",
                               indicator %in% c("S89","S88") ~ "Overweight",
                               indicator %in% c("S103","S100") ~ "Very high blood glucose",
                               indicator %in% c("S109","S106") ~ "Moderate/severe blood pressure",
                               TRUE ~ NA_character_)
         ) %>% 
  mutate_at(vars(contains("bias")),function(x) x*100) %>% 
  mutate(indicator = factor(indicator,levels=c("Stunting",
                                               "Underweight",
                                               "Overweight",
                                               "Anemia",
                                               "Very high blood glucose",
                                               "Moderate/severe blood pressure"),ordered=TRUE))


(nfhs4_ipw_estimates %>% 
  ggplot(data=.,aes(x=bias_ipw_unadj,y=indicator)) +
  geom_jitter(height=0.2,size=0.5) +
  facet_grid(~population,scales="free_y") +
  xlab("(IPW adjusted - Unadjusted Prevalence) (%)") +
  ylab("") +
  theme_bw() +
    theme(text = element_text(size = 8)))%>% 
  ggsave(.,filename = paste0(path_response_folder,"/figures/jitter of bias_ipw_unadj.png"),width=2000,height=800,units="px")

(nfhs4_ipw_estimates %>% 
    ggplot(data=.,aes(x=bias_ipw_orig,y=indicator)) +
    geom_jitter(height=0.2,size=0.5) +
    facet_grid(~population,scales="free_y") +
    xlab("(IPW adjusted - Original Prevalence) (%)") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size = 8)))%>% 
  ggsave(.,filename = paste0(path_response_folder,"/figures/jitter of bias_ipw_orig.png"),width=2000,height=800,units="px")


(nfhs4_ipw_estimates %>% 
    ggplot(data=.,aes(x=bias_orig_unadj,y=indicator)) +
    geom_jitter(height=0.2,size=0.5) +
    facet_grid(~population,scales="free_y") +
    xlab("(Original - Unadjusted Prevalence) (%)") +
    ylab("") +
    theme_bw() +
    theme(text = element_text(size = 8)))%>% 
  ggsave(.,filename = paste0(path_response_folder,"/figures/jitter of bias_orig_unadj.png"),width=2000,height=800,units="px")

