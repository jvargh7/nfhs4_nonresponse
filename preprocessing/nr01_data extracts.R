


nonresponse_variables <- readxl::read_excel(paste0(path_response_folder,"/working/Non Participation Variable List.xlsx"),
                                        sheet = "iapr_map")


# Children ----------

iapr_child <- haven::read_dta(paste0(path_india_raw_data,"/IAPR74DT/IAPR74FL.dta"),
                       col_select = na.omit(nonresponse_variables$child)) %>% 
  dplyr::filter(!is.na(hc1))

saveRDS(iapr_child,paste0(path_response_folder,"/working/iapr_child.RDS"))
rm(iapr_child)

# Women ------------
iair_extract <- haven::read_dta(paste0(path_india_raw_data,"/IAIR74DT/IAIR74FL.dta"),
                         col_select = na.omit(nonresponse_variables$female_iair))

iapr_female <- haven::read_dta(paste0(path_india_raw_data,"/IAPR74DT/IAPR74FL.dta"),
                         col_select = na.omit(nonresponse_variables$female)) %>% 
  dplyr::filter(!is.na(ha1))

iair_clean <- iair_extract %>% 
  left_join(iapr_female,
            by=c("v003"="hvidx",
                 "v002" = "hv002",
                 "v001" = "hv001"))
saveRDS(iair_clean,paste0(path_response_folder,"/working/iair_clean.RDS"))
rm(iair_clean,iair_extract,iapr_female)


# Men ------------

iamr_extract <- haven::read_dta(paste0(path_india_raw_data,"/IAMR74DT/IAMR74FL.dta"),
                         col_select = na.omit(nonresponse_variables$male_iamr)) 

iapr_male <- haven::read_dta(paste0(path_india_raw_data,"/IAPR74DT/IAPR74FL.dta"),
                      col_select = na.omit(nonresponse_variables$male)) %>% 
  dplyr::filter(!is.na(hb1))

# IAPR has more eligible males than IAMR 
iamr_clean <- iapr_male %>% 
  right_join(iamr_extract,
            by=c("hvidx" = "mv003",
                 "hv002" = "mv002",
                 "hv001" = "mv001"))

saveRDS(iamr_clean,paste0(path_response_folder,"/working/iamr_clean.RDS"))
rm(iamr_clean,iamr_extract,iapr_male)
