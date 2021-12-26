source("preprocessing/nraux04_datasets for descriptive statistics.R")
rm(male_surveydesign,male_surveydesign_hiv,
   female_surveydesign,female_surveydesign_hiv,
   child_surveydesign,child_surveydesign_hb)
child_model_summary <- readRDS(paste0(path_response_folder,"/working/child non response with state model summary.RDS"))
male_model_summary <- readRDS(paste0(path_response_folder,"/working/male non response with state model summary.RDS"))
female_model_summary <- readRDS(paste0(path_response_folder,"/working/female non response with state model summary.RDS"))

# child_model_summary <- readRDS(paste0(path_response_folder,"/working/child non response model summary.RDS"))
# male_model_summary <- readRDS(paste0(path_response_folder,"/working/male non response model summary.RDS"))
# female_model_summary <- readRDS(paste0(path_response_folder,"/working/female non response model summary.RDS"))

child_weights <- child %>% 
  dplyr::select(hhid,hvidx,hv001,
                hv002,hv006,hv007,hv008,hv021,state,hv024,shdistri,weight,hweight,
                valid_height,valid_weight,valid_hb)
  

child_weights$ipw_height <- 1/(child_model_summary[[1]]$predictions$response)
child_weights$ipw_weight <- 1/(child_model_summary[[2]]$predictions$response)
child_weights$ipw_hb <- NA_real_
child_weights[!is.na(child_weights$valid_hb),]$ipw_hb <- 1/(child_model_summary[[3]]$predictions$response)
saveRDS(child_weights,paste0(path_response_folder,"/child weights.RDS"))



female_weights <- female %>% 
  dplyr::select(hvidx,hv001,
                hv002,hv006,hv007,hv008,hv021,state,hv024,shdistri,weight,hweight,
                valid_height,valid_weight,valid_hb,
                valid_sbp,valid_sbp_atleast1,valid_glucose,consented_hiv)

female_weights$ipw_height <- 1/(female_model_summary[[1]]$predictions$response)
female_weights$ipw_weight <- 1/(female_model_summary[[2]]$predictions$response)
female_weights$ipw_hb <- 1/(female_model_summary[[3]]$predictions$response)
female_weights$ipw_sbp <- 1/(female_model_summary[[4]]$predictions$response)
female_weights$ipw_sbp_atleast1 <- 1/(female_model_summary[[5]]$predictions$response)
female_weights$ipw_glucose <- 1/(female_model_summary[[6]]$predictions$response)

female_weights$ipw_hiv <- NA
female_weights[!is.na(female_weights$consented_hiv),]$ipw_hiv <- 1/(female_model_summary[[7]]$predictions$response)
saveRDS(female_weights,paste0(path_response_folder,"/female weights.RDS"))


male_weights <- male %>% 
  dplyr::select(hhid,hvidx,hv001,
                hv002,hv006,hv007,hv008,hv021,state,hv024,shdistri,weight,hweight,
                valid_height,valid_weight,valid_hb,
                valid_sbp,valid_sbp_atleast1,valid_glucose,consented_hiv)

male_weights$ipw_height <- 1/(male_model_summary[[1]]$predictions$response)
male_weights$ipw_weight <- 1/(male_model_summary[[2]]$predictions$response)
male_weights$ipw_hb <- 1/(male_model_summary[[3]]$predictions$response)
male_weights$ipw_sbp <- 1/(male_model_summary[[4]]$predictions$response)
male_weights$ipw_sbp_atleast1 <- 1/(male_model_summary[[5]]$predictions$response)
male_weights$ipw_glucose <- 1/(male_model_summary[[6]]$predictions$response)

male_weights$ipw_hiv <- NA
male_weights[!is.na(male_weights$consented_hiv),]$ipw_hiv <- 1/(male_model_summary[[7]]$predictions$response)
saveRDS(male_weights,paste0(path_response_folder,"/male weights.RDS"))

