
source("preprocessing/nraux04_datasets for descriptive statistics.R")
source("functions/fit models.R")


source("analysis/nrmodels02_non response in children.R")
source("analysis/nrmodels03_non response in males.R")
source("analysis/nrmodels04_non response in females.R")


source("analysis/nrmodels05_non response in children with state.R")
source("analysis/nrmodels06_non response in males with state.R")
source("analysis/nrmodels07_non response in females with state.R")

source("analysis/nrmodels08_non reporting in children with state.R")
source("analysis/nrmodels09_non reporting in males with state.R")
source("analysis/nrmodels10_non reporting in females with state.R")

# Checking for systematically higher predicted probability for delhi --------

# child$pred_height <- child_model_summary[[1]]$predictions$response
# 
# View(child %>% 
#        dplyr::filter(hv024 %in% c("delhi","telangana","andhra pradesh")) %>% 
#        dplyr::select(hv024,starts_with("hv"),pred_height,valid_height))
# 
# child$pred_hb = NA
# child[child$age_category != "[0,6)",]$pred_hb <- child_model_summary[[3]]$predictions$response
# 
# View(child %>% 
#        dplyr::filter(hv024 %in% c("delhi","telangana","andhra pradesh")) %>% 
#        dplyr::select(hv024,starts_with("hv"),pred_hb,valid_hb))
# 
# 
# 
# # Checking for systematically higher predicted probability for delhi --------
# 
# male$pred_glucose <- male_model_summary[[6]]$predictions$response
# 
# View(male %>% 
#        dplyr::filter(hv024 %in% c("delhi","telangana","andhra pradesh")) %>% 
#        dplyr::select(hv024,starts_with("hv"),pred_glucose,valid_glucose))
# 
# child$pred_hb = NA
# child[child$age_category != "[0,6)",]$pred_hb <- child_model_summary[[3]]$predictions$response
# 
# View(child %>% 
#        dplyr::filter(hv024 %in% c("delhi","telangana","andhra pradesh")) %>% 
#        dplyr::select(hv024,starts_with("hv"),pred_hb,valid_hb))
