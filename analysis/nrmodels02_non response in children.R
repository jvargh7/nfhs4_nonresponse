# 
# source("preprocessing/nraux04_datasets for descriptive statistics.R")
# source("functions/fit models.R")

all_variables = names(child)

valid_variables <- c("valid_height","valid_weight","valid_hb")

# Dataset creation --------
model_df <- child %>% 
  dplyr::select(state,shdistri,hv001,strata,
                one_of(valid_variables),
                age_category,schooling,schooling_imp, caste,
                religion, wealth, rural,insurance,
                nchildren,nadults
  )
# model_svydesign = child_surveydesign



# rm(female,male,child)
# rm(male_surveydesign,child_surveydesign,female_surveydesign)

child_model_summary <- map(valid_variables,
                     function(v){
                       print(v);
                       gc();
                       formula_valid = paste0(v,"~ age_category + schooling_imp + caste + religion + wealth + rural + insurance + 
                                                  # married + 
                                              nchildren + nadults");
                       if(v == "valid_hb"){
                         m <- fit_model(formula_chr = formula_valid,df = child_surveydesign_hb,model = "svyglm")
                       }
                       if(v %in% c("valid_height","valid_weight")){
                         m <- fit_model(formula_chr = formula_valid,df = child_surveydesign,model = "svyglm")
                       }
                       pred = predict(m,se.fit=TRUE,type="response") %>% data.frame(.);
                       coefs = broom::tidy(m,exponentiate = FALSE);
                       vcov = m$cov.unscaled;
                       pred$fitted_values = m$fitted.values %>% as.numeric(.);
                       pred$y = m$y %>% as.numeric(.);
                       fit_stats = data.frame(
                         aic = m$aic,
                         deviance = m$deviance,
                         df_null = m$df.null,
                         df_residual = m$df.residual,
                         converged = m$converged
                       );
                       
                       list(variable = v,
                            predictions = pred,
                            coefficients = coefs,
                            vcov = vcov,
                            fit_stats = fit_stats) %>% 
                         
                         return(.)
                       
                     })


saveRDS(child_model_summary,paste0(path_response_folder,"/working/child non response model summary.RDS"))