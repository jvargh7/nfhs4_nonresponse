
# source("preprocessing/nraux04_datasets for descriptive statistics.R")
# source("functions/fit models.R")

all_variables = names(male)

valid_variables <- c(all_variables[str_detect(all_variables,"valid")],"consented_hiv")
valid_variables <- valid_variables[!valid_variables %in% c("valid_hiv","valid_sbp_count",
                                                           "valid_dbp_count","valid_dbp","valid_dbp_atleast1")]

# Dataset creation --------
model_df <- male %>% 
  dplyr::select(state,shdistri,hv001,
                one_of(valid_variables),
                age_category,schooling, caste,
                religion, wealth, rural,insurance,
                married,nchildren,nadults
  )
# model_svydesign = male_surveydesign



# rm(female,male,child)
# rm(male_surveydesign,child_surveydesign,female_surveydesign)

male_model_summary <- map(valid_variables,
                     function(v){
                       print(v);
                       gc();
                       formula_valid = paste0(v,"~ age_category + schooling + caste + religion + wealth + rural + insurance + 
                                                  married + nchildren + nadults + state");
                       if(v!= "consented_hiv"){
                         m <- fit_model(formula_chr = formula_valid,df = male_surveydesign,model = "svyglm")
                       }
                       if(v == "consented_hiv"){
                         m <- fit_model(formula_chr = formula_valid,df = male_surveydesign_hiv,model = "svyglm")
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


saveRDS(male_model_summary,paste0(path_response_folder,"/working/male non response with state model summary.RDS"))