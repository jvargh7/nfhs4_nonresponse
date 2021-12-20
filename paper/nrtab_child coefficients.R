child_model_summary <- readRDS(paste0(path_response_folder,"/working/child non response with state model summary.RDS"))

coefficients_table <- map_dfr(child_model_summary,
                              function(l){
                                
                                l$coefficients %>% 
                                  mutate(estimate = exp(estimate)) %>% 
                                  mutate(coef_ci = paste0(round(estimate,1)," (",
                                                          round(estimate/exp(1.96*std.error),1) , ",",
                                                          round(estimate*exp(1.96*std.error),1), ")"),
                                         variable = l$variable
                                  ) %>% 
                                  dplyr::select(variable,term,coef_ci)
                                
                                
                              }) %>% 
  pivot_wider(names_from=variable,values_from=coef_ci) %>% 
  dplyr::select(term,valid_height,valid_weight,valid_hb)

write_csv(coefficients_table,paste0(path_response_folder,"/working/table_child non response coefficients.csv"))


child_reported_summary <- readRDS(paste0(path_response_folder,"/working/child non reporting with state model summary.RDS"))

coefficients_table_reported <- map_dfr(child_reported_summary,
                              function(l){
                                
                                l$coefficients %>% 
                                  mutate(estimate = exp(estimate)) %>% 
                                  mutate(coef_ci = paste0(round(estimate,1)," (",
                                                          round(estimate/exp(1.96*std.error),1) , ",",
                                                          round(estimate*exp(1.96*std.error),1), ")"),
                                         variable = l$variable
                                  ) %>% 
                                  dplyr::select(variable,term,coef_ci)
                                
                                
                              }) %>% 
  pivot_wider(names_from=variable,values_from=coef_ci) %>% 
  dplyr::select(term,reported_height,reported_weight,reported_hb)

write_csv(coefficients_table_reported,paste0(path_response_folder,"/working/table_child non reporting coefficients.csv"))
