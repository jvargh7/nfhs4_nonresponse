require(geepack)
require(lme4)
require(survey)

fit_model <- function(formula_chr,df,model = "geeglm"){
  
  outcome_var = str_extract(formula_chr,"^[a-z0-9_]+\\s*~") %>% str_replace_all(.,"[\\s|~]+","")
  
  if(model == "geeglm"){
    model_fit <- geeglm(formula = as.formula(paste0(formula_chr,"+ factor(state)")),
                        family=binomial(),
                        data=df,
                        id = hv001,corstr="independence")
    
  }
  
  if(model == "svyglm"){
    
    model_fit <- svyglm(formula = as.formula(formula_chr),design=df,family=binomial())
  }
  
  if(model == "lmer"){
    model_fit <- glmer(as.formula(paste0(formula_chr,"+ (1|state/shdistri/hv001)")),
                       family=binomial(),
                      data = df)
  
  }
  
  return(model_fit)
  
}
