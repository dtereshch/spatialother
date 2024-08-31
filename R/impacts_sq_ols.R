impacts_sq_ols <- function(model, variable, sqterm, range, alpha = 0.05, n){
  
  library(rlang)
  
  variable <- as_name(enquo(variable))
  sqterm <- as_name(enquo(sqterm))
  modname <- as_name(enquo(model))
  
  # lag_variable <- paste("lag", variable, sep = "_")
  # lag_sqterm <- paste("lag", sqterm, sep = "_")
  
  coefs <- model[["coefficients"]] %>% as_vector()
  
  eff_dir_variable <- coefs[variable] %>% as.numeric()
  eff_dir_sqterm <- coefs[sqterm] %>% as.numeric()
  # eff_ind_variable <- coefs[lag_variable,] %>% as.numeric()
  # eff_ind_sqterm <- coefs[lag_sqterm,] %>% as.numeric()
  #eff_tot
  
  cov_mat <- model[["var"]] %>% as.matrix()
  
  v_dir_variable <- cov_mat[variable, variable]
  #v_ind_variable <- cov_mat[lag_variable, lag_variable]
  
  v_dir_sqterm <- cov_mat[sqterm, sqterm]
  #v_ind_sqterm <- cov_mat[lag_sqterm, lag_sqterm]
  
  cov_dir <- cov_mat[variable, sqterm]
  #cov_ind <- cov_mat[lag_variable, lag_sqterm]
  
  
  variable_effects <- tibble(model = modname, sqterm = range)
  
  variable_effects <- variable_effects %>% mutate(me_Direct = eff_dir_variable + 2 * eff_dir_sqterm * sqterm,
                                                  #me_Indirect = eff_ind_variable + eff_ind_sqterm * sqterm,
                                                  #me_Total = eff_tot_variable + eff_tot_sqterm * sqterm,
                                                  v_Direct = v_dir_variable + 4 * (sqterm^2) * v_dir_sqterm + 4 * sqterm * cov_dir,
                                                  #v_Indirect = v_ind_variable + (sqterm^2) * v_ind_sqterm + 2 * sqterm * cov_ind,
                                                  #v_Total = v_tot_variable + (sqterm^2) * v_tot_sqterm + 2 * sqterm * cov_tot,
                                                  se_Direct = sqrt(v_Direct), # /n
                                                  #se_Indirect = sqrt(v_Indirect), #/n
                                                  #se_Total = sqrt(v_Total), #/n
                                                  cilow_Direct = me_Direct - qnorm((1 - alpha / 2)) * se_Direct,
                                                  cihigh_Direct = me_Direct + qnorm((1 - alpha / 2)) * se_Direct,
                                                  #cilow_Indirect = me_Indirect - qnorm((1 - alpha / 2)) * se_Indirect,
                                                  #cihigh_Indirect = me_Indirect + qnorm((1 - alpha / 2)) * se_Indirect,
                                                  #cilow_Total = me_Total - qnorm((1 - alpha / 2)) * se_Total,
                                                  #cihigh_Total = me_Total + qnorm((1 - alpha / 2)) * se_Total
  )
  
  variable_effects_long <- variable_effects %>% pivot_longer(-c(model, sqterm), names_to = c(".value", "effect"), names_pattern = "(.*)_(.*)")
  
  return(variable_effects_long)
}