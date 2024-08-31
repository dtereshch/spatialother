impacts_moderation_slx <- function(model, variable, interaction, listw, R = 999, range, alpha = 0.05, n){
  
  library(rlang)
  
  variable <- as_name(enquo(variable))
  interaction <- as_name(enquo(interaction))
  modname <- as_name(enquo(model))
  
  lag_variable <- paste("lag", variable, sep = "_")
  lag_interaction <- paste("lag", interaction, sep = "_")

  coefs <- model[["coefficients"]] %>% as_vector()
  
  eff_dir_variable <- coefs[variable,] %>% as.numeric()
  eff_dir_interaction <- coefs[interaction,] %>% as.numeric()
  eff_ind_variable <- coefs[lag_variable,] %>% as.numeric()
  eff_ind_interaction <- coefs[lag_interaction,] %>% as.numeric()
  #eff_tot
  
  cov_mat <- model[["var"]] %>% as.matrix()
  
  v_dir_variable <- cov_mat[variable, variable]
  v_ind_variable <- cov_mat[lag_variable, lag_variable]
  
  v_dir_interaction <- cov_mat[interaction, interaction]
  v_ind_interaction <- cov_mat[lag_interaction, lag_interaction]
  
  cov_dir <- cov_mat[variable, interaction]
  cov_ind <- cov_mat[lag_variable, lag_interaction]
  
  
  variable_effects <- tibble(model = modname, moderator = range)
  
  variable_effects <- variable_effects %>% mutate(me_Direct = eff_dir_variable + eff_dir_interaction * moderator,
                                                  me_Indirect = eff_ind_variable + eff_ind_interaction * moderator,
                                                  #me_Total = eff_tot_variable + eff_tot_interaction * moderator,
                                                  v_Direct = v_dir_variable + (moderator^2) * v_dir_interaction + 2 * moderator * cov_dir,
                                                  v_Indirect = v_ind_variable + (moderator^2) * v_ind_interaction + 2 * moderator * cov_ind,
                                                  #v_Total = v_tot_variable + (moderator^2) * v_tot_interaction + 2 * moderator * cov_tot,
                                                  se_Direct = sqrt(v_Direct), # /n
                                                  se_Indirect = sqrt(v_Indirect), #/n
                                                  #se_Total = sqrt(v_Total), #/n
                                                  cilow_Direct = me_Direct - qnorm((1 - alpha / 2)) * se_Direct,
                                                  cihigh_Direct = me_Direct + qnorm((1 - alpha / 2)) * se_Direct,
                                                  cilow_Indirect = me_Indirect - qnorm((1 - alpha / 2)) * se_Indirect,
                                                  cihigh_Indirect = me_Indirect + qnorm((1 - alpha / 2)) * se_Indirect,
                                                  #cilow_Total = me_Total - qnorm((1 - alpha / 2)) * se_Total,
                                                  #cihigh_Total = me_Total + qnorm((1 - alpha / 2)) * se_Total
                                                  )
  
  variable_effects_long <- variable_effects %>% pivot_longer(-c(model, moderator), names_to = c(".value", "effect"), names_pattern = "(.*)_(.*)")
  
  return(variable_effects_long)
}