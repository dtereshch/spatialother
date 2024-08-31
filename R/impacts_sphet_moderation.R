impacts_moderation <- function(model, variable, interaction, range, alpha = 0.05, 
                               listw, R = 999, n){
  
  library(rlang)
  
  variable <- as_name(enquo(variable))
  interaction <- as_name(enquo(interaction))
  modname <- as_name(enquo(model))
  
  if (model[["method"]] == "gmm lag") {
    
    effects <- sphet::impacts(model, listw = listw, R = R)
    
    eff_direct <- effects[["sres"]][["direct"]] %>% as_tibble() %>% 
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
    eff_indirect <- effects[["sres"]][["indirect"]] %>% as_tibble() %>% 
      summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
    # eff_total <- effects[["sres"]][["total"]] %>% as_tibble() %>% 
    #   summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
    
    # covariance matrices
    cov_direct <- effects[["sres"]][["direct"]] %>% as_tibble() %>% cov()
    cov_indirect <- effects[["sres"]][["indirect"]] %>% as_tibble() %>% cov()
    #cov_total <- effects[["sres"]][["total"]] %>% as_tibble() %>% cov()
    
    eff_dir_variable <- eff_direct[variable] %>% as.numeric()
    eff_dir_interaction <- eff_direct[interaction] %>% as.numeric()
    eff_ind_variable <- eff_indirect[variable] %>% as.numeric()
    eff_ind_interaction <- eff_indirect[interaction] %>% as.numeric()
    #eff_tot_variable <- eff_total[variable] %>% as.numeric()
    #eff_tot_interaction <- eff_total[interaction] %>% as.numeric()
    
    v_dir_variable <- cov_direct[variable, variable]
    v_ind_variable <- cov_indirect[variable, variable]
    #v_tot_variable <- cov_total[variable, variable]
    
    v_dir_interaction <- cov_direct[interaction, interaction]
    v_ind_interaction <- cov_indirect[interaction, interaction]
    #v_tot_interaction <- cov_total[interaction, interaction]
    
    cov_dir <- cov_direct[variable, interaction]
    cov_ind <- cov_indirect[variable, interaction]
    #cov_tot <- cov_total[variable, interaction]
    
    
    variable_effects <- tibble(model = modname, moderator = range)
    
    variable_effects <- variable_effects %>% mutate(me_Direct = eff_dir_variable + eff_dir_interaction * moderator,
                                                    me_Indirect = eff_ind_variable + eff_ind_interaction * moderator,
                                                    #me_Total = eff_tot_variable + eff_tot_interaction * moderator,
                                                    v_Direct = v_dir_variable + (moderator^2) * v_dir_interaction + 2 * moderator * cov_dir,
                                                    v_Indirect = v_ind_variable + (moderator^2) * v_ind_interaction + 2 * moderator * cov_ind,
                                                    #v_Total = v_tot_variable + (moderator^2) * v_tot_interaction + 2 * moderator * cov_tot,
                                                    se_Direct = sqrt(v_Direct / R),
                                                    se_Indirect = sqrt(v_Indirect / R),
                                                    #se_Total = sqrt(v_Total / n),
                                                    cilow_Direct = me_Direct - qnorm((1 - alpha / 2)) * se_Direct,
                                                    cihigh_Direct = me_Direct + qnorm((1 - alpha / 2)) * se_Direct,
                                                    cilow_Indirect = me_Indirect - qnorm((1 - alpha / 2)) * se_Indirect,
                                                    cihigh_Indirect = me_Indirect + qnorm((1 - alpha / 2)) * se_Indirect,
                                                    #cilow_Total = me_Total - qnorm((1 - alpha / 2)) * se_Total,
                                                    #cihigh_Total = me_Total + qnorm((1 - alpha / 2)) * se_Total
    )
    
  } else if (model[["Durbin"]] != FALSE) {
    
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
    
  } else {
    
    # lag_variable <- paste("lag", variable, sep = "_")
    # lag_interaction <- paste("lag", interaction, sep = "_")
    
    coefs <- model[["coefficients"]] %>% as_vector()
    
    eff_dir_variable <- coefs[variable,] %>% as.numeric()
    eff_dir_interaction <- coefs[interaction,] %>% as.numeric()
    # eff_ind_variable <- coefs[lag_variable,] %>% as.numeric()
    # eff_ind_interaction <- coefs[lag_interaction,] %>% as.numeric()
    #eff_tot
    
    cov_mat <- model[["var"]] %>% as.matrix()
    
    v_dir_variable <- cov_mat[variable, variable]
    #v_ind_variable <- cov_mat[lag_variable, lag_variable]
    
    v_dir_interaction <- cov_mat[interaction, interaction]
    #v_ind_interaction <- cov_mat[lag_interaction, lag_interaction]
    
    cov_dir <- cov_mat[variable, interaction]
    #cov_ind <- cov_mat[lag_variable, lag_interaction]
    
    
    variable_effects <- tibble(model = modname, moderator = range)
    
    variable_effects <- variable_effects %>% mutate(me_Direct = eff_dir_variable + eff_dir_interaction * moderator,
                                                    #me_Indirect = eff_ind_variable + eff_ind_interaction * moderator,
                                                    #me_Total = eff_tot_variable + eff_tot_interaction * moderator,
                                                    v_Direct = v_dir_variable + (moderator^2) * v_dir_interaction + 2 * moderator * cov_dir,
                                                    #v_Indirect = v_ind_variable + (moderator^2) * v_ind_interaction + 2 * moderator * cov_ind,
                                                    #v_Total = v_tot_variable + (moderator^2) * v_tot_interaction + 2 * moderator * cov_tot,
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
    
  }
  
  variable_effects_long <- variable_effects %>% pivot_longer(-c(model, moderator), names_to = c(".value", "effect"), names_pattern = "(.*)_(.*)")
  
  return(variable_effects_long)
  
  }