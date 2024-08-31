impactsSDPDm_moderation <- function(model, variable, interaction, nsim = 999, range, alpha = 0.05, n){
  
  library(rlang)
  source("functions/impactsSDPDm_simres.R")
  
  variable <- as_name(enquo(variable))
  interaction <- as_name(enquo(interaction))
  modname <- as_name(enquo(model))
  
  effects <- impactsSDPDm_simres(model, NSIM = nsim)
  
  # average efects
  eff_dir_st <- effects[["DIRECTst.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  eff_ind_st <- effects[["INDIRECTst.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  eff_tot_st <- effects[["TOTALst.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

  eff_dir_lt <- effects[["DIRECTlt.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  eff_ind_lt <- effects[["INDIRECTlt.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  eff_tot_lt <- effects[["TOTALlt.sres"]] %>% as_tibble() %>% 
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
  
  # covariance matrices
  cov_dir_st <- effects[["DIRECTst.sres"]] %>% as_tibble() %>% cov()
  cov_ind_st <- effects[["INDIRECTst.sres"]] %>% as_tibble() %>% cov()
  cov_tot_st <- effects[["TOTALst.sres"]] %>% as_tibble() %>% cov()
  
  cov_dir_lt <- effects[["DIRECTlt.sres"]] %>% as_tibble() %>% cov()
  cov_ind_lt <- effects[["INDIRECTlt.sres"]] %>% as_tibble() %>% cov()
  cov_tot_lt <- effects[["TOTALlt.sres"]] %>% as_tibble() %>% cov()
  
  eff_dir_st_variable <- eff_dir_st[variable] %>% as.numeric()
  eff_dir_st_interaction <- eff_dir_st[interaction] %>% as.numeric()
  eff_ind_st_variable <- eff_ind_st[variable] %>% as.numeric()
  eff_ind_st_interaction <- eff_ind_st[interaction] %>% as.numeric()
  eff_tot_st_variable <- eff_tot_st[variable] %>% as.numeric()
  eff_tot_st_interaction <- eff_tot_st[interaction] %>% as.numeric()
  
  eff_dir_lt_variable <- eff_dir_lt[variable] %>% as.numeric()
  eff_dir_lt_interaction <- eff_dir_lt[interaction] %>% as.numeric()
  eff_ind_lt_variable <- eff_ind_lt[variable] %>% as.numeric()
  eff_ind_lt_interaction <- eff_ind_lt[interaction] %>% as.numeric()
  eff_tot_lt_variable <- eff_tot_lt[variable] %>% as.numeric()
  eff_tot_lt_interaction <- eff_tot_lt[interaction] %>% as.numeric()
  
  v_dir_st_variable <- cov_dir_st[variable, variable]
  v_ind_st_variable <- cov_ind_st[variable, variable]
  v_tot_st_variable <- cov_tot_st[variable, variable]
  v_dir_st_interaction <- cov_dir_st[interaction, interaction]
  v_ind_st_interaction <- cov_ind_st[interaction, interaction]
  v_tot_st_interaction <- cov_tot_st[interaction, interaction]
  cov_dir_st <- cov_dir_st[variable, interaction]
  cov_ind_st <- cov_ind_st[variable, interaction]
  cov_tot_st <- cov_tot_st[variable, interaction]
  
  v_dir_lt_variable <- cov_dir_lt[variable, variable]
  v_ind_lt_variable <- cov_ind_lt[variable, variable]
  v_tot_lt_variable <- cov_tot_lt[variable, variable]
  v_dir_lt_interaction <- cov_dir_lt[interaction, interaction]
  v_ind_lt_interaction <- cov_ind_lt[interaction, interaction]
  v_tot_lt_interaction <- cov_tot_lt[interaction, interaction]
  cov_dir_lt <- cov_dir_lt[variable, interaction]
  cov_ind_lt <- cov_ind_lt[variable, interaction]
  cov_tot_lt <- cov_tot_lt[variable, interaction]
  
  variable_effects <- tibble(model = modname, moderator = range)
  
  variable_effects <- variable_effects %>% mutate(me_Direct.ST = eff_dir_st_variable + eff_dir_st_interaction * moderator,
                                                  me_Indirect.ST = eff_ind_st_variable + eff_ind_st_interaction * moderator,
                                                  me_Total.ST = eff_tot_st_variable + eff_tot_st_interaction * moderator,
                                                  v_Direct.ST = v_dir_st_variable + (moderator^2) * v_dir_st_interaction + 2 * moderator * cov_dir_st,
                                                  v_Indirect.ST = v_ind_st_variable + (moderator^2) * v_ind_st_interaction + 2 * moderator * cov_ind_st,
                                                  v_Total.ST = v_tot_st_variable + (moderator^2) * v_tot_st_interaction + 2 * moderator * cov_tot_st,
                                                  se_Direct.ST = sqrt(v_Direct.ST / nsim),
                                                  se_Indirect.ST = sqrt(v_Indirect.ST / nsim),
                                                  se_Total.ST = sqrt(v_Total.ST / nsim),
                                                  cilow_Direct.ST = me_Direct.ST - qnorm((1 - alpha / 2)) * se_Direct.ST,
                                                  cihigh_Direct.ST = me_Direct.ST + qnorm((1 - alpha / 2)) * se_Direct.ST,
                                                  cilow_Indirect.ST = me_Indirect.ST - qnorm((1 - alpha / 2)) * se_Indirect.ST,
                                                  cihigh_Indirect.ST = me_Indirect.ST + qnorm((1 - alpha / 2)) * se_Indirect.ST,
                                                  cilow_Total.ST = me_Total.ST - qnorm((1 - alpha / 2)) * se_Total.ST,
                                                  cihigh_Total.ST = me_Total.ST + qnorm((1 - alpha / 2)) * se_Total.ST,
                                                  me_Direct.LT = eff_dir_lt_variable + eff_dir_lt_interaction * moderator,
                                                  me_Indirect.LT = eff_ind_lt_variable + eff_ind_lt_interaction * moderator,
                                                  me_Total.LT = eff_tot_lt_variable + eff_tot_lt_interaction * moderator,
                                                  v_Direct.LT = v_dir_lt_variable + (moderator^2) * v_dir_lt_interaction + 2 * moderator * cov_dir_lt,
                                                  v_Indirect.LT = v_ind_lt_variable + (moderator^2) * v_ind_lt_interaction + 2 * moderator * cov_ind_lt,
                                                  v_Total.LT = v_tot_lt_variable + (moderator^2) * v_tot_lt_interaction + 2 * moderator * cov_tot_lt,
                                                  se_Direct.LT = sqrt(v_Direct.LT / nsim),
                                                  se_Indirect.LT = sqrt(v_Indirect.LT / nsim),
                                                  se_Total.LT = sqrt(v_Total.LT / nsim),
                                                  cilow_Direct.LT = me_Direct.LT - qnorm((1 - alpha / 2)) * se_Direct.LT,
                                                  cihigh_Direct.LT = me_Direct.LT + qnorm((1 - alpha / 2)) * se_Direct.LT,
                                                  cilow_Indirect.LT = me_Indirect.LT - qnorm((1 - alpha / 2)) * se_Indirect.LT,
                                                  cihigh_Indirect.LT = me_Indirect.LT + qnorm((1 - alpha / 2)) * se_Indirect.LT,
                                                  cilow_Total.LT = me_Total.LT - qnorm((1 - alpha / 2)) * se_Total.LT,
                                                  cihigh_Total.LT = me_Total.LT + qnorm((1 - alpha / 2)) * se_Total.LT
  )
  
  variable_effects_long <- variable_effects %>% 
    pivot_longer(-c(model, moderator), names_to = c(".value", "effect"), names_pattern = "(.*)_(.*)") %>%
    separate_wider_delim(effect, ".", names = c("effect", "period"))
  
  return(variable_effects_long)
}