impactsSDPDm_sq <- function(model, variable, sqterm, interact, interact2, nsim = 999, range, alpha = 0.05, n){
  
  library(rlang)
  source("functions/impactsSDPDm_simres.R")
  
  variable <- as_name(enquo(variable))
  sqterm <- as_name(enquo(sqterm))
  interact <- as_name(enquo(interact))
  interact2 <- as_name(enquo(interact2))
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
  eff_dir_st_sqterm <- eff_dir_st[sqterm] %>% as.numeric()
  eff_dir_st_interact <- eff_dir_st[interact] %>% as.numeric()
  eff_dir_st_interact2 <- eff_dir_st[interact2] %>% as.numeric()
  eff_ind_st_variable <- eff_ind_st[variable] %>% as.numeric()
  eff_ind_st_sqterm <- eff_ind_st[sqterm] %>% as.numeric()
  eff_ind_st_interact <- eff_ind_st[interact] %>% as.numeric()
  eff_ind_st_interact2 <- eff_ind_st[interact2] %>% as.numeric()
  eff_tot_st_variable <- eff_tot_st[variable] %>% as.numeric()
  eff_tot_st_sqterm <- eff_tot_st[sqterm] %>% as.numeric()
  eff_tot_st_interact <- eff_tot_st[interact] %>% as.numeric()
  eff_tot_st_interact2 <- eff_tot_st[interact2] %>% as.numeric()
  
  eff_dir_lt_variable <- eff_dir_lt[variable] %>% as.numeric()
  eff_dir_lt_sqterm <- eff_dir_lt[sqterm] %>% as.numeric()
  eff_dir_lt_interact <- eff_dir_lt[interact] %>% as.numeric()
  eff_dir_lt_interact2 <- eff_dir_lt[interact2] %>% as.numeric()
  eff_ind_lt_variable <- eff_ind_lt[variable] %>% as.numeric()
  eff_ind_lt_sqterm <- eff_ind_lt[sqterm] %>% as.numeric()
  eff_ind_lt_interact <- eff_ind_lt[interact] %>% as.numeric()
  eff_ind_lt_interact2 <- eff_ind_lt[interact2] %>% as.numeric()
  eff_tot_lt_variable <- eff_tot_lt[variable] %>% as.numeric()
  eff_tot_lt_sqterm <- eff_tot_lt[sqterm] %>% as.numeric()
  eff_tot_lt_interact <- eff_tot_lt[interact] %>% as.numeric()
  eff_tot_lt_interact2 <- eff_tot_lt[interact2] %>% as.numeric()
  
  v_dir_st_variable <- cov_dir_st[variable, variable]
  v_ind_st_variable <- cov_ind_st[variable, variable]
  v_tot_st_variable <- cov_tot_st[variable, variable]
  v_dir_st_sqterm <- cov_dir_st[sqterm, sqterm]
  v_ind_st_sqterm <- cov_ind_st[sqterm, sqterm]
  v_tot_st_sqterm <- cov_tot_st[sqterm, sqterm]
  v_dir_st_interact <- cov_dir_st[interact, interact]
  v_ind_st_interact <- cov_ind_st[interact, interact]
  v_tot_st_interact <- cov_tot_st[interact, interact]
  v_dir_st_interact2 <- cov_dir_st[interact2, interact2]
  v_ind_st_interact2 <- cov_ind_st[interact2, interact2]
  v_tot_st_interact2 <- cov_tot_st[interact2, interact2]
  
  cov_dir_st <- cov_dir_st[variable, sqterm]
  cov_ind_st <- cov_ind_st[variable, sqterm]
  cov_tot_st <- cov_tot_st[variable, sqterm]
  #add
  
  v_dir_lt_variable <- cov_dir_lt[variable, variable]
  v_ind_lt_variable <- cov_ind_lt[variable, variable]
  v_tot_lt_variable <- cov_tot_lt[variable, variable]
  v_dir_lt_sqterm <- cov_dir_lt[sqterm, sqterm]
  v_ind_lt_sqterm <- cov_ind_lt[sqterm, sqterm]
  v_tot_lt_sqterm <- cov_tot_lt[sqterm, sqterm]
  v_dir_lt_interact <- cov_dir_lt[interact, interact]
  v_ind_lt_interact <- cov_ind_lt[interact, interact]
  v_tot_lt_interact <- cov_tot_lt[interact, interact]
  v_dir_lt_interact2 <- cov_dir_lt[interact2, interact2]
  v_ind_lt_interact2 <- cov_ind_lt[interact2, interact2]
  v_tot_lt_interact2 <- cov_tot_lt[interact2, interact2]
  
  cov_dir_lt <- cov_dir_lt[variable, sqterm]
  cov_ind_lt <- cov_ind_lt[variable, sqterm]
  cov_tot_lt <- cov_tot_lt[variable, sqterm]
  #add
  
  variable_effects <- tibble(model = modname, sqterm = range)
  
  variable_effects <- variable_effects %>% mutate(me_Direct.ST = eff_dir_st_variable + 2 * eff_dir_st_sqterm * sqterm,
                                                  me_Indirect.ST = eff_ind_st_variable + 2 * eff_ind_st_sqterm * sqterm,
                                                  me_Total.ST = eff_tot_st_variable + 2 * eff_tot_st_sqterm * sqterm,
                                                  v_Direct.ST = v_dir_st_variable + 4 * (sqterm^2) * v_dir_st_sqterm + 4 * sqterm * cov_dir_st,
                                                  v_Indirect.ST = v_ind_st_variable + 4 * (sqterm^2) * v_ind_st_sqterm + 4 * sqterm * cov_ind_st,
                                                  v_Total.ST = v_tot_st_variable + 4 * (sqterm^2) * v_tot_st_sqterm + 4 * sqterm * cov_tot_st,
                                                  se_Direct.ST = sqrt(v_Direct.ST / nsim),
                                                  se_Indirect.ST = sqrt(v_Indirect.ST / nsim),
                                                  se_Total.ST = sqrt(v_Total.ST / nsim),
                                                  cilow_Direct.ST = me_Direct.ST - qnorm((1 - alpha / 2)) * se_Direct.ST,
                                                  cihigh_Direct.ST = me_Direct.ST + qnorm((1 - alpha / 2)) * se_Direct.ST,
                                                  cilow_Indirect.ST = me_Indirect.ST - qnorm((1 - alpha / 2)) * se_Indirect.ST,
                                                  cihigh_Indirect.ST = me_Indirect.ST + qnorm((1 - alpha / 2)) * se_Indirect.ST,
                                                  cilow_Total.ST = me_Total.ST - qnorm((1 - alpha / 2)) * se_Total.ST,
                                                  cihigh_Total.ST = me_Total.ST + qnorm((1 - alpha / 2)) * se_Total.ST,
                                                  me_Direct.LT = eff_dir_lt_variable + 2 * eff_dir_lt_sqterm * sqterm,
                                                  me_Indirect.LT = eff_ind_lt_variable + 2 * eff_ind_lt_sqterm * sqterm,
                                                  me_Total.LT = eff_tot_lt_variable + 2 * eff_tot_lt_sqterm * sqterm,
                                                  v_Direct.LT = v_dir_lt_variable + 4 * (sqterm^2) * v_dir_lt_sqterm + 4 * sqterm * cov_dir_lt,
                                                  v_Indirect.LT = v_ind_lt_variable + 4 * (sqterm^2) * v_ind_lt_sqterm + 4 * sqterm * cov_ind_lt,
                                                  v_Total.LT = v_tot_lt_variable + 4 * (sqterm^2) * v_tot_lt_sqterm + 4 * sqterm * cov_tot_lt,
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
    pivot_longer(-c(model, sqterm), names_to = c(".value", "effect"), names_pattern = "(.*)_(.*)") %>%
    separate_wider_delim(effect, ".", names = c("effect", "period"))
  
  return(variable_effects_long)
}