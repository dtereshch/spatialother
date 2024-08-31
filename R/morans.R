morans <- function(data, period, var, mat) {
  
  require(dplyr)
  require(purrr)
  require(spdep)
  
  moran <- data %>% 
    dplyr::filter(year == period) %>% 
    dplyr::select(var) %>% 
    purrr::as_vector() %>% 
    spdep::moran.test(mat, alternative = "greater")
  
  return(c(year = period, I = moran[["estimate"]][["Moran I statistic"]], p = moran[["p.value"]]))
}