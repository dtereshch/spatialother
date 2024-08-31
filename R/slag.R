slag <- function(data, x, wmat, prefix = "w"){
  
  require(dplyr)
  
  xmat <- data %>% dplyr::select(x) %>% as.matrix()
  wx <- wmat %*% xmat
  colnames(wx) <- paste(prefix, colnames(wx), sep = "")
  data_wx <- data %>% dplyr::bind_cols(wx)
  
  return(data_wx)
}