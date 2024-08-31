tidy_spatial <- function(model){
  
  # library(rlang)
  # modna <- as_name(enquo(model))
  
  modla <- as_label(model)
 
  if (modla == "<sphet>") {
    
    coefs <- as.matrix(model$coefficients)
    covma <- as.matrix(model$var)
    
  } else if (modla == "<SDPDm>") {
    
    coefs <- as.matrix(model$coefficients)
    n_coef <- length(coefs)
    covma <- as.matrix(model$varcov)[1:n_coef, 1:n_coef]
    
  } else {
    
    stop("This type of model %s is not supported yet!", modla)
    
  }

  tidy_table <- data.frame(row.names(coefs), 
                           coefs, 
                           sqrt(diag(covma)),
                           2*pnorm((-1)*abs(coefs / sqrt(diag(covma)))))
    
  colnames(tidy_table) <- c("term", "estimate", "std.error", "p.value")
  gl <- data.frame(Model = modla)
  tidy_mod <- list(tidy = tidy_table, glance = gl)
  class(tidy_mod) <- "modelsummary_list"
  
  return(tidy_mod)
}