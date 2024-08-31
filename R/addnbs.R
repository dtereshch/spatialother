addnbs <- function(sp.sample){
  
  require(sp)
  require(spdep)
  
  queen_nb <- spdep::poly2nb(sp.sample, row.names = sp.sample$ID, queen = TRUE)
  
  count = card(queen_nb)
  if (!any(count == 0)) {
    return(queen_nb)
  }
  
  ## get nearest neighbour index, use centroids:
  nnbs = spdep::knearneigh(sp::coordinates(sp.sample))$nn
  
  no_edges_from = which(count == 0)
  for (i in no_edges_from) {
    queen_nb[[i]] = nnbs[i]
  }
  return(queen_nb)
}

# Soruce: https://stackoverflow.com/questions/57269254/how-to-impute-missing-neighbours-of-a-spatial-weight-matrix-queen-contiguity