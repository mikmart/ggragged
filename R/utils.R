iapply <- function(X, IND, FUN, ...) {
  replace(X, IND, lapply(X[IND], FUN, ...))
}
