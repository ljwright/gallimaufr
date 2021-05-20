reverse <- function(x){
  max(x, na.rm = TRUE) - x + 1
}

wtd_scale <- function(x, weight = NULL, n_sd = 1, na_rm = TRUE){
  mean <- wtd.mean(x, weights = weight, na.rm = na_rm)
  sd <- wtd.var(x, weights = weight, na.rm = na_rm) %>%
    sqrt()

  (x - mean) / (n_sd * sd)
}
