# A function that runs basis risk estimates for all periods using NCREIF data

basis_risk <- function(ds, df) {

  ### Simple linear regression only ###

  if (length(ds) != length(df)) stop("Basis risk regression badly specified.")
  n <- length(ds)
  k <- 1
  
  # Parameter estimation with bias term
  val_x <- matrix(c(rep(1, n), ds), nrow = n, ncol = 2)
  val_y <- as.matrix(df)
  est_b <- solve(t(val_x) %*% val_x) %*% t(val_x) %*% val_y
  
  # Regression summary
  est_y <- val_x %*% est_b
  est_r <- val_y - est_y
  df_n <- n - (k + 1)
  est_sigma <- sqrt(sum(est_r ^ 2) / df_n) 
  
  # T Values
  est_seb1 <- est_sigma / sqrt(sum((ds - mean(ds)) ^ 2))
  est_tb1 <- est_b[2, 1] / est_seb1
  pval <- 2 * pt(abs(est_tb1), df_n, lower.tail = FALSE)
  
  # Return
  return(list(beta = est_b[2, 1], statistic = est_tb1, pvalue = pval))
    
}

# Test case ---------------------------------------------------------------
# ds <- 1:20
# df <- 2 * ds + rnorm(n = 20, mean = 2, sd = 3)
# basis_risk(ds, df)
# summary(lm(df ~ ds, data = data.frame(df, ds)))

