std_var <- function(n, sds, iter = 1000, ...)
{
  msw_obs <- msw(n, sds)
  z2_sd_obs <- z2_sd(sds = sds, msw = msw_obs)
  
  obs_sd_z2 <- sd(z2_sd_obs)
  
  # Create a column of simulated variances per cell
  tmp <- apply(t(n - 1), 2, function(x)
  {
    step1 <- rchisq(n = iter, df = x)
    step2 <- step1 / x
  })
  
  # Compute msw for simulated variances (per row)
  msw_sim <- apply(tmp, 1, function(x) msw(n, x))
  
  # Normalize simulated variances
  sim_z2 <- z2_sd(sds = tmp, msw = msw_sim)
  # Calculate SD for each iteration
  sim_sd_z2 <- apply(sim_z2, 1, sd)
  
  res <- sum(sim_sd_z2 < obs_sd_z2) / iter
  
  return(res)
}

msw <- function(
  n,
  sds
)
{
  if (is.null(n)) stop("Please specify n")
  if (is.null(sds)) stop("Please specify sd")
  
  res <- sum((n - 1) * sds^2) / sum((n - 1))
  
  return(res)
}

z2_sd <- function(sds, var, msw)
{
  # Implement check that not both sd and var are given
  res <- sds^2 / msw
  
  return(res)
}
