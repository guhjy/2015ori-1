fisher_method <- function(pval,
                          adjustment = .0000001)
{
  if (adjustment > 0)
  {
    pval <- ifelse(pval == 0, pval + adjustment, pval)
  }
  
  fish <- -2 * sum(log(pval))
  df_fish <- length(pval) * 2
  p_fish <- pchisq(q = fish, df = df_fish, lower.tail = FALSE)
  
  return(cbind(fish, df_fish, p_fish))
}