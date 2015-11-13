# Written by CHJ Hartgerink
# This is a down-and-dirty scripped and I have not double-checked these results
# Primarily wrote this to provide some feedback to pilot participants
# DO NOT USE ANY OF THIS SCRIPTING AS GOOD ANALYSES

# Read in data ------------------------------------------------------------
dat <- read.table('data/pilot_01 cleaned.csv', sep = ',', dec = '.', header = TRUE)

pvals <- list(NULL)
for (i in 1:length(unique(dat$id))){
  sel <- dat$id == unique(dat$id)[i]
  alpha <- .05
  
  # The object to put the p-values for each individual test per respondent
  pvals_test <- NULL
  # The object to put the combined result of all methods per id
  pvals_id <- NULL
  
  # Conduct reverse Fisher method where null effect is expected -------------
  # Includes gender condition (1 per study)
  # And interaction conditions (1 per study)
  # Each respondent has 1 + 1 * #studies of fabricated nonsignificant results
  pvals_ln1minp <- c(dat$gender_p[sel], dat$genderXcondition_p[sel])
  ln1minp <- -2 * sum(log(1 - pvals_ln1minp))
  pvals_test[1] <- pchisq(q = ln1minp, df = 2 * length(pvals_ln1minp), lower.tail = FALSE)
  
  # Conduct Simonsohn method of variance of variances -----------------------
  if (unique(dat$id)[i] == "R_124DW2C8FOWUJJn"){
    pvals_test[2] <- NA
  } else {
    m1 <- c(dat$male_low_m[sel & dat$study == 1],
            dat$male_high_m[sel & dat$study == 1],
            dat$female_low_m[sel & dat$study == 1],
            dat$female_high_m[sel & dat$study == 1])
    m2 <- c(dat$male_low_m[sel & dat$study == 2],
            dat$male_high_m[sel & dat$study == 2],
            dat$female_low_m[sel & dat$study == 2],
            dat$female_high_m[sel & dat$study == 2])
    m3 <- c(dat$male_low_m[sel & dat$study == 3],
            dat$male_high_m[sel & dat$study == 3],
            dat$female_low_m[sel & dat$study == 3],
            dat$female_high_m[sel & dat$study == 3])
    m4 <- c(dat$male_low_m[sel & dat$study == 4],
            dat$male_high_m[sel & dat$study == 4],
            dat$female_low_m[sel & dat$study == 4],
            dat$female_high_m[sel & dat$study == 4])
    
    sd1 <- c(dat$male_low_sd[sel & dat$study == 1],
             dat$male_high_sd[sel & dat$study == 1],
             dat$female_low_sd[sel & dat$study == 1],
             dat$female_high_sd[sel & dat$study == 1])
    sd2 <- c(dat$male_low_sd[sel & dat$study == 2],
             dat$male_high_sd[sel & dat$study == 2],
             dat$female_low_sd[sel & dat$study == 2],
             dat$female_high_sd[sel & dat$study == 2])
    sd3 <- c(dat$male_low_sd[sel & dat$study == 3],
             dat$male_high_sd[sel & dat$study == 3],
             dat$female_low_sd[sel & dat$study == 3],
             dat$female_high_sd[sel & dat$study == 3])
    sd4 <- c(dat$male_low_sd[sel & dat$study == 4],
             dat$male_high_sd[sel & dat$study == 4],
             dat$female_low_sd[sel & dat$study == 4],
             dat$female_high_sd[sel & dat$study == 4])
    
    n1 <- c(dat$male_low_n[sel & dat$study == 1],
            dat$male_high_n[sel & dat$study == 1],
            dat$female_low_n[sel & dat$study == 1],
            dat$female_high_n[sel & dat$study == 1])
    n2 <- c(dat$male_low_n[sel & dat$study == 2],
            dat$male_high_n[sel & dat$study == 2],
            dat$female_low_n[sel & dat$study == 2],
            dat$female_high_n[sel & dat$study == 2])
    n3 <- c(dat$male_low_n[sel & dat$study == 3],
            dat$male_high_n[sel & dat$study == 3],
            dat$female_low_n[sel & dat$study == 3],
            dat$female_high_n[sel & dat$study == 3])
    n4 <- c(dat$male_low_n[sel & dat$study == 4],
            dat$male_high_n[sel & dat$study == 4],
            dat$female_low_n[sel & dat$study == 4],
            dat$female_high_n[sel & dat$study == 4])
    
    # Do not know how to do this for different sample sizes yet
    # For pilot I just used the mean
    se1 <- mean(sd1) / sqrt(2 * mean(n1))
    se2 <- mean(sd2) / sqrt(2 * mean(n2))
    se3 <- mean(sd3) / sqrt(2 * mean(n3))
    se4 <- mean(sd4) / sqrt(2 * mean(n4))
    
    psi1 <- sd(sd1) / se1
    psi2 <- sd(sd2) / se2
    psi3 <- sd(sd3) / se3
    psi4 <- sd(sd4) / se4
    
    psi <- mean(c(psi1, psi2, psi3, psi4))
    
    n.iter <- 100000
    spsi <- NULL
    
    for(j in 1:n.iter){
      ssd1 <- c(sd(rnorm(n1[1], m1[1], mean(sd1))),
                sd(rnorm(n1[2], m1[2], mean(sd1))),
                sd(rnorm(n1[3], m1[3], mean(sd1))),
                sd(rnorm(n1[4], m1[4], mean(sd1))))
      ssd2 <- c(sd(rnorm(n2[1], m2[1], mean(sd2))),
                sd(rnorm(n2[2], m2[2], mean(sd2))),
                sd(rnorm(n2[3], m2[3], mean(sd2))),
                sd(rnorm(n2[4], m2[4], mean(sd2))))
      ssd3 <- c(sd(rnorm(n3[1], m3[1], mean(sd3))),
                sd(rnorm(n3[2], m3[2], mean(sd3))),
                sd(rnorm(n3[3], m3[3], mean(sd3))),
                sd(rnorm(n3[4], m3[4], mean(sd3))))
      ssd4 <- c(sd(rnorm(n4[1], m4[1], mean(sd4))),
                sd(rnorm(n4[2], m4[2], mean(sd4))),
                sd(rnorm(n4[3], m4[3], mean(sd4))),
                sd(rnorm(n4[4], m4[4], mean(sd4))))
      se1 <- mean(ssd1) / sqrt(2 * mean(n1))
      se2 <- mean(ssd2) / sqrt(2 * mean(n2))
      se3 <- mean(ssd3) / sqrt(2 * mean(n3))
      se4 <- mean(ssd4) / sqrt(2 * mean(n4))
      
      spsi1 <- sd(ssd2) / se2
      spsi2 <- sd(ssd2) / se2
      spsi3 <- sd(ssd3) / se3
      spsi4 <- sd(ssd4) / se4
      
      spsi[j] <- mean(c(spsi1, spsi2, spsi3, spsi4))}
    if(psi > mean(spsi)){
    pvals_test[2] <- sum(spsi >= psi) / n.iter} else {
      pvals_test[2] <- sum(spsi <= psi) / n.iter
    }
  }
  
  print(pvals_test)
  pvals[[i]] <- pvals_test[!is.na(pvals_test)]
}


# Results per id for combined test ----------------------------------------
#only full cases

# R_3hcMrBwyRMxkuOw
pchisq(q = -2 * sum(log(pvals[[1]])), df = 2 * length(pvals[[1]]), lower.tail = FALSE)
# R_BXLJRT0jFkm9kM9
pchisq(q = -2 * sum(log(pvals[[2]])), df = 2 * length(pvals[[2]]), lower.tail = FALSE)
