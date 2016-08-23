options(digits = 20)

# Read the data
ml <- read.csv(ml_file, stringsAsFactors = FALSE)
qualtrics <- read.csv(qualtrics_file, stringsAsFactors = FALSE)

ml$stat_know <- NA
ml$spss <- NA
ml$rcran <- NA
ml$stata <- NA
ml$sas <- NA
ml$matlab <- NA
ml$python <- NA
ml$other <- NA
ml$nopro <- NA
ml$noanch <- NA
ml$rng <- NA
ml$full <- NA
ml$tries <- NA
ml$descr <- NA
ml$bonus <- NA
ml$type <- 'ml'
qualtrics$type <- 'qualtrics'

dat <- rbind(qualtrics, ml)

# conduct the analyses iteratively
for (pp in unique(dat$referrer))
{
  # Loop through each study to compute the ANOVA results
  for (j in 1:4)
  {
    indexor <- dat$referrer == pp & dat$study == j
    
    harm_mean <- 4^2/(sum(1/dat$anch_n[indexor]))
    male_mean <- mean(dat$anch_m[indexor & dat$partgender == 0])
    female_mean <- mean(dat$anch_m[indexor & dat$partgender == 1])
    control_mean <- mean(dat$anch_m[indexor & dat$anchoring == 0])
    exp_mean <- mean(dat$anch_m[indexor & dat$anchoring == 1])
    
    ss_error <- sum((dat$anch_n[indexor] - 1) * dat$anch_sd[indexor]^2)
    ss_gender <- (harm_mean * var(c(male_mean, female_mean))) / 2
    ss_condition <- (harm_mean * var(c(control_mean, exp_mean))) / 2
    ss_interaction <- harm_mean * var(dat$anch_m[indexor]) * 3/4 - ss_gender - ss_condition
    ss_total <- ss_error + ss_gender + ss_condition + ss_interaction
    
    ms_error <- ss_error / (sum(dat$anch_n[indexor]) - 4)
    ms_gender <- ss_gender / (2 - 1)
    ms_condition <- ss_condition / (2 - 1)
    ms_interaction <- ss_interaction / 1
    
    dat$f_gender[indexor] <- ms_gender / ms_error
    dat$df1_gender[indexor] <- 2 - 1
    dat$df2_gender[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_gender[indexor] <- pf(q = ms_gender / ms_error,
                                df1 = 2 - 1,
                                df2 = sum(dat$anch_n[indexor]) - 4,
                                lower.tail = FALSE)
    dat$es2_gender[indexor] <- 
      (((ms_gender / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_gender / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) + 1)
    
    dat$f_condition[indexor] <- ms_condition / ms_error
    dat$df1_condition[indexor] <- 2 - 1
    dat$df2_condition[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_condition[indexor] <- pf(q = ms_condition / ms_error,
                                   df1 = 2 - 1,
                                   df2 = sum(dat$anch_n[indexor]) - 4,
                                   lower.tail = FALSE)
    dat$es2_condition[indexor] <- 
      (((ms_condition / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_condition / ms_error) * (2 -1)) / (sum(dat$anch_n[indexor]) - 4)) + 1)
    
    dat$f_interaction[indexor] <- ms_interaction / ms_error
    dat$df1_interaction[indexor] <- 1
    dat$df2_interaction[indexor] <- sum(dat$anch_n[indexor]) - 4
    dat$p_interaction[indexor] <- pf(q = ms_interaction / ms_error,
                                     df1 = 1,
                                     df2 = sum(dat$anch_n[indexor]) - 4,
                                     lower.tail = FALSE)
    dat$es2_interaction[indexor] <- 
      (((ms_interaction / ms_error) * 1) / (sum(dat$anch_n[indexor]) - 4)) / 
      ((((ms_interaction / ms_error) * 1) / (sum(dat$anch_n[indexor]) - 4)) + 1)
  }

sel <- dat$referrer == pp

# Variance analysis

# ---
# Assuming homogeneity of variances for all ANCHORING condition
# ---
# Standardizing the sds
# Study 1
sel1 <- dat$referrer == pp & dat$study == 1
ms_w_1 <- sum((dat$anch_n[sel1] - 1) * dat$anch_sd[sel1]^2) / sum((dat$anch_n[sel1] - 1))
tilde_s2_1 <- dat$anch_sd[sel1]^2 / ms_w_1
# Study 2
sel2 <- dat$referrer == pp & dat$study == 2
ms_w_2 <- sum((dat$anch_n[sel2] - 1) * dat$anch_sd[sel2]^2) / sum((dat$anch_n[sel2] - 1))
tilde_s2_2 <- dat$anch_sd[sel2]^2 / ms_w_2
# Study 3
sel3 <- dat$referrer == pp & dat$study == 3
ms_w_3 <- sum((dat$anch_n[sel3] - 1) * dat$anch_sd[sel3]^2) / sum((dat$anch_n[sel3] - 1))
tilde_s2_3 <- dat$anch_sd[sel3]^2 / ms_w_3
# Study 4
sel4 <- dat$referrer == pp & dat$study == 4
ms_w_4 <- sum((dat$anch_n[sel4] - 1) * dat$anch_sd[sel4]^2) / sum((dat$anch_n[sel4] - 1))
tilde_s2_4 <- dat$anch_sd[sel4]^2 / ms_w_4

obs_sigma_sd <- sd(c(tilde_s2_1, tilde_s2_2, tilde_s2_3, tilde_s2_4))
obs_maxmin <- c(max(c(tilde_s2_1, tilde_s2_2, tilde_s2_3, tilde_s2_4)) -
                  min(c(tilde_s2_1, tilde_s2_2, tilde_s2_3, tilde_s2_4)))

obs_sigma_sd_study1 <- sd(tilde_s2_1)
obs_sigma_sd_study2 <- sd(tilde_s2_2)
obs_sigma_sd_study3 <- sd(tilde_s2_3)
obs_sigma_sd_study4 <- sd(tilde_s2_4)

obs_maxmin_study1 <- max(tilde_s2_1) - min(tilde_s2_1)
obs_maxmin_study2 <- max(tilde_s2_2) - min(tilde_s2_2)
obs_maxmin_study3 <- max(tilde_s2_3) - min (tilde_s2_3)
obs_maxmin_study4 <- max(tilde_s2_4) - min (tilde_s2_4)

s2 <- dat$anch_sd[dat$referrer == pp]^2
n_min_1 <- dat$anch_n[dat$referrer == pp] - 1
sim_sigma <- NULL

for (z in 1:16)
{
  # Made a small error in the preregistration, so this now deviates.
  sim_sigma <- cbind(sim_sigma, rchisq(n = iter, df = n_min_1[z]) / n_min_1[z])
}

# Standardize the generated SDs based on the msW from the simulated values
# Per study
ms_w_1 <- apply(sim_sigma[,1:4], 1, function(x) sum(n_min_1[1:4] * x) / sum(n_min_1[1:4]))
ms_w_2 <- apply(sim_sigma[,5:8], 1, function(x) sum(n_min_1[5:8] * x) / sum(n_min_1[5:8]))
ms_w_3 <- apply(sim_sigma[,9:12], 1, function(x) sum(n_min_1[9:12] * x) / sum(n_min_1[9:12]))
ms_w_4 <- apply(sim_sigma[,13:16], 1, function(x) sum(n_min_1[13:16] * x) / sum(n_min_1[13:16]))

sim_sigma[,1:4] <- sim_sigma[,1:4] / ms_w_1
sim_sigma[,5:8] <- sim_sigma[,5:8] / ms_w_2
sim_sigma[,9:12] <- sim_sigma[,9:12] / ms_w_3
sim_sigma[,13:16] <- sim_sigma[,13:16] / ms_w_4

# Calculate the variance of the simulated sds
sim_sigma_sd_overall <- apply(sim_sigma, 1, sd)
sim_sigma_sd_study1 <- apply(sim_sigma[,1:4], 1, sd)
sim_sigma_sd_study2 <- apply(sim_sigma[,5:8], 1, sd)
sim_sigma_sd_study3 <- apply(sim_sigma[,9:12], 1, sd)
sim_sigma_sd_study4 <- apply(sim_sigma[,13:16], 1, sd)

# Calculate the range of the simulated sds
sim_maxmin_overall <- apply(sim_sigma, 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study1 <- apply(sim_sigma[,1:4], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study2 <- apply(sim_sigma[,5:8], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study3 <- apply(sim_sigma[,9:12], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study4 <- apply(sim_sigma[,13:16], 1, FUN = function(x)
{
  max(x) - min(x)
})

# Calculate the p-value overall and per study
dat$variance_sd_p_overall_homo[sel] <- sum(sim_sigma_sd_overall < obs_sigma_sd) / iter
dat$variance_sd_p_study1[sel] <- sum(sim_sigma_sd_study1 < obs_sigma_sd_study1) / iter
dat$variance_sd_p_study2[sel] <- sum(sim_sigma_sd_study2 < obs_sigma_sd_study2) / iter
dat$variance_sd_p_study3[sel] <- sum(sim_sigma_sd_study3 < obs_sigma_sd_study3) / iter
dat$variance_sd_p_study4[sel] <- sum(sim_sigma_sd_study4 < obs_sigma_sd_study4) / iter

dat$maxmin_p_overall_homo[sel] <- sum(sim_maxmin_overall < obs_maxmin) / iter
dat$maxmin_p_study1[sel] <- sum(sim_maxmin_study1 < obs_maxmin_study1) / iter
dat$maxmin_p_study2[sel] <- sum(sim_maxmin_study2 < obs_maxmin_study2) / iter
dat$maxmin_p_study3[sel] <- sum(sim_maxmin_study3 < obs_maxmin_study3) / iter
dat$maxmin_p_study4[sel] <- sum(sim_maxmin_study4 < obs_maxmin_study4) / iter

# ---
# Assuming heterogeneity of variances for each ANCHORING condition
# Let's call this hetero
# ---
# Standardizing the sds
# Study 1
sel1_low <- dat$referrer == pp & dat$study == 1 & dat$anchoring == 0
sel1_high <- dat$referrer == pp & dat$study == 1 & dat$anchoring == 1
ms_w_1_low <- sum((dat$anch_n[sel1_low] - 1) * dat$anch_sd[sel1_low]^2) / sum((dat$anch_n[sel1_low] - 1))
tilde_s2_1_low <- dat$anch_sd[sel1_low]^2 / ms_w_1_low
ms_w_1_high <- sum((dat$anch_n[sel1_high] - 1) * dat$anch_sd[sel1_high]^2) / sum((dat$anch_n[sel1_high] - 1))
tilde_s2_1_high <- dat$anch_sd[sel1_high]^2 / ms_w_1_high

# Study 2
sel2_low <- dat$referrer == pp & dat$study == 2 & dat$anchoring == 0
sel2_high <- dat$referrer == pp & dat$study == 2 & dat$anchoring == 1
ms_w_2_low <- sum((dat$anch_n[sel2_low] - 1) * dat$anch_sd[sel2_low]^2) / sum((dat$anch_n[sel2_low] - 1))
tilde_s2_2_low <- dat$anch_sd[sel2_low]^2 / ms_w_2_low
ms_w_2_high <- sum((dat$anch_n[sel2_high] - 1) * dat$anch_sd[sel2_high]^2) / sum((dat$anch_n[sel2_high] - 1))
tilde_s2_2_high <- dat$anch_sd[sel2_high]^2 / ms_w_2_high

# Study 3
sel3_low <- dat$referrer == pp & dat$study == 3 & dat$anchoring == 0
sel3_high <- dat$referrer == pp & dat$study == 3 & dat$anchoring == 1
ms_w_3_low <- sum((dat$anch_n[sel3_low] - 1) * dat$anch_sd[sel3_low]^2) / sum((dat$anch_n[sel3_low] - 1))
tilde_s2_3_low <- dat$anch_sd[sel3_low]^2 / ms_w_3_low
ms_w_3_high <- sum((dat$anch_n[sel3_high] - 1) * dat$anch_sd[sel3_high]^2) / sum((dat$anch_n[sel3_high] - 1))
tilde_s2_3_high <- dat$anch_sd[sel3_high]^2 / ms_w_3_high

# Study 4
sel4_low <- dat$referrer == pp & dat$study == 4 & dat$anchoring == 0
sel4_high <- dat$referrer == pp & dat$study == 4 & dat$anchoring == 1
ms_w_4_low <- sum((dat$anch_n[sel4_low] - 1) * dat$anch_sd[sel4_low]^2) / sum((dat$anch_n[sel4_low] - 1))
tilde_s2_4_low <- dat$anch_sd[sel4_low]^2 / ms_w_4_low
ms_w_4_high <- sum((dat$anch_n[sel4_high] - 1) * dat$anch_sd[sel4_high]^2) / sum((dat$anch_n[sel4_high] - 1))
tilde_s2_4_high <- dat$anch_sd[sel4_high]^2 / ms_w_4_high

obs_sigma_sd <- sd(c(tilde_s2_1_low,
                     tilde_s2_1_high,
                     tilde_s2_2_low,
                     tilde_s2_2_high,
                     tilde_s2_3_low,
                     tilde_s2_3_high,
                     tilde_s2_4_low,
                     tilde_s2_4_high))

obs_maxmin <- max(c(tilde_s2_1_low,
                    tilde_s2_1_high,
                    tilde_s2_2_low,
                    tilde_s2_2_high,
                    tilde_s2_3_low,
                    tilde_s2_3_high,
                    tilde_s2_4_low,
                    tilde_s2_4_high)) - 
  min(c(tilde_s2_1_low,
        tilde_s2_1_high,
        tilde_s2_2_low,
        tilde_s2_2_high,
        tilde_s2_3_low,
        tilde_s2_3_high,
        tilde_s2_4_low,
        tilde_s2_4_high))

obs_sigma_sd_study1_low <- sd(tilde_s2_1_low)
obs_sigma_sd_study1_high <- sd(tilde_s2_1_high)
obs_sigma_sd_study2_low <- sd(tilde_s2_2_low)
obs_sigma_sd_study2_high <- sd(tilde_s2_2_high)
obs_sigma_sd_study3_low <- sd(tilde_s2_3_low)
obs_sigma_sd_study3_high <- sd(tilde_s2_3_high)
obs_sigma_sd_study4_low <- sd(tilde_s2_4_low)
obs_sigma_sd_study4_high <- sd(tilde_s2_4_high)

obs_maxmin_study1_low <- max(tilde_s2_1_low) - min(tilde_s2_1_low)
obs_maxmin_study1_high <- max(tilde_s2_1_high) - min(tilde_s2_1_high)
obs_maxmin_study2_low <- max(tilde_s2_2_low) - min(tilde_s2_2_low)
obs_maxmin_study2_high <- max(tilde_s2_2_high) - min(tilde_s2_2_high)
obs_maxmin_study3_low <- max(tilde_s2_3_low) - min(tilde_s2_3_low)
obs_maxmin_study3_high <- max(tilde_s2_3_high) - min(tilde_s2_3_high)
obs_maxmin_study4_low <- max(tilde_s2_4_low) - min(tilde_s2_4_low)
obs_maxmin_study4_high <- max(tilde_s2_4_high) - min(tilde_s2_4_high)


# Standardize the generated SDs based on the msW from the simulated values
# Per study
ms_w_1_low <- apply(sim_sigma[,1:2], 1, function(x) sum(n_min_1[1:2] * x) / sum(n_min_1[1:2]))
ms_w_1_high <- apply(sim_sigma[,3:4], 1, function(x) sum(n_min_1[3:4] * x) / sum(n_min_1[3:4]))
ms_w_2_low <- apply(sim_sigma[,5:6], 1, function(x) sum(n_min_1[5:6] * x) / sum(n_min_1[5:6]))
ms_w_2_high <- apply(sim_sigma[,7:8], 1, function(x) sum(n_min_1[7:8] * x) / sum(n_min_1[7:8]))
ms_w_3_low <- apply(sim_sigma[,9:10], 1, function(x) sum(n_min_1[9:10] * x) / sum(n_min_1[9:10]))
ms_w_3_high <- apply(sim_sigma[,11:12], 1, function(x) sum(n_min_1[11:12] * x) / sum(n_min_1[11:12]))
ms_w_4_low <- apply(sim_sigma[,13:14], 1, function(x) sum(n_min_1[13:14] * x) / sum(n_min_1[13:14]))
ms_w_4_high <- apply(sim_sigma[,15:16], 1, function(x) sum(n_min_1[15:16] * x) / sum(n_min_1[15:16]))

sim_sigma[,1:2] <- sim_sigma[,1:2] / ms_w_1_low
sim_sigma[,3:4] <- sim_sigma[,3:4] / ms_w_1_high
sim_sigma[,5:6] <- sim_sigma[,5:6] / ms_w_2_low
sim_sigma[,7:8] <- sim_sigma[,7:8] / ms_w_2_high
sim_sigma[,9:10] <- sim_sigma[,9:10] / ms_w_3_low
sim_sigma[,11:12] <- sim_sigma[,11:12] / ms_w_3_high
sim_sigma[,13:14] <- sim_sigma[,13:14] / ms_w_4_low
sim_sigma[,15:16] <- sim_sigma[,15:16] / ms_w_4_high

# Calculate the variance of the simulated sds
sim_sigma_sd_overall <- apply(sim_sigma, 1, sd)
sim_sigma_sd_study1_low <- apply(sim_sigma[,1:2], 1, sd)
sim_sigma_sd_study1_high <- apply(sim_sigma[,3:4], 1, sd)
sim_sigma_sd_study2_low <- apply(sim_sigma[,5:6], 1, sd)
sim_sigma_sd_study2_high <- apply(sim_sigma[,7:8], 1, sd)
sim_sigma_sd_study3_low <- apply(sim_sigma[,9:10], 1, sd)
sim_sigma_sd_study3_high <- apply(sim_sigma[,11:12], 1, sd)
sim_sigma_sd_study4_low <- apply(sim_sigma[,13:14], 1, sd)
sim_sigma_sd_study4_high <- apply(sim_sigma[,15:16], 1, sd)

# Calculate the range of the simulated sds
sim_maxmin_overall <- apply(sim_sigma, 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study1_low <- apply(sim_sigma[,1:2], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study1_high <- apply(sim_sigma[,3:4], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study2_low <- apply(sim_sigma[,5:6], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study2_high <- apply(sim_sigma[,7:8], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study3_low <- apply(sim_sigma[,9:10], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study3_high <- apply(sim_sigma[,11:12], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study4_low <- apply(sim_sigma[,13:14], 1, FUN = function(x)
{
  max(x) - min(x)
})
sim_maxmin_study4_high <- apply(sim_sigma[,15:16], 1, FUN = function(x)
{
  max(x) - min(x)
})

# Calculate the p-value overall and per study
dat$variance_sd_p_overall_hetero[sel] <- sum(sim_sigma_sd_overall < obs_sigma_sd) / iter
dat$variance_sd_p_study1_low[sel] <- sum(sim_sigma_sd_study1_low < obs_sigma_sd_study1_low) / iter
dat$variance_sd_p_study1_high[sel] <- sum(sim_sigma_sd_study1_high < obs_sigma_sd_study1_high) / iter
dat$variance_sd_p_study2_low[sel] <- sum(sim_sigma_sd_study2_low < obs_sigma_sd_study2_low) / iter
dat$variance_sd_p_study2_high[sel] <- sum(sim_sigma_sd_study2_high < obs_sigma_sd_study2_high) / iter
dat$variance_sd_p_study3_low[sel] <- sum(sim_sigma_sd_study3_low < obs_sigma_sd_study3_low) / iter
dat$variance_sd_p_study3_high[sel] <- sum(sim_sigma_sd_study3_high < obs_sigma_sd_study3_high) / iter
dat$variance_sd_p_study4_low[sel] <- sum(sim_sigma_sd_study4_low < obs_sigma_sd_study4_low) / iter
dat$variance_sd_p_study4_high[sel] <- sum(sim_sigma_sd_study4_high < obs_sigma_sd_study4_high) / iter  

dat$maxmin_p_overall_hetero[sel] <- sum(sim_maxmin_overall < obs_maxmin) / iter
dat$maxmin_p_study1_low[sel] <- sum(sim_maxmin_study1_low < obs_maxmin_study1_low) / iter
dat$maxmin_p_study1_high[sel] <- sum(sim_maxmin_study1_high < obs_maxmin_study1_high) / iter
dat$maxmin_p_study2_low[sel] <- sum(sim_maxmin_study2_low < obs_maxmin_study2_low) / iter
dat$maxmin_p_study2_high[sel] <- sum(sim_maxmin_study2_high < obs_maxmin_study2_high) / iter
dat$maxmin_p_study3_low[sel] <- sum(sim_maxmin_study3_low < obs_maxmin_study3_low) / iter
dat$maxmin_p_study3_high[sel] <- sum(sim_maxmin_study3_high < obs_maxmin_study3_high) / iter
dat$maxmin_p_study4_low[sel] <- sum(sim_maxmin_study4_low < obs_maxmin_study4_low) / iter
dat$maxmin_p_study4_high[sel] <- sum(sim_maxmin_study4_high < obs_maxmin_study4_high) / iter

# Compute the Fisher method p-values 
# for gender and interaction

# Safety check to make sure there are 4 unique values
# Iteratively made exceptions for the cases that do not
if (length(unique(dat$p_gender[sel])) < 4 | 
    # length(unique(dat$p_condition[sel])) < 4 |
    length(unique(dat$p_interaction[sel])) < 4)
{
  if (pp == 'R_6opw4qQURhqxT3D')
  {
    p_gender <- rep(1, 4)
  } else
  {
    stop(sprintf('Something wrong with the number of p-values available for %s',
                 pp))
  }
} else 
{
  p_gender <- unique(dat$p_gender[sel])  
}

dat$gender_fish[sel] <- -2 * sum(log(1 - ((p_gender[p_gender > .05] - .05) / (1 - .05))))
dat$gender_fish_df[sel] <- 2 * length(p_gender[p_gender > .05])
dat$gender_fish_p[sel] <- pchisq(-2 * sum(log(1 - ((p_gender[p_gender > .05] - .05) / (1 - .05)))),
                                 df = 2 * length(p_gender[p_gender > .05]),
                                 lower.tail = FALSE)  

p_interaction <- unique(dat$p_interaction[sel])
dat$interaction_fish[sel] <- -2 * sum(log(1 - ((p_interaction[p_interaction > .05] - .05) / (1 - .05))))
dat$interaction_fish_df[sel] <- 2 * length(p_interaction[p_interaction > .05])
dat$interaction_fish_p[sel] <- pchisq(-2 * sum(log(1 - ((p_interaction[p_interaction > .05] - .05) / (1 - .05)))),
                                      df = 2 * length(p_interaction[p_interaction > .05]),
                                      lower.tail = FALSE)

# correct outcomes for those participants 
# who fabricated significant gender effects
if (pp == 'R_9XhjfXDjcchvSiF' | pp == 'R_3JEilrqcgWYTOkd')
{
  dat$gender_fish[sel] <- NA
  dat$gender_fish_df[sel] <- NA
  dat$gender_fish_p[sel] <- NA
  
  dat$interaction_fish[sel] <- NA
  dat$interaction_fish_df[sel] <- NA
  dat$interaction_fish_p[sel] <- NA
  
} 
}

dat_summary <- dat[ , -which(names(dat) %in% c("study",
                                               "partgender",
                                               "anchoring",
                                               "anch_n",
                                               "anch_m",
                                               "anch_sd",
                                               "f_gender",
                                               "df1_gender",
                                               "df2_gender",
                                               "p_gender",
                                               "f_condition",
                                               "df1_condition",
                                               "df2_condition",
                                               "p_condition",
                                               "f_interaction",
                                               "df1_interaction",
                                               "df2_interaction",
                                               "p_interaction"))]
dat_summary <- dat_summary[!duplicated(dat_summary$referrer), ]
dat_summary$fish_combine_3_homo <- -2 * (log(dat_summary$variance_sd_p_overall_homo) +
                                           log(dat_summary$gender_fish_p) + 
                                           log(dat_summary$interaction_fish_p))
dat_summary$fish_combine_3_homo_p <- pchisq(q = dat_summary$fish_combine_3_homo,
                                            df = 2 * 3, lower.tail = FALSE)
dat_summary$fish_combine_3_hetero <- -2 * (log(dat_summary$variance_sd_p_overall_hetero) +
                                             log(dat_summary$gender_fish_p) + 
                                             log(dat_summary$interaction_fish_p))
dat_summary$fish_combine_3_hetero_p <- pchisq(q = dat_summary$fish_combine_3_hetero,
                                              df = 2 * 3, lower.tail = FALSE)
dat_summary$fish_combine_6_homo <- -2 * (log(dat_summary$variance_sd_p_study1) +
                                           log(dat_summary$variance_sd_p_study2) + 
                                           log(dat_summary$variance_sd_p_study3) + 
                                           log(dat_summary$variance_sd_p_study4) +
                                           log(dat_summary$gender_fish_p) + 
                                           log(dat_summary$interaction_fish_p))
dat_summary$fish_combine_6_homo_p <- pchisq(q = dat_summary$fish_combine_6_homo,
                                            df = 2 * 6, lower.tail = FALSE)
dat_summary$fish_combine_6_hetero <- -2 * (log(dat_summary$variance_sd_p_study1) +
                                             log(dat_summary$variance_sd_p_study2) + 
                                             log(dat_summary$variance_sd_p_study3) + 
                                             log(dat_summary$variance_sd_p_study4) +
                                             log(dat_summary$gender_fish_p) + 
                                             log(dat_summary$interaction_fish_p))
dat_summary$fish_combine_6_hetero_p <- pchisq(q = dat_summary$fish_combine_6_hetero,
                                              df = 2 * 6, lower.tail = FALSE)

write.csv(dat_summary,
          dat_summary_file, row.names = FALSE)