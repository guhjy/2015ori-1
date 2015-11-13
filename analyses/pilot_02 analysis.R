# Written by CHJ Hartgerink
# This is a down-and-dirty scripped and I have not double-checked these results
# Primarily wrote this to provide some feedback to pilot participants
# DO NOT USE ANY OF THIS SCRIPTING AS GOOD ANALYSES

# Read in data ------------------------------------------------------------

dat_raw <- read.table(file = 'data/pilot_02 cleaned raw data.csv', header = TRUE,
                      sep = ",", dec = '.')
dat_test <- read.table(file = 'data/pilot_02 cleaned test results.csv', header = TRUE,
                      sep = ",", dec = '.')

# Loop over each unique id ------------------------------------------------

for (i in 1:length(unique(dat_raw$id))){ # add {}
pval_test <- NULL

# Select data for the respondent ------------------------------------------

sel <- dat_raw$id == unique(dat_raw$id)[i] # change to i
dat_raw_sel <- dat_raw[sel, ]

# Terminal digit analyses -------------------------------------------------
temp <- strsplit(as.character(c(dat_raw_sel$mean_congruent,
                        dat_raw_sel$sd_congruent,
                        dat_raw_sel$mean_incongruent,
                        dat_raw_sel$sd_incongruent)), split = "")

prefinal <- NULL
final <- NULL
for (j in 1:length(temp)){
  prefinal[j] <- tail(temp[[j]], 2)[1]
  final[j] <- tail(temp[[j]], 1)
}

pval_test[1] <- pchisq(sum((table(prefinal) - 10)^2 / 10), df = 9)
pval_test[2] <- pchisq(sum((table(final) - 10)^2 / 10), df = 9)

# Higher dimensional correlations -----------------------------------------

pval_test[3] <- 1 - cor.test(dat_raw_sel$mean_congruent, dat_raw_sel$mean_incongruent)$p.value
pval_test[4] <- 1 - cor.test(dat_raw_sel$sd_congruent, dat_raw_sel$sd_incongruent)$p.value
pval_test[5] <- 1 - cor.test(dat_raw_sel$mean_congruent, dat_raw_sel$sd_congruent)$p.value
pval_test[6] <- 1 - cor.test(dat_raw_sel$mean_incongruent, dat_raw_sel$sd_incongruent)$p.value

cat(sprintf("ID = %s, p-value = %s\n", unique(dat_raw$id)[i], # change to i
              pchisq(-2 * sum(log(pval_test)), df = 12, lower.tail = FALSE)))
}