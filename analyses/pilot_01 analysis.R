# Read in data ------------------------------------------------------------
dat <- read.table('data/pilot_01 cleaned.csv', sep = ',', dec = '.', header = TRUE)

sel <- dat$id == unique(dat$id)[1]
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
pvals_test[1] <- pchisq(q = ln1minp, df = 2 * length(pvals_id), lower.tail = FALSE)

# Conduct Simonsohn method of variance of variances -----------------------
