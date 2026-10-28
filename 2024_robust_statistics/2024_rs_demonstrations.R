## ======================================================================================================================= ##
## Script:       INTERACTIVE CODE COURSE COMPONENTS FOR 2024 ROBUST STATISTICS
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2024-10-17
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

rm(list = ls())

library(here)
source(here::here("2024_robust_statistics", "2024_rs_dependencies.R"))

##======================================================================================================================== ##
## P-VALUE DISTRIBUTION
## ======================================================================================================================= ##

## Effect 1 -------------------------------------------------------------------------------------------------------------- ##
# set parameters for simulated experiments
nexp <- 10000 # number of experiments
nsample <- 71 # sample size of both groups
m1 <- 100 # mean of group 1
m2 <- 105 # mean of group 2
sd1 <- 15 # standard deviation of group 1
sd2 <- 15 # standard deviation of group 2

# set empty vector for storing experiment results
p1 <- numeric(10000)

for (i in 1:nexp) {
  x1 <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  y1 <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  p1[i] <- t.test(x1, y1)$p.value # store the *p*-value
}

# computer power (probability of rejecting null when it is false)
(sum(p1 < 0.05) / nexp)

gg_p1_distr <- p1 %>%
  data.frame(pvals = p1) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(
    breaks = seq(0, 1, by = 0.05),
    color = "#2E2E2E",
    alpha = 0.7,
    aes(fill = pvals < 0.05)
  ) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(
    yintercept = 5000,
    color = "red",
    linetype = "dashed",
    size = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    lwd = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = 100000,
    colour = "#2E2E2E",
    lwd = 0.75
  )

print(gg_p1_distr)

ggsave(
  gg_p1_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p1_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 2 -------------------------------------------------------------------------------------------------------------- ##
# increase sample size
nsample <- nsample + 56 # increase sample size of both groups

p2 <- numeric(nexp) # store all simulated *p*-values

for (i in 1:nexp) {
  # for each simulated experiment
  x2 <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  y2 <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  p2[i] <- t.test(x2, y2)$p.value # store the *p*-value
}

# computer power (probability of rejecting null when it is false)
round((sum(p2 < 0.05) / nexp), 2)

gg_p2_distr <- p2 %>%
  data.frame(pvals = p2) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(
    breaks = seq(0, 1, by = 0.05),
    color = "#2E2E2E",
    alpha = 0.7,
    aes(fill = pvals < 0.05)
  ) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(
    yintercept = 5000,
    color = "red",
    linetype = "dashed",
    size = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    lwd = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = 100000,
    colour = "#2E2E2E",
    lwd = 0.75
  )

print(gg_p2_distr)

ggsave(
  gg_p2_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p2_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 3 -------------------------------------------------------------------------------------------------------------- ##
# decrease standard deviation
sd1 <- sd1 - 5 # standard deviation of group 1
sd2 <- sd2 - 5 # standard deviation of group 2

p2 <- numeric(nexp) # store all simulated *p*-values

for (i in 1:nexp) {
  # for each simulated experiment
  x2 <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  y2 <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  p2[i] <- t.test(x2, y2)$p.value # store the *p*-value
}

# computer power (probability of rejecting null when it is false)
round((sum(p2 < 0.05) / nexp), 2)

gg_p2_distr <- p2 %>%
  data.frame(pvals = p2) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(
    breaks = seq(0, 1, by = 0.05),
    color = "#2E2E2E",
    alpha = 0.7,
    aes(fill = pvals < 0.05)
  ) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(
    yintercept = 5000,
    color = "red",
    linetype = "dashed",
    size = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    lwd = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = 100000,
    colour = "#2E2E2E",
    lwd = 0.75
  )

print(gg_p2_distr)

ggsave(
  gg_p2_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p2_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 4 -------------------------------------------------------------------------------------------------------------- ##
# change group means to be equivalent
m2 <- 100 # mean of group 2
sd1 <- 15 # standard deviation of group 1
sd2 <- 15 # standard deviation of group 2

pnull <- numeric(nexp) # store all simulated *p*-values

for (i in 1:nexp) {
  # for each simulated experiment
  xnull <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  ynull <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  pnull[i] <- t.test(xnull, ynull)$p.value # store the *p*-value
}

gg_pnull_distr <- pnull %>%
  data.frame(pvals = pnull) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(
    breaks = seq(0, 1, by = 0.05),
    color = "#2E2E2E",
    alpha = 0.7,
    aes(fill = pvals < 0.05)
  ) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", size = .25) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    lwd = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = 100000,
    colour = "#2E2E2E",
    lwd = 0.75
  )

# computer type 1 error rate (probability of rejecting null when it is true)
round((sum(pnull < 0.05) / 100000), 2) # false positive (type 1 error)

print(gg_pnull_distr)

ggsave(
  gg_pnull_distr,
  path = here::here("2024_os_course", "images"),
  filename = "os_pvalue_pnull_distr.png",
  dpi = 1200,
  device = "png"
)

## ======================================================================================================================= ##
## BAYESIAN UPDATING VISUALISATION
## ======================================================================================================================= ##
# credit to Alex Etz (https://alexanderetz.com/2015/07/25/understanding-bayes-updating-priors-via-the-likelihood/)

# The question: what is a basketball player's true probability (theta) of making a three-pointer?
#
# We answer it in three ingredients:
#   1. PRIOR       - what we believe about theta *before* seeing any shots
#   2. LIKELIHOOD  - how well each possible value of theta explains the shots we actually observed
#   3. POSTERIOR   - our updated belief: prior x likelihood (rescaled so it integrates to 1)
#
# Why the Beta distribution? Each shot is a make (1) or miss (0), so the data are binomial. A Beta prior combined
# with binomial data always gives a Beta posterior (the Beta is the "conjugate prior" for the binomial).

#   - Beta(1, 1) = 0 makes, 0 misses   -> flat: every theta between 0 and 1 is equally plausible
#   - Beta(4, 9) = 3 makes, 8 misses

## Data ------------------------------------------------------------------------------------------------------------------ ##
# 100 three-point attempts: 1 = make, 0 = miss.
# Laid out as four rows of 25 shots, because below we update our beliefs one batch of 25 at a time.
shotData <- c(
  1,
  0,
  0,
  0,
  1,
  0,
  1,
  0,
  1,
  0,
  0,
  0,
  1,
  1,
  1,
  0,
  1,
  0,
  1,
  1,
  1,
  0,
  0,
  1,
  1,
  0,
  0,
  1,
  1,
  1,
  0,
  0,
  1,
  1,
  0,
  0,
  0,
  1,
  1,
  1,
  0,
  0,
  0,
  0,
  1,
  1,
  0,
  1,
  1,
  0,
  1,
  0,
  0,
  1,
  0,
  1,
  0,
  1,
  1,
  1,
  0,
  1,
  0,
  1,
  0,
  0,
  1,
  1,
  0,
  1,
  0,
  0,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  1,
  0,
  0,
  1,
  1,
  0,
  0,
  1,
  1,
  1,
  0,
  1,
  1,
  1,
  1,
  1,
  0
)

# Overall: 58 makes and 42 misses
sum(shotData) # 58 makes
length(shotData) # 100 shots

# Observed proportion of makes = 58 / 100 = .58
# (this is the maximum-likelihood estimate: the single value of theta under which the data are most probable)
mean(shotData)

## Likelihood curve ------------------------------------------------------------------------------------------------------ ##
# Grid of candidate values for theta.
x <- seq(.001, .999, .001)

# Likelihood of 58 makes and 42 misses at each candidate theta. The likelihood has exactly the shape of a
# Beta(1 + 58, 1 + 42) density.

y2 <- dbeta(x, 1 + 58, 1 + 42)

# probability of success (three pointer)
sum(shotData == 1) / sum(shotData == 0) + sum(shotData == 1)

plot(
  x,
  y2,
  type = "l",
  lty = 3,
  lwd = 2,
  col = "darkorange", # dotted orange line
  xlim = c(0, 1),
  ylim = c(0, 1.25 * max(y2, 1.6)), # 25% headroom above the peak
  xlab = "Probability of success",
  ylab = "Density",
  main = "Likelihood Curve for 3-pt Shots",
  sub = "(Binomial Data, 58/100)",
  las = 1,
  cex.lab = 1.5,
  cex.main = 1.5,
  axes = FALSE # axes drawn manually below
)
axis(1, at = seq(0, 1, .2)) # x axis with ticks at 0, .2, .4, ... 1
axis(2, las = 1) # y axis with horizontal labels
# The peak sits at .58 (the observed proportion); the width shows how uncertain 100 shots leave us.

## Plot function --------------------------------------------------------------------------------------------------------- ##
# @ PS = prior successes (input to beta distribution)
# @ PF = prior failures (input to beta distribution)
# @ k = number of observed successes in the data
# @ null = point null hypothesis (default = NULL)
# @ CI = credibility interval (.95 default)

## Updating from a uniform (flat) prior ---------------------------------------------------------------------------------- ##
# Start knowing nothing: every success rate from 0 to 1 is equally plausible.
plot.beta(1, 1, ymax = 3.2, main = "Uniform Prior, Beta(1,1)")

# Now feed in the data 25 shots at a time. Each posterior becomes the prior for the next batch.

# Shots 1-25: 13 makes, 12 misses -> Beta(1 + 13, 1 + 12)
plot.beta(1, 1, 13, 25, ymax = 10, main = "Beta(1,1) to Beta(14,13)")

# Shots 26-50: 12 makes, 13 misses -> Beta(14 + 12, 13 + 13)
plot.beta(14, 13, 12, 25, ymax = 10, main = "Beta(14,13) to Beta(26,26)")

# Shots 51-75: 14 makes, 11 misses -> Beta(26 + 14, 26 + 11)
plot.beta(26, 26, 14, 25, ymax = 10, main = "Beta(26,26) to Beta(40,37)")

# Shots 76-100: 19 makes, 6 misses -> Beta(40 + 19, 37 + 6)
plot.beta(40, 37, 19, 25, ymax = 10, main = "Beta(40,37) to Beta(59,43)")

# Key point: updating in batches gives the same answer as using all 100 shots at once (58 makes, 42 misses).
# The final posterior is identical to the likelihood curve above, because a flat prior adds no information.
plot.beta(1, 1, 58, 100, ymax = 10, main = "Beta(1,1) to Beta(59,43)")


## Updating from an informed (sceptical) prior --------------------------------------------------------------------------- ##
# Now start with a belief that the player is fairly poor (~31% success), held about as strongly as if we had
# already watched 11 shots (3 makes, 8 misses).
plot.beta(4, 9, ymax = 3.2, main = "Informed Prior, Beta(4,9)")

# One make -> Beta(5,9). The shift is smaller than under the flat prior, because the prior already carries weight.
plot.beta(4, 9, 1, 1, ymax = 3.2, main = "Informed Prior, Beta(4,9)")

# Same four batches of 25 shots as before:
plot.beta(4, 9, 13, 25, ymax = 10, main = "Beta(4,9) to Beta(17,21)") # shots   1-25: 13 makes
plot.beta(17, 21, 12, 25, ymax = 10, main = "Beta(17,21) to Beta(29,34)") # shots  26-50: 12 makes
plot.beta(29, 34, 14, 25, ymax = 10, main = "Beta(29,34) to Beta(43,45)") # shots  51-75: 14 makes
plot.beta(43, 45, 19, 25, ymax = 10, main = "Beta(43,45) to Beta(62,51)") # shots 76-100: 19 makes

# All 100 shots at once gives the same endpoint.
# Compare with the flat-prior result: Beta(62,51) has mean 62/113 = .55 vs .58 for Beta(59,43).
# With enough data the two posteriors converge - the data come to dominate the prior - but the sceptical
# prior still pulls the estimate slightly downwards after 100 shots.
plot.beta(4, 9, 58, 100, ymax = 10, main = "Beta(4,9) to Beta(62,51)")
