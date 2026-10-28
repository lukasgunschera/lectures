## ======================================================================================================================= ##
## Script:       INTERACTIVE CODE COURSE COMPONENTS FOR 2026 ROBUST STATISTICS
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

# The idea: instead of running one experiment, we run the *same* experiment 10,000 times on simulated data and look at
# the p-values we get. This shows that a p-value is a random variable: its distribution depends on whether a true
# effect exists and, if it does, on how much power the study has to detect it.
#
# Each simulated experiment compares two independent groups with a t-test (e.g. an IQ-like score, mean 100, SD 15).
#
# Power depends on the standardised effect size, d = (mean difference) / SD, and on the sample size.
#   Effect 1: baseline                d = 5/15 = .33, n = 71 per group    -> power ~ .50
#   Effect 2: larger sample           d = 5/15 = .33, n = 127 per group   -> power ~ .75
#   Effect 3: less noise (smaller SD) d = 5/10 = .50, n = 127 per group   -> power ~ .98
#   Effect 4: no true effect          d = 0,          n = 127 per group   -> "power" = Type I error ~ .05
#

## Effect 1: a true effect with ~50% power ------------------------------------------------------------------------------- ##
# Simulation settings
nexp <- 10000 # number of simulated experiments
nsample <- 71 # participants per group
m1 <- 100 # population mean of group 1
m2 <- 105 # population mean of group 2 (true difference = 5 points)
sd1 <- 15 # population SD of group 1
sd2 <- 15 # population SD of group 2

# Reference line for the histograms: how many p-values each of the 20 bars would contain if the null were true
null_bin_height <- nexp / 20

# Empty vector to hold one p-value per simulated experiment
p1 <- numeric(nexp)

# Run the experiment nexp times. On each run we draw a fresh random sample for both groups from the populations
# defined above, run a t-test, and keep only its p-value.
for (i in 1:nexp) {
  x1 <- rnorm(n = nsample, mean = m1, sd = sd1) # sample for group 1
  y1 <- rnorm(n = nsample, mean = m2, sd = sd2) # sample for group 2
  p1[i] <- t.test(x1, y1)$p.value # Welch t-test (R's default); store the p-value
}

# Power = proportion of experiments with p < .05, i.e. how often we correctly reject the (false) null hypothesis.
# Expect ~.50: with this effect size and sample size, half of all studies would miss a real effect.
mean(p1 < 0.05)

# Histogram of the 10,000 p-values
gg_p1_distr <- data.frame(pvals = p1) %>%
  ggplot(aes(x = pvals)) +
  geom_histogram(
    breaks = seq(0, 1, by = 0.05), # 20 bins, each .05 wide, so the first bin is exactly p < .05
    color = "#2E2E2E",
    alpha = 0.7,
    aes(fill = pvals < 0.05) # colour significant vs non-significant p-values differently
  ) +
  scale_y_continuous(limits = c(0, nexp)) + # fixed y axis so the plots of all four effects are comparable
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  # Dashed line: expected bar height if there were no true effect (uniform p-values)
  geom_hline(
    yintercept = null_bin_height,
    color = "red",
    linetype = "dashed",
    linewidth = .25
  ) +
  # Draw the x and y axis lines manually (x axis from 0 to 1, y axis from 0 to nexp)
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    linewidth = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = nexp,
    colour = "#2E2E2E",
    linewidth = 0.75
  )

print(gg_p1_distr)

ggsave(
  gg_p1_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p1_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 2: same effect, larger sample ---------------------------------------------------------------------------------- ##
# Increase the sample size from 71 to 127 per group. The true difference (5) and the SD (15) are unchanged,
# so the effect size is the same (d = .33). More data means less sampling error, so p-values cluster closer to 0.
nsample <- nsample + 56 # 71 + 56 = 127 per group

p2 <- numeric(nexp)

for (i in 1:nexp) {
  x2 <- rnorm(n = nsample, mean = m1, sd = sd1)
  y2 <- rnorm(n = nsample, mean = m2, sd = sd2)
  p2[i] <- t.test(x2, y2)$p.value
}

# Power: expect ~.75, up from ~.50
round(mean(p2 < 0.05), 2)

# Same plot as Effect 1
gg_p2_distr <- data.frame(pvals = p2) %>%
  ggplot(aes(x = pvals)) +
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
    yintercept = null_bin_height,
    color = "red",
    linetype = "dashed",
    linewidth = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    linewidth = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = nexp,
    colour = "#2E2E2E",
    linewidth = 0.75
  )

print(gg_p2_distr)

ggsave(
  gg_p2_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p2_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 3: same difference, less noise --------------------------------------------------------------------------------- ##
# Reduce the SD of both groups from 15 to 10 (e.g. a more reliable measure), keeping n = 127 per group.
# The raw difference is still 5 points, but relative to the noise it is now larger: d = 5/10 = .50.
sd1 <- sd1 - 5 # 15 - 5 = 10
sd2 <- sd2 - 5 # 15 - 5 = 10

p3 <- numeric(nexp)

for (i in 1:nexp) {
  x3 <- rnorm(n = nsample, mean = m1, sd = sd1)
  y3 <- rnorm(n = nsample, mean = m2, sd = sd2)
  p3[i] <- t.test(x3, y3)$p.value
}

# Power: expect ~.98, nearly every experiment detects the effect
round(mean(p3 < 0.05), 2)

# Same plot as Effect 1
gg_p3_distr <- data.frame(pvals = p3) %>%
  ggplot(aes(x = pvals)) +
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
    yintercept = null_bin_height,
    color = "red",
    linetype = "dashed",
    linewidth = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    linewidth = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = nexp,
    colour = "#2E2E2E",
    linewidth = 0.75
  )

print(gg_p3_distr)

ggsave(
  gg_p3_distr,
  path = here::here("2024_robust_statistics", "images"),
  filename = "rs_pvalue_p3_distr.png",
  dpi = 1200,
  device = "png"
)

## Effect 4: no true effect (the null hypothesis is true) ---------------------------------------------------------------- ##
# Make both population means equal (100 vs 100) and restore SD = 15. Sample size stays at 127 per group.
# Any "significant" result now is a false positive.
m2 <- 100 # group 2 now has the same mean as group 1
sd1 <- 15
sd2 <- 15

pnull <- numeric(nexp)

for (i in 1:nexp) {
  xnull <- rnorm(n = nsample, mean = m1, sd = sd1)
  ynull <- rnorm(n = nsample, mean = m2, sd = sd2)
  pnull[i] <- t.test(xnull, ynull)$p.value
}

# Type I error rate = proportion of p < .05 when the null is true. Expect ~.05, i.e. alpha.
round(mean(pnull < 0.05), 2)

# Same plot as Effect 1. Here the bars should all sit roughly on the dashed line: p-values are uniformly distributed,
# so p = .01 is no more likely than p = .81.
gg_pnull_distr <- data.frame(pvals = pnull) %>%
  ggplot(aes(x = pvals)) +
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
    yintercept = null_bin_height,
    color = "red",
    linetype = "dashed",
    linewidth = .25
  ) +
  annotate(
    geom = "segment",
    x = 0,
    xend = 1,
    y = -Inf,
    yend = -Inf,
    colour = "#2E2E2E",
    linewidth = 0.75
  ) +
  annotate(
    geom = "segment",
    x = -Inf,
    xend = -Inf,
    y = 0,
    yend = nexp,
    colour = "#2E2E2E",
    linewidth = 0.75
  )

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
