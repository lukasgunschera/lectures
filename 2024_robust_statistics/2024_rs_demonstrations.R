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
nexp <- 10000      # number of experiments
nsample <- 71      # sample size of both groups
m1 <- 100          # mean of group 1
m2 <- 105          # mean of group 2
sd1 <- 15          # standard deviation of group 1
sd2 <- 15          # standard deviation of group 2

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
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = "#2E2E2E", alpha = 0.7, aes(fill = pvals < 0.05)) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 5000, color = "red", linetype = "dashed", size = .25) +
  annotate(geom = "segment", x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 100000, colour = "#2E2E2E", lwd = 0.75)

print(gg_p1_distr)

ggsave(gg_p1_distr,
  path = here::here("2024_robust_statistics", "images"), filename = "rs_pvalue_p1_distr.png",
  dpi = 1200, device = "png"
)

## Effect 2 -------------------------------------------------------------------------------------------------------------- ##
# increase sample size
nsample <- 127      # sample size of both groups

p2 <- numeric(nexp) # store all simulated *p*-values

for (i in 1:nexp) { # for each simulated experiment
  x2 <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  y2 <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  p2[i] <- t.test(x2, y2)$p.value # store the *p*-value
}

# computer power (probability of rejecting null when it is false)
round((sum(p2 < 0.05) / nexp), 2)

gg_p2_distr <- p2 %>%
  data.frame(pvals = p2) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = "#2E2E2E", alpha = 0.7, aes(fill = pvals < 0.05)) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 5000, color = "red", linetype = "dashed", size = .25) +
  annotate(geom = "segment", x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 100000, colour = "#2E2E2E", lwd = 0.75)

print(gg_p2_distr)

ggsave(gg_p2_distr,
  path = here::here("2024_robust_statistics", "images"), filename = "rs_pvalue_p2_distr.png",
  dpi = 1200, device = "png"
)

## Effect 3 -------------------------------------------------------------------------------------------------------------- ##
# change group means to be equivalent
m2 <- 100      # mean of group 2

pnull <- numeric(nexp) # store all simulated *p*-values

for (i in 1:nexp) { # for each simulated experiment
  xnull <- rnorm(n = nsample, mean = m1, sd = sd1) # Simulate data
  ynull <- rnorm(n = nsample, mean = m2, sd = sd2) # Simulate data
  pnull[i] <- t.test(xnull, ynull)$p.value # store the *p*-value
}

gg_pnull_distr <- pnull %>%
  data.frame(pvals = pnull) %>%
  ggplot(., aes(x = pvals)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = "#2E2E2E", alpha = 0.7, aes(fill = pvals < 0.05)) +
  scale_y_continuous(limits = c(0, nexp)) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  labs(x = "p-value", y = "Number of p-values") +
  plot_theme +
  aspect_ratio_balanced +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 50, color = "red", linetype = "dashed", size = .25) +
  annotate(geom = "segment", x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 100000, colour = "#2E2E2E", lwd = 0.75)

# computer power (probability of rejecting null when it is false)
round((sum(pnull < 0.05) / 100000), 2)

print(gg_pnull_distr)

ggsave(gg_pnull_distr,
  path = here::here("2024_os_course", "images"), filename = "os_pvalue_pnull_distr.png",
  dpi = 1200, device = "png"
)

## ======================================================================================================================= ##
## BAYESIAN UPDATING VISUALISATION
## ======================================================================================================================= ##
# credit to Alex Etz (https://alexanderetz.com/2015/07/25/understanding-bayes-updating-priors-via-the-likelihood/)

# create binary data of 'making threes at a basketball game'
shotData <- c(1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0,
              1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1,
              0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0)

x = seq(.001, .999, .001) ##Set up for creating the distributions
y2 = dbeta(x, 1 + 58, 1 + 42) # data for likelihood curve, plotted as the posterior from a be

# probability of success (three pointer)
sum(shotData == 1) / sum(shotData == 0) + sum(shotData == 1)

# show likelihood curve for above data
plot(seq(.001, .999, .001), dbeta(x, 1 + 58, 1 + 42),
  xlim = c(0, 1), ylim = c(0, 1.25 * max(dbeta(x, 1 + 58, 1 + 42), 1.6)), type = "l", ylab = "Density", lty = 3,
  xlab = "Probability of success", las = 1, main = "Likelihood Curve for 3-pt Shots", sub = "(Binomial Data, 58/100)", lwd = 2,
  cex.lab = 1.5, cex.main = 1.5, col = "darkorange", axes = FALSE
)
axis(1, at = seq(0, 1, .2)) # adds custom x axis
axis(2, las = 1) # custom y axis

## Plot function -------------------------------------------------------------------------------------------------------------- ##
# @ PS = prior successes (input to beta distribution)
# @ PF = prior failures (input to beta distribution)
# @ k = number of observed successes in the data
# @ null = point null hypothesis (default = NULL)
# @ CI = credibility interval (.95 default)

## Uniform Prior --------------------------------------------------------------------------------------------------------- ##
plot.beta(1, 1, ymax = 3.2, main = "Uniform Prior, Beta(1,1)")

# miss or make next one?
#plot.beta(1, 1, 1, 1, ymax = 3.2, main = "Uniform Prior, Beta(1,1)")
#plot.beta(1, 1, 0, 1, ymax = 3.2, main = "Uniform Prior, Beta(1,1)")


# observe 13 threes and 12 fails
plot.beta(1, 1, 1, 1, main = "Beta(1,1) to Beta(14,13)", ymax = 10)

# observe another 12 threes and 13 fails
plot.beta(14, 13, 12, 25, ymax = 10, main = "Beta(14,13) to Beta(26,26)")

# observe another 23 threes and 17 fails
plot.beta(40, 37, 19, 25, ymax = 10, main = "Beta(40,37) to Beta(59,43)")
plot.beta(1, 1, 58, 100, ymax = 10, main = "Beta(1,1) to Beta(59,43)") # after 100 throws

## Informed/biased Prior ------------------------------------------------------------------------------------------------- ##
plot.beta(4, 9, ymax = 3.2, main = "Informed Prior, Beta(4,9)")

# miss or make next one?
plot.beta(4, 9, 1, 1, ymax = 3.2, main = "Informed Prior, Beta(4,9)")



plot.beta(4, 9, 13, 25, main = "Beta(4,9) to Beta(17,21)", ymax = 10)
plot.beta(17, 21, 12, 25, ymax = 10, main = "Beta(17,21) to Beta(29,34)")
plot.beta(29, 34, 14, 25, ymax = 10, main = "Beta(29,34) to Beta(43,45)")
plot.beta(43, 45, 19, 25, ymax = 10, main = "Beta(43,45) to Beta(62,51)")
plot.beta(4, 9, 58, 100, ymax = 10, main = "Beta(4,9) to Beta(62,51)") # after 100 throws


