## ============================================================================================================================ ##
## Script:    COURSE VISUALS
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Thu Oct 31 18:13:51 2024
## ============================================================================================================================ ##
##
## ============================================================================================================================ ##

rm(list = ls())

library(here)
library(HDInterval)
library(ggplot2)
library(viridis)
source(here::here("2024_robust_statistics", "2024_rs_dependencies.R"))



data_01 <- data.frame(
  participant = c("Instructors", "Scientists", "Students"),
  error = c(80.00, 89.70, 100.00)
)

## NHST Plot: Student and instructor error proportion of p-value definition --------------------------------------------------- ##
gg_stats_error <- data_01 %>%
  ggplot(aes(x = participant, y = error, fill = participant)) +
  geom_bar(stat = "identity", ) +
  geom_text(aes(label = paste0(error, "%")), vjust = -0.5) +
  ylab("Error (%)") +
  xlab("") +
  guides(fill = guide_legend(title = " ")) +
  scale_fill_viridis_d(direction = -1, begin = .05) +
  plot_theme +
  theme(aspect.ratio = 1.2 / 1) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  annotate(geom = "segment", x = 1, xend = 3, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 100, colour = "#2E2E2E", lwd = 0.75)

# save plot
ggsave(gg_stats_error,
       path = here::here("2024_robust_statistics", "images"), filename = "rs_pvalue_error.png",
       dpi = 1200, device = "png"
)

## HDI Plot ------------------------------------------------------------------------------------------------------------------- ##

# Step 1: Generate a posterior distribution sample
set.seed(42)  # for reproducibility
posterior_samples <- rnorm(10000, mean = 5, sd = 2)

# Step 2: Calculate the 95% HDI
hdi <- hdi(posterior_samples, credMass = 0.95)

# Step 3: Calculate density and prepare data for plotting
density_data <- density(posterior_samples, n = 1024)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Define cut-off points for the tails
lower_tail_cutoff <- quantile(posterior_samples, probs = 0.025)
upper_tail_cutoff <- quantile(posterior_samples, probs = 0.975)

# Mark regions for coloring: left tail, HDI region, and right tail
density_df$region <- with(density_df, ifelse(
  x <= lower_tail_cutoff, "Lower Tail",
  ifelse(x >= upper_tail_cutoff, "Upper Tail", "HDI")
))

# Step 4: Plot the posterior distribution with HDI and colorized tails
gg_bayes_hdi <- ggplot(density_df, aes(x = x, y = y, fill = region)) +
  geom_area(data = subset(density_df, region == "Lower Tail"), fill = viridis(5)[3], alpha = 0.8) +
  geom_area(data = subset(density_df, region == "HDI"), fill = viridis(1), alpha = 0.75) +
  geom_area(data = subset(density_df, region == "Upper Tail"), fill = viridis(5)[3], alpha = 0.8) +

  # Dashed lines for HDI boundaries
  geom_vline(xintercept = hdi, linetype = "dashed", color = "black", size = .5) +

  # Customize axis and labels
  labs(x = "Parameter value", y = "Posterior") +
  scale_x_continuous(limits = c(-5, 15)) +

  plot_theme +
  theme(aspect.ratio = 1 / 2) +

  # Custom annotations for boundary segments
  annotate(geom = "segment", x = -5, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = .20, colour = "#2E2E2E", lwd = 0.75)

# save results
ggsave(gg_bayes_hdi,
       path = here::here("2024_robust_statistics", "images"), filename = "rs_hdi_bayes.png",
       dpi = 1200, device = "png")

## HDI Plot Skewed ------------------------------------------------------------------------------------------------------------ ##

# Step 1: Generate a posterior distribution sample
set.seed(42)
posterior_samples <- rgamma(10000, shape = 2, rate = 0.3)  # Skewed distribution

# Step 2: Calculate the 95% HDI
hdi_bounds <- hdi(posterior_samples, credMass = 0.95)

# Step 3: Calculate density and prepare data for plotting
density_data <- density(posterior_samples, n = 1024)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Mark regions for coloring: left tail, HDI region, and right tail using the HDI bounds
density_df$region <- with(density_df, ifelse(
  x < hdi_bounds[1], "Lower Tail",
  ifelse(x > hdi_bounds[2], "Upper Tail", "HDI")
))

# Step 4: Plotting the posterior distribution with the HDI and tail regions
gg_bayes_hdi_skew <- ggplot(density_df, aes(x = x, y = y, fill = region)) +
  # Left tail region (lower values than HDI)
  geom_area(data = subset(density_df, region == "Lower Tail"), fill = viridis(5)[3], alpha = 0.8) +

  # HDI region (highest density interval)
  geom_area(data = subset(density_df, region == "HDI"), fill = viridis(1), alpha = 0.75) +

  # Right tail region (higher values than HDI)
  geom_area(data = subset(density_df, region == "Upper Tail"), fill = viridis(5)[3], alpha = 0.8) +

  # Dashed lines for HDI boundaries
  geom_vline(xintercept = hdi_bounds, linetype = "dashed", color = "black", size = 0.5) +

  # Customize axis and labels
  labs(x = "Parameter value", y = "Posterior") +
  scale_x_continuous(limits = c(-5, 45), breaks = seq(-5,45,10)) +
  scale_y_continuous(limits = c(0,.11), breaks = c(seq(0,.1, .05))) +

  # Customize plot theme
  plot_theme +
  theme(aspect.ratio = 1 / 2) +

  # Custom annotations for boundary segments
  annotate(geom = "segment", x = -5, xend = 45, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = .1, colour = "#2E2E2E", lwd = 0.75)

# save results
ggsave(gg_bayes_hdi_skew,
       path = here::here("2024_robust_statistics", "images"), filename = "rs_hdi_bayes_skew.png",
       dpi = 1200, device = "png")

## Precision Bayes ------------------------------------------------------------------------------------------------------------ ##
set.seed(42)  # for reproducibility
posterior_samples <- rnorm(100000, mean = 5, sd = .1)

# Step 2: Calculate the 95% HDI
hdi <- hdi(posterior_samples, credMass = 0.95)

# Step 3: Calculate density and prepare data for plotting
density_data <- density(posterior_samples, n = 10000)
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Define cut-off points for the tails
lower_tail_cutoff <- quantile(posterior_samples, probs = 0.025)
upper_tail_cutoff <- quantile(posterior_samples, probs = 0.975)

# Mark regions for coloring: left tail, HDI region, and right tail
density_df$region <- with(density_df, ifelse(
  x <= lower_tail_cutoff, "Lower Tail",
  ifelse(x >= upper_tail_cutoff, "Upper Tail", "HDI")
))

# Step 4: Plot the posterior distribution with HDI and colorized tails
gg_bayes_hdi_slim <- ggplot(density_df, aes(x = x, y = y, fill = region)) +
  geom_area(data = subset(density_df, region == "Lower Tail"), fill = viridis(5)[3], alpha = 0.8) +
  geom_area(data = subset(density_df, region == "HDI"), fill = viridis(1), alpha = 0.75) +
  geom_area(data = subset(density_df, region == "Upper Tail"), fill = viridis(5)[3], alpha = 0.8) +

  # Dashed lines for HDI boundaries
  geom_vline(xintercept = hdi, linetype = "dashed", color = "black", size = .5) +

  # Customize axis and labels
  labs(x = "Parameter value", y = "Posterior") +
  scale_x_continuous(limits = c(-5, 15)) +
  scale_y_continuous(limits = c(0, 5)) +

  plot_theme +
  theme(aspect.ratio = 1 / 2) +

  # Custom annotations for boundary segments
  annotate(geom = "segment", x = -5, xend = 15, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75) +
  annotate(geom = "segment", x = -Inf, xend = -Inf, y = 0, yend = 5, colour = "#2E2E2E", lwd = 0.75)

# save results
ggsave(gg_bayes_hdi_slim,
       path = here::here("2024_robust_statistics", "images"), filename = "rs_hdi_bayes_slim.png",
       dpi = 1200, device = "png")

