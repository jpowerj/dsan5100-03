#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: r-source-globals
source("../../_globals.r")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: simulate-pop
#| warning: false
library(tidyverse)
library(latex2exp)
# Populations
N <- 100
M <- 100
# Labels
chi_label <- TeX('$X \\sim \\chi$')
psi_label <- TeX('$Y \\sim \\psi$')
x_df <- tibble(val = runif(N, 0, 10), Population = "chi")
x_mean <- mean(x_df$val)
x_mean_str <- sprintf("%.2f", x_mean)
xbar_label <- TeX(paste0('$\\bar{X} = ',x_mean_str,'$'))
y_df <- tibble(val = runif(M, 1, 15), Population = "psi")
y_mean <- mean(y_df$val)
y_mean_str <- sprintf("%.2f", y_mean)
ybar_label <- TeX(paste0('$\\bar{Y} = ',y_mean_str,'$'))
data_df <- bind_rows(x_df, y_df)
mean_df <- tribble(
    ~var, ~var_mean,
    "x", x_mean,
    "y", y_mean
)
ggplot(data_df, aes(x=val, fill=Population)) +
  geom_density(alpha = 0.5, linewidth = g_linewidth) +
  geom_vline(
    data=mean_df,
    aes(xintercept=var_mean, color=var),
    linewidth = g_linewidth,
    linetype = "dashed"
  ) +
  dsan_theme() +
  scale_fill_manual(
    values = c(cbPalette[1], cbPalette[2]),
    labels = c(chi_label, psi_label)
  ) +
  scale_color_manual(
    "Sample Means",
    values = c(cbPalette[1], cbPalette[2]),
    labels = c(xbar_label, ybar_label)
  ) +
  labs(
    title = "Empirical Distributions of X and Y",
    x = "Sampled Value",
    y = "Empirical Probability Density"
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: t-test-statistic
# Compute information about sample X
x_df <- data_df |> filter(Population == "chi")
x_mean <- mean(x_df$val)
x_variance <- var(x_df$val)
x_size <- nrow(x_df)
# Compute information about sample Y
y_df <- data_df |> filter(Population == "psi")
y_mean <- mean(y_df$val)
y_variance <- var(y_df$val)
y_size <- nrow(y_df)
# And now use this information to compute the test statistic
numer <- x_mean - y_mean
var_term_x <- x_variance / x_size
var_term_y <- y_variance / y_size
denom <- sqrt(var_term_x + var_term_y)
t_obs <- numer / denom
t_obs
#
#
#
#
#
#
#
2 * pt(t_obs, df = N + M)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: pooled-mean
z_mean <- mean(data_df$val)
# Here "valp" is short for "value-prime", the transformed
# values x' and y' from the equations above
x_df <- x_df |> mutate(
    valp = val - x_mean + z_mean,
)
xp_vals <- x_df$valp
xp_mean <- mean(xp_vals)
xp_mean_str <- sprintf("%.2f", xp_mean)
xpbar_label <- TeX(paste0("$\\bar{X'} = ",xp_mean_str,"$"))
y_df <- y_df |> mutate(
    valp = val - y_mean + z_mean
)
yp_vals <- y_df$valp
yp_mean <- mean(y_df$valp)
yp_mean_str <- sprintf("%.2f", yp_mean)
ypbar_label <- TeX(paste0("$\\bar{Y'} = ",yp_mean_str,"$"))
synth_df <- bind_rows(x_df, y_df)
synth_mean_df <- tribble(
    ~var, ~var_mean,
    "x", xp_mean,
    "y", yp_mean
)
ggplot(synth_df, aes(x=valp, fill=Population)) +
  geom_density(alpha = 0.5, linewidth = g_linewidth) +
  geom_vline(
    data=synth_mean_df,
    aes(xintercept=var_mean, color=var),
    linewidth = g_linewidth,
    linetype = "dashed",
    alpha = 0.5
  ) +
  dsan_theme() +
  scale_fill_manual(
    values = c(cbPalette[1], cbPalette[2]),
    labels = c(chi_label, psi_label)
  ) +
  scale_color_manual(
    "Sample Means",
    values = c(cbPalette[1], cbPalette[2]),
    labels = c(xpbar_label, ypbar_label)
  ) +
  labs(
    title = "Distributions of Synthetic X and Y",
    x = "Sampled Value",
    y = "Empirical Probability Density"
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: perform-bootstrap
num_bs_samples <- 1000
sample_with_replacement <- function(sample_vals) {
    sample_size <- length(sample_vals)
    subsample <- sample(sample_vals, sample_size, replace = TRUE)
    return(subsample)
}
perform_bootstrap <- function(sample_vals, B) {
    bootstrapped_datasets <- replicate(B, sample_with_replacement(sample_vals))
    return(bootstrapped_datasets)
}
bootstrap_samples_x <- perform_bootstrap(xp_vals, num_bs_samples)
bootstrap_samples_y <- perform_bootstrap(yp_vals, num_bs_samples)
#
#
#
#
#
#| label: check-bootstrap-dims
#| code-fold: show
dim(bootstrap_samples_x)
dim(bootstrap_samples_y)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: bootstrapped-stats
#| code-fold: show
compute_test_stat <- function(xp, yp) {
    # Compute information about x'
    xp_mean <- mean(xp)
    xp_variance <- var(xp)
    xp_size <- length(xp)
    # Compute information about y'
    yp_mean <- mean(yp)
    yp_variance <- var(yp)
    yp_size <- length(yp)
    # And compute the test statistic using this info
    numer <- xp_mean - yp_mean
    var_term_xp <- xp_variance / xp_size
    var_term_yp <- yp_variance / yp_size
    denom <- sqrt(var_term_xp + var_term_yp)
    t_b <- numer / denom
    return(t_b)
}
# For efficiency we could combine this into a single
# vectorized operation! But, the for loop here makes
# it clear that we're obtaining B = 1000 test statistic
# values
all_test_stats <- c()
for (i in 1:num_bs_samples) {
    cur_xtilde <- bootstrap_samples_x[,i]
    cur_ytilde <- bootstrap_samples_y[,i]
    cur_test_stat <- compute_test_stat(cur_xtilde, cur_ytilde)
    all_test_stats <- c(all_test_stats, cur_test_stat)
}
#
#
#
#
#
#| label: check-num-test-stats
#| code-fold: show
length(all_test_stats)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: plot-test-stats
ts_df <- tibble(t = all_test_stats)
ggplot(ts_df, aes(x=t)) +
  geom_density(linewidth = g_linewidth) +
  dsan_theme() +
  labs(
    title = "Distribution of 1000 Bootstrapped Test Statistics"
  )
#
#
#
#
#
#
#
#
#
#
