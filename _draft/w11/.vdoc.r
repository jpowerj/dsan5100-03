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
source("../_globals.r")
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
#| label: z-test-hypothesized-dist
#| fig-align: center
library(tidyverse)
hyp_mu <- 120
sigma <- 20
bird_dnorm <- function(x) dnorm(x, mean=hyp_mu, sd=sigma)
sigma_df <- tribble(
    ~x,
    hyp_mu - 3 * sigma,
    hyp_mu - 2 * sigma,
    hyp_mu - 1 * sigma,
    hyp_mu + 1 * sigma,
    hyp_mu + 2 * sigma,
    hyp_mu + 3 * sigma
)
sigma_df <- sigma_df |> mutate(
    y = 0,
    xend = x,
    yend = bird_dnorm(x)
)
plot_rad <- 4 * sigma
plot_bounds <- c(hyp_mu - plot_rad, hyp_mu + plot_rad)
bird_plot <- ggplot(data=data.frame(x=plot_bounds), aes(x=x)) +
  stat_function(fun=bird_dnorm, linewidth=g_linewidth) +
  geom_vline(aes(xintercept=hyp_mu), linewidth=g_linewidth) +
  geom_segment(data=sigma_df, aes(x=x, y=y, xend=xend, yend=yend), linewidth=g_linewidth, linetype="dashed") +
  dsan_theme("half") +
  labs(
    title = "Our Hypothesized Bird Distribution",
    x = "Wingspan Values (m)",
    y = "Probability Density"
  )
bird_plot
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
#| label: simulated-sample-means
simulate_sample_mean <- function(N, mu, sigma) {
    samples <- rnorm(N, mu, sigma)
    sample_mean <- mean(samples)
    return(sample_mean)
}
N <- 16
obs_sample_mean <- 131
num_reps <- 10000
sample_means <- as_tibble(replicate(num_reps, simulate_sample_mean(N, hyp_mu, sigma)))
asymp_dnorm <- function(x) dnorm((x - hyp_mu)/(sigma / sqrt(N)), 0, 1)
ggplot() +
  geom_histogram(
    data=sample_means,
    aes(x=value, y=5*after_stat(density)),
    binwidth=1) +
  stat_function(
    data=data.frame(x=c(100,140)),
    aes(x=x, color='asymp'),
    fun=asymp_dnorm,
    linewidth = g_linewidth
  ) +
  geom_segment(
    aes(x = obs_sample_mean, xend=obs_sample_mean, y=-Inf, yend=Inf, color='obs'),
    linewidth = g_linewidth
  ) +
  dsan_theme("half") +
  labs(
    x = "Simulated Sample Mean",
    y = "(Empirical) Density",
    title = paste0(num_reps," Simulated Sample Means, N = 16")
  ) +
  scale_color_manual(values=c('asymp'='black', 'obs'=cbPalette[1]), labels=c('asymp'='Asymptotic\nDistribution', 'obs'='Observed\nSample Mean')) +
  remove_legend_title()
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
#| label: z-dist-with-sample-mean
#| fig-align: center
label_df_signif_int <- tribble(
    ~x, ~y, ~label,
    2.7, 0.075, "95% Signif.\nCutoff"
)
compute_z_val <- function(x) {
    return ((x - hyp_mu) / (sigma / sqrt(N)))
}
obs_zval <- compute_z_val(obs_sample_mean)
sample_dnorm <- function(x) dnorm(compute_z_val(x))
ggplot(data=data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, linewidth=g_linewidth) +
  #geom_segment(data=sigma_df, aes(x=x, y=y, xend=xend, yend=yend), linewidth=g_linewidth, linetype="dashed") +
  geom_vline(aes(xintercept = obs_zval, color='sample_mean'), linewidth=g_linewidth) +
  dsan_theme("half") +
  labs(
    title = "z-Test Distribution",
    x = "z Scores",
    y = "Probability Density"
  ) +
  scale_color_manual(values=c('sample_mean'=cbPalette[1]), labels=c('sample_mean'='Z(Observed Sample Mean)')) +
  remove_legend_title()
#
#
#
#
#
#
#| label: z-test-left
#| fig-align: center
library(tidyverse)
int_tstat <- 1.041
int_tstat_str <- sprintf("%.02f", int_tstat)
label_df_int <- tribble(
    ~x, ~y, ~label,
    0.25, 0.05, paste0("P(t > ",int_tstat_str,")\n= 0.3")
)

funcShaded <- function(x, lower_bound, upper_bound){
    y <- dnorm(x)
    y[x < lower_bound | x > upper_bound] <- NA
    return(y)
}
funcShadedIntercept <- function(x) funcShaded(x, int_tstat, Inf)
funcShadedSignif <- function(x) funcShaded(x, 1.96, Inf)
ggplot(data=data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, linewidth=g_linewidth) +
  geom_vline(aes(xintercept=int_tstat), linewidth=g_linewidth) +
  geom_vline(aes(xintercept = 1.96), linewidth=g_linewidth, linetype="dashed") +
  stat_function(fun = funcShadedIntercept, geom = "area", fill = cbPalette[1], alpha = 0.5) +
  stat_function(fun = funcShadedSignif, geom = "area", fill = "grey", alpha = 0.333) +
  geom_text(label_df_int, mapping = aes(x = x, y = y, label = label), size = 10) +
  geom_text(label_df_signif_int, mapping = aes(x = x, y = y, label = label), size = 8) +
  # Add single additional tick
  scale_x_continuous(breaks=c(-2, 0, int_tstat, 2), labels=c("-2","0",int_tstat_str,"2")) +
  dsan_theme("quarter") +
  labs(
    title = "z Values vs. Normal Distribution",
    x = "z",
    y = "Density"
  ) +
  theme(axis.text.x = element_text(colour = c("black", "black", cbPalette[1], "black")))
#
#
#
#
#
#
#
#| label: left-tailed-test
pnorm(-1.84)
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
#| label: right-tailed-test
1 - pnorm(1.24)
pnorm(1.24, lower.tail = FALSE)
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
#| label: basic-scatter
library(ggplot2)
library(tibble)
x_data <- seq(from=0, to=1, by=0.02)
num_x <- length(x_data)
y_data <- x_data + runif(num_x, 0, 0.2)
reg_df <- tibble(x=x_data, y=y_data)
ggplot(reg_df, aes(x=x, y=y)) +
  geom_point(size=g_pointsize) +
  dsan_theme("quarter")
```
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
#| label: basic-regression
ggplot(reg_df, aes(x=x, y=y)) + 
  geom_point(size=g_pointsize) +
  geom_smooth(method = "lm", se = FALSE, color = cbPalette[1], formula = y ~ x, linewidth = g_linewidth*3) +
  dsan_theme("quarter")
```
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
#| label: pc-line
#| fig-width: 6
N <- 11
x <- seq(from = 0, to = 1, by = 1 / (N - 1))
y <- x + rnorm(N, 0, 0.25)
mean_y <- mean(y)
spread <- y - mean_y
df <- tibble(x = x, y = y, spread = spread)
ggplot(df, aes(x=x, y=y)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color=cbPalette[1], linewidth=g_linewidth*2) +
  geom_segment(xend=(x+y)/2, yend=(x+y)/2, linewidth=g_linewidth*2, color=cbPalette[2]) +
  geom_point(size=g_pointsize) +
  coord_equal() +
  dsan_theme("half") +
  labs(
    title = "Principal Component Line"
  )
#
#
#
#| label: pca-line
#| fig-width: 6
ggplot(df, aes(x=x, y=y)) +
  geom_point(size=g_pointsize) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color=cbPalette[1], linewidth=g_linewidth*2) +
  geom_segment(xend=x, yend=x, linewidth=g_linewidth*2, color=cbPalette[2]) +
  coord_equal() +
  dsan_theme("half") +
  labs(
    title = "Regression Line"
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
#| label: gdp-plot
#| warning: false
#| fig-width: 10
#| fig-height: 4
library(readr)
library(ggplot2)
gdp_df <- read_csv("assets/gdp_pca.csv")

dist_to_line <- function(x0, y0, a, c) {
    numer <- abs(a * x0 - y0 + c)
    denom <- sqrt(a * a + 1)
    return(numer / denom)
}
# Finding PCA line for industrial vs. exports
x <- gdp_df$industrial
y <- gdp_df$exports
lossFn <- function(lineParams, x0, y0) {
    a <- lineParams[1]
    c <- lineParams[2]
    return(sum(dist_to_line(x0, y0, a, c)))
}
o <- optim(c(0, 0), lossFn, x0 = x, y0 = y)
ggplot(gdp_df, aes(x = industrial, y = exports)) +
    geom_point(size=g_pointsize/2) +
    geom_abline(aes(slope = o$par[1], intercept = o$par[2], color="pca"), linewidth=g_linewidth, show.legend = TRUE) +
    geom_smooth(aes(color="lm"), method = "lm", se = FALSE, linewidth=g_linewidth, key_glyph = "blank") +
    scale_color_manual(element_blank(), values=c("pca"=cbPalette[2],"lm"=cbPalette[1]), labels=c("Regression","PCA")) +
    dsan_theme("half") +
    remove_legend_title() +
    labs(
      title = "PCA Line vs. Regression Line",
	    x = "Industrial Production (% of GDP)",
	    y = "Exports (% of GDP)"
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
#| label: pca-plot
#| warning: false
ggplot(gdp_df, aes(pc1, .fittedPC2)) +
    geom_point(size = g_pointsize/2) +
    geom_hline(aes(yintercept=0, color='PCA Line'), linetype='solid', size=g_linesize) +
    geom_rug(sides = "b", linewidth=g_linewidth/1.2, length = unit(0.1, "npc"), color=cbPalette[3]) +
    expand_limits(y=-1.6) +
    scale_color_manual(element_blank(), values=c("PCA Line"=cbPalette[2])) +
    dsan_theme("full") +
    remove_legend_title() +
    labs(
      title = "Exports vs. Industrial Production in Principal Component Space",
      x = "First Principal Component (Dimension of Greatest Variance)",
      y = "Second Principal Component"
    )
#
#
#
#
#
#| label: pca-facet-plot
#| warning: false
library(dplyr)
library(tidyr)
plot_df <- gdp_df %>% select(c(country_code, pc1, agriculture, military))
long_df <- plot_df %>% pivot_longer(!c(country_code, pc1), names_to = "var", values_to = "val")
long_df <- long_df |> mutate(
  var = case_match(
    var,
    "agriculture" ~ "Agricultural Production",
    "military" ~ "Military Spending"
  )
)
ggplot(long_df, aes(x = pc1, y = val, facet = var)) +
    geom_point() +
    facet_wrap(vars(var), scales = "free") +
	dsan_theme("full") +
	labs(
		x = "Industrial-Export Dimension",
		y = "% of GDP"
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
#| label: lin-reg
#| echo: true
lin_model <- lm(military ~ industrial, data=gdp_df)
summary(lin_model)
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
#| label: t-stat-intercept
#| fig-align: center
#| fig-height: 6
library(ggplot2)
int_tstat <- 1.041
int_tstat_str <- sprintf("%.02f", int_tstat)
label_df_int <- tribble(
    ~x, ~y, ~label,
    0.25, 0.05, paste0("P(t > ",int_tstat_str,")\n= 0.3")
)
label_df_signif_int <- tribble(
    ~x, ~y, ~label,
    2.7, 0.075, "95% Signif.\nCutoff"
)
funcShaded <- function(x, lower_bound, upper_bound){
    y <- dnorm(x)
    y[x < lower_bound | x > upper_bound] <- NA
    return(y)
}
funcShadedIntercept <- function(x) funcShaded(x, int_tstat, Inf)
funcShadedSignif <- function(x) funcShaded(x, 1.96, Inf)
ggplot(data=data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, linewidth=g_linewidth) +
  geom_vline(aes(xintercept=int_tstat), linewidth=g_linewidth) +
  geom_vline(aes(xintercept = 1.96), linewidth=g_linewidth, linetype="dashed") +
  stat_function(fun = funcShadedIntercept, geom = "area", fill = cbPalette[1], alpha = 0.5) +
  stat_function(fun = funcShadedSignif, geom = "area", fill = "grey", alpha = 0.333) +
  geom_text(label_df_int, mapping = aes(x = x, y = y, label = label), size = 10) +
  geom_text(label_df_signif_int, mapping = aes(x = x, y = y, label = label), size = 8) +
  # Add single additional tick
  scale_x_continuous(breaks=c(-2, 0, int_tstat, 2), labels=c("-2","0",int_tstat_str,"2")) +
  dsan_theme("quarter") +
  labs(
    title = "t Value for Intercept",
    x = "t",
    y = "Density"
  ) +
  theme(axis.text.x = element_text(colour = c("black", "black", cbPalette[1], "black")))
#
#
#
#
#
#
#| label: t-stat-coef
#| fig-align: center
#| fig-height: 6
library(ggplot2)
coef_tstat <- 2.602
coef_tstat_str <- sprintf("%.02f", coef_tstat)
label_df_coef <- tribble(
    ~x, ~y, ~label,
    3.65, 0.06, paste0("P(t > ",coef_tstat_str,")\n= 0.01")
)
label_df_signif_coef <- tribble(
  ~x, ~y, ~label,
  1.05, 0.03, "95% Signif.\nCutoff"
)
funcShadedCoef <- function(x) funcShaded(x, coef_tstat, Inf)
ggplot(data=data.frame(x=c(-4,4)), aes(x=x)) +
  stat_function(fun=dnorm, linewidth=g_linewidth) +
  geom_vline(aes(xintercept=coef_tstat), linetype="solid", linewidth=g_linewidth) +
  geom_vline(aes(xintercept=1.96), linetype="dashed", linewidth=g_linewidth) +
  stat_function(fun = funcShadedCoef, geom = "area", fill = cbPalette[2], alpha = 0.5) +
  stat_function(fun = funcShadedSignif, geom = "area", fill = "grey", alpha = 0.333) +
  # Label shaded area
  geom_text(label_df_coef, mapping = aes(x = x, y = y, label = label), size = 10) +
  # Label significance cutoff
  geom_text(label_df_signif_coef, mapping = aes(x = x, y = y, label = label), size = 8) +
  coord_cartesian(clip = "off") +
  # Add single additional tick
  scale_x_continuous(breaks=c(-4, -2, 0, 2, coef_tstat, 4), labels=c("-4", "-2","0", "2", coef_tstat_str,"4")) +
  dsan_theme("quarter") +
  labs(
    title = "t Value for Coefficient",
    x = "t",
    y = "Density"
  ) +
  theme(axis.text.x = element_text(colour = c("black", "black", "black", "black", cbPalette[2], "black")))
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
#| label: residual-plot
library(broom)
gdp_resid_df <- augment(lin_model)
ggplot(gdp_resid_df, aes(x = .fitted, y = .resid)) +
    geom_point(size = g_pointsize/2) +
    geom_hline(yintercept=0, linetype="dashed") +
    dsan_theme("quarter") +
    labs(
      title = "Residual Plot for Industrial ~ Military",
      x = "Fitted Value",
      y = "Residual"
    )
#
#
#
#| label: heteroskedastic
x <- 1:80
errors <- rnorm(length(x), 0, x^2/1000)
y <- x + errors
het_model <- lm(y ~ x)
df_het <- augment(het_model)
ggplot(df_het, aes(x = .fitted, y = .resid)) +
    geom_point(size = g_pointsize / 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    dsan_theme("quarter") +
    labs(
        title = "Residual Plot for Heteroskedastic Data",
        x = "Fitted Value",
        y = "Residual"
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
#
#
#
#
#
#| label: base-rates-plot
#| warning: false
library(tidyverse)
num_librarians <- 350000
num_farmers <- 800000000
occu_df <- tribble(
    ~Occupation, ~Count,
    "Farmer", num_farmers,
    "Librarian", num_librarians,
)

ggplot(occu_df, aes(x=factor(Occupation, levels=c("Librarian","Farmer")), y=Count, fill=Occupation)) +
  geom_bar(stat='identity') +
  dsan_theme() +
  labs(
    x = "Occupation",
    y = "Count",
    title = "Librarians vs. Farmers Globally"
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
