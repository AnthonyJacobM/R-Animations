library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)
library(latex2exp)
library(ggtext)

# Function to create a data frame for plotting
create_rect_data <- function(f_exp, pars) {
  a = pars$a
  b = pars$b
  n = pars$n
  lambda = pars$lambda
  h <- (b - a) / n
  
  x <- seq(a + h/2, b - h/2, by = h)
  y <- dexp(x, rate = pars$lambda)
  
  df <- data.frame(x = x, y = y, xmin = x - h/2, xmax = x + h/2, ymin = 0, ymax = y)
  df$h = h
  df$Area = abs(df$xmax - df$xmin) * abs(df$ymax - df$ymin)
  df$group = seq_along(x)
  df$lambda = lambda
  return(df)
}

create_hist_data <- function(f_sample_avg, pars){
  df_lst = list()
  num_samples = pars$num_samples
  sample_size = pars$sample_size
  sample_means = f_sample_avg(pars)
  # Normalize the sample avg by subtracting population avg and dividing by standard error
  sample_means_z = (sample_means - 1/pars$lambda) * sqrt(pars$sample_size) * pars$lambda
  
  # Option to use custom breaks:
  sturg_breaks = 1 + log2(num_samples)
  scotts_breaks = (max(sample_means) - min(sample_means)) / (3.49 * sd(sample_means) / num_samples^(1/3))
  scotts_breaks_z = (max(sample_means_z) - min(sample_means_z)) / (3.49 * sd(sample_means_z) / num_samples^(1/3))
  
  h1 <- hist(sample_means, breaks = round(scotts_breaks))
  h2 <- hist(sample_means_z, breaks = round(scotts_breaks_z))
  x <- h1$mids
  y <- h1$counts / sum(h1$counts)
  h <- mean(h1$breaks[2:length(h1$breaks)] - h1$breaks[1:length(h1$breaks)-1]) # avg step size
  
  # Normalized version:
  x_z <- h2$mids
  y_z <- h2$counts / sum(h2$counts)
  h_z <- mean(h2$breaks[2:length(h2$breaks)] - h2$breaks[1:length(h2$breaks)-1]) # avg step size
  
  df <- data.frame(x = x, y = y, xmin = x - h/2, xmax = x + h/2, ymin = 0, ymax = y) 
  df_z <- data.frame(x_z = x_z, y_z = y_z, xmin_z = x_z - h_z/2, xmax_z = x_z + h_z/2, ymin_z = 0, ymax_z = y_z)
  df$h = h
  df_z$h_z = h_z
  df$Area = abs(df$xmax - df$xmin) * abs(df$ymax - df$ymin)
  df_z$Area_z = abs(df_z$xmax_z - df_z$xmin_z) * abs(df_z$ymax_z - df_z$ymin_z)
  df$group = seq_along(x)
  df_z$group = seq_along(x_z)
  df$lambda = lambda
  df_z$lambda = lambda
  df_lst = list(df = df, df_z = df_z)
  return(df_lst)
}



# Example function and parameters
# Optional: use density function instead of dexp()
f_exp_pdf <- function(x, pars) {
  lambda = pars$lambda 
  y = lambda * exp(-lambda * x)
  return(y)
}



# use distribution function instead of pexp()
f_exp_cdf <- function(x, pars) {
  lambda = pars$lambda 
  y = 1 - exp(-lambda * x)
  return(y)
}



# Function to calculate the distribution of the sample means
f_sample_avg <- function(pars){
  lambda = pars$lambda 
  num_samples <- pars$num_samples 
  sample_size <- pars$sample_size
  
  # Simulate the exponential distribution
  set.seed(123)  # For reproducibility
  samples <- replicate(num_samples, rexp(sample_size, rate = lambda))
  
  # Calculate the sample means
  sample_means <- colMeans(samples)
  
  return(sample_means)
}

# Color palette options:
cols_turbo = viridis::turbo(n)
cols_viridis = viridis::viridis(n)
cols_topo = topo.colors(n)
cols_ylgnbu = RColorBrewer::brewer.pal(n, "YlGnBu") #BrBG, YlGnBu, PRGn, PiYG

pars <- list(a = 0.1, b = 3*pi, n = 25, 
             n_lambda = 21, lambda = 1/10, lambda_f = 2.1,
             cols = cols_ylgnbu, 
             sample_size = 5, sample_size_n = 46, 
             sample_size_f = 50, num_samples = 300)

# Variation where lambda or sample size varies
df_lst = list()
df_lst_z = list()

iters = pars$sample_size_n
lambda_bin = seq(pars$lambda, pars$lambda_f, length.out = iters) # vary lambda
lambda_bin_2 = rep(pars$lambda, times = iters)
sample_size_bin = seq(pars$sample_size, pars$sample_size_f, by = 1) # vary sample size

for (i in 1:iters){
  pars$lambda = lambda_bin_2[[i]]
  pars$sample_size = sample_size_bin[[i]]
  hist_data <- create_hist_data(f_sample_avg, pars)
  rect_data <- hist_data[[1]]
  rect_data_z <- hist_data[[2]]
  rect_data$N = i
  rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
  rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y) # or use: pexp(rect_data$x, rate = pars$lambda)
  rect_data$Error = abs(1/pars$lambda - rect_data$y) # Calculate the error between population average and sample distribution
  rect_data$sample_size = pars$sample_size
  rect_data$num_samples = pars$num_samples
  rect_data_z$N = i
  rect_data_z$ymax2_z = cumsum(rect_data_z$ymax_z)/sum(rect_data_z$ymax_z)
  rect_data_z$y2_z = cumsum(rect_data_z$y_z)/sum(rect_data_z$y_z) 
  rect_data_z$Error = abs(1/pars$lambda - rect_data_z$y_z) # Calculate the error between population average and sample distribution
  rect_data_z$sample_size = pars$sample_size
  rect_data_z$num_samples = pars$num_samples
  df_lst[[i]] = rect_data
  df_lst_z[[i]] = rect_data_z
}

df_appxs = dplyr::bind_rows(df_lst)
df_appxs_z = dplyr::bind_rows(df_lst_z)

# Create the data frame for plotting
lambda0 = min(df_appxs$lambda)
lambdaf = max(df_appxs$lambda)
rect_data <- df_appxs[df_appxs$sample_size == 30,] # Critical value of sample size where CLT applies
rect_data_z <- df_appxs_z[df_appxs_z$sample_size == 30,] # Critical value of sample size where CLT applies


# PDF of CLT (Exponential Distribution) and Animation:
q_pdf <- ggplot(rect_data, aes(x, y, fill = xmax)) +
  #geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = xmax), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote(bar(Y)[n]), y = bquote("P(" ~ bar(Y)[n] ~ "=" ~ bar(y) ~ ")"), fill = "Sample\nAverage") +
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
    #axis.title.x = element_markdown()
  )

# Run Animation
a1 <- animate(q_pdf, dpi = 300)

# PDF of CLT (Normalized Exponential Distribution) and Animation:
q_pdf_z <- ggplot(rect_data_z, aes(x_z, y_z, fill = xmax_z)) +
  #geom_line() +
  geom_rect(aes(xmin = xmin_z, xmax = xmax_z, ymin = ymin_z, ymax = ymax_z, fill = xmax_z), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote("(" ~ bar(Y) ~  " - " ~ mu ~ ")" ~  sqrt(n)/sigma), y = bquote("P(" ~ bar(Y)[n] ~ "=" ~ bar(y) ~ ")"), fill = "Standard\nError") +
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
    #axis.title.x = element_markdown()
  )

# Run Animation
a1_z <- animate(q_pdf_z, dpi = 300)


# Number of rectangles increases:
q_pdf2 <-ggplot(df_appxs, aes(x, y, fill = xmax)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = xmax), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote(bar(Y)[n]), y = bquote("P(" ~ bar(Y)[n] ~ "=" ~ bar(y) ~ ")"), fill = "Sample\nAverage") +
  #labs(subtitle = paste("Number of Rectangles:", ~ df_appxs$N, "\nArea:", round(sum(df_appxs$Area), 3))) + 
  theme_minimal() + 
  #transition_manual(df$N, cumulative = TRUE) + 
  transition_states(states = df_appxs$N, state_length = 1) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a2 <- animate(q_pdf2, dpi = 300)

# Normalized version:
q_pdf2_z <-ggplot(df_appxs_z, aes(x_z, y_z, fill = xmax_z)) +
  geom_rect(aes(xmin = xmin_z, xmax = xmax_z, ymin = ymin_z, ymax = ymax_z, fill = xmax_z), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote("(" ~ bar(Y) ~  " - " ~ mu ~ ")" ~  sqrt(n)/sigma), y = bquote("P(" ~ bar(Y)[n] ~ "=" ~ bar(y) ~ ")"), fill = "Standard\nError") +
  theme_minimal() + 
  #transition_manual(df$N, cumulative = TRUE) + 
  transition_states(states = df_appxs_z$N, state_length = 1) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a2_z <- animate(q_pdf2_z, dpi = 300)


# -- Animate CDF -- #
# CDF of Exponential Plot and Animation:
q_cdf <- ggplot(rect_data, aes(x, y2, fill = xmax)) +
  #geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = xmax), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote(bar(Y)[n]), y = bquote("P(" ~ bar(Y)[n] ~ "<" ~ bar(y) ~ ")"), fill = "Sample\nAverage") +
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a3 <- animate(q_cdf, dpi = 300)

# -- Animate CDF -- #
# CDF of Exponential Plot and Animation:
q_cdf_z <- ggplot(rect_data_z, aes(x_z, y2_z, fill = xmax_z)) +
  #geom_line() +
  geom_rect(aes(xmin = xmin_z, xmax = xmax_z, ymin = ymin_z, ymax = ymax2_z, fill = xmax_z), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote("(" ~ bar(Y) ~  " - " ~ mu ~ ")" ~  sqrt(n)/sigma), y = bquote("P(" ~ bar(Y)[n] ~ "<" ~ bar(y) ~ ")"), fill = "Sample\nError") +
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a3_z <- animate(q_cdf_z, dpi = 300)


q_cdf2 <-ggplot(df_appxs, aes(x, y2, fill = xmax)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = xmax), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote(bar(Y)[n]), y = bquote("P(" ~ bar(Y)[n] ~ "<" ~ bar(y) ~ ")"), fill = "Sample\nAverage") +
  theme_minimal() + 
  #transition_manual(df$N, cumulative = TRUE) + 
  transition_states(states = df_appxs$N, state_length = 1) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a4 <- animate(q_cdf2, dpi = 300)

# CDF of Exponential Plot and Animation:
q_cdf2_z <- ggplot(df_appxs_z, aes(x_z, y2_z, fill = xmax_z)) +
  geom_rect(aes(xmin = xmin_z, xmax = xmax_z, ymin = ymin_z, ymax = ymax2_z, fill = xmax_z), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Distribution of the Sample Average: Exponential Distribution", 
       x = bquote("(" ~ bar(Y) ~  " - " ~ mu ~ ")" ~  sqrt(n)/sigma), y = bquote("P(" ~ bar(Y)[n] ~ "<" ~ bar(y) ~ ")"), fill = "Standard\nError") +
  transition_states(states = df_appxs_z$N, state_length = 1) +
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a4_z <- animate(q_cdf2_z, dpi = 300)












# Error vs the Distribution of the Sample Averages:
q_error <- ggplot(df_appxs, aes(x, Error, fill = Error)) +
  #geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = Error, fill = xmax), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Distribution of the Sample Average", 
       x = bquote(bar(Y)[n]), y = bquote("P(" ~ bar(Y)[n] ~ "=" ~ bar(y) ~ ")"), fill = "Error") +
  theme_minimal() + 
  #transition_manual(df$N, cumulative = TRUE) + 
  transition_states(states = df_appxs$N, state_length = 1) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a_error <- animate(q_error, dpi = 300)