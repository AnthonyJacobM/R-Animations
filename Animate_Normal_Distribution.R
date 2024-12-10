library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

# Function to create a data frame for plotting
create_rect_data <- function(f, pars) {
  a = pars$a
  b = pars$b
  n = pars$n
  h <- (b - a) / n
  
  x <- seq(a + h/2, b - h/2, by = h)
  y <- dnorm(x, mean = pars$mu, sd = pars$sd)
  
  
  df = data.frame(x = x, y = y, xmin = x - h/2, xmax = x + h/2, ymin = 0, ymax = y)
  df$Area = abs(df$xmax - df$xmin) * abs(df$ymax - df$ymin)
  df$group = seq_along(x)
  return(df)
}

# Example function and parameters
# Optional: use density function instead of dnorm() ->
f <- function(x, pars) {
  mu = pars$mu 
  sd = pars$sd
  z = (x - mu) / sd
  y = 1/(sd * sqrt(2*pi)) *  exp(-1/2*z^2)
  return(y)
}

cols_turbo = viridis::turbo(n)
cols_viridis = viridis::viridis(n)
cols_topo = topo.colors(n)
cols_ylgnbu = RColorBrewer::brewer.pal(n, "YlGnBu") #BrBG, YlGnBu, PRGn, PiYG

pars <- list(a = -pi, b = pi, n = 20, nf = 60, mu = 0, sd = 1, cols = cols_topo)

# Variation where number of sub-rectangles increases
df_lst = list()

iters = pars$nf - pars$n
for (i in 1:iters){
  k = i + (pars$nf - iters)
  pars$n = k
  rect_data <- create_rect_data(f, pars)
  rect_data$N = k
  rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
  rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y) # or use: pnorm(rect_data$x)
  df_lst[[i]] = rect_data
}

df_appxs = dplyr::bind_rows(df_lst)



# Create the data frame for plotting
n0 = min(df_appxs$N)
rect_data <- df_appxs[df_appxs$N == n0,]


# PDF of Standard Normal Plot and Animation:
q_pdf <- ggplot(rect_data, aes(x, y, fill = ymax)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ymax), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Standard Normal Distribution: PDF", 
       x = "Y", y = "P(Y = y)", fill = "PDF") + 
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

# Run Animation
a1 <- animate(q_pdf, dpi = 300)


# Number of rectangles increases:
q_pdf2 <-ggplot(df_appxs, aes(x, y, fill = ymax)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ymax), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Standard Normal Distribution: PDF", 
       x = "Y", y = "P(Y = y)", fill = "PDF") +
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



# -- Animate CDF -- #
# CDF of Standard Normal Plot and Animation:
#rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
#rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y)
q_cdf <- ggplot(rect_data, aes(x, y2, fill = ymax2)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = ymax2), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Standard Normal Distribution: CDF", 
       x = "Y", y = "P(Y <= y)", fill = "CDF") + 
  transition_manual(group, cumulative = TRUE) +  
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a3 <- animate(q_cdf, dpi = 300)


# Number of rectangles increases:
q_cdf2 <-ggplot(df_appxs, aes(x, y2, fill = ymax2)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = ymax2), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Standard Normal Distribution: CDF", 
       x = "Y", y = "P(Y <= y)", fill = "CDF") +
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

a4 <- animate(q_cdf2, dpi = 300)


