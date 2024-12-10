library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

# Function to create a data frame for plotting
create_rect_data <- function(f_exp, pars) {
  a = pars$a
  b = pars$b
  n = pars$n
  lambda = pars$lambda
  h <- (b - a) / n
  
  x <- seq(a + h/2, b - h/2, by = h)
  y <- dexp(x, rate = pars$lambda)
  
  
  df = data.frame(x = x, y = y, xmin = x - h/2, xmax = x + h/2, ymin = 0, ymax = y)
  df$h = h
  df$Area = abs(df$xmax - df$xmin) * abs(df$ymax - df$ymin)
  df$group = seq_along(x)
  df$lambda = lambda
  return(df)
}

# Example function and parameters
# Optional: use density function instead of dexp() ->
f_exp <- function(x, pars) {
  lambda = pars$lambda 
  y = lambda * exp(-lambda * x)
  return(y)
}

cols_turbo = viridis::turbo(n)
cols_viridis = viridis::viridis(n)
cols_topo = topo.colors(n)
cols_ylgnbu = RColorBrewer::brewer.pal(n, "YlGnBu") #BrBG, YlGnBu, PRGn, PiYG
cols_gnwtrd = colorRampPalette(c('green', 'white', 'red'))(n)

pars <- list(a = 0.1, b = 7*pi, n = 25, n_lambda = 21, lambda = 0.1, cols = cols_ylgnbu)

# Variation where lambda varies
df_lst = list()

iters = pars$n_lambda
lambda_bin = seq(pars$lambda, pars$lambda + 2, length.out = iters)

for (i in 1:iters){
  pars$lambda = lambda_bin[[i]]
  rect_data <- create_rect_data(f_exp, pars)
  rect_data$N = i
  rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
  rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y) # or use: pexp(rect_data$x, rate = pars$lambda)
  df_lst[[i]] = rect_data
}

df_appxs = dplyr::bind_rows(df_lst)



# Create the data frame for plotting
lambda0 = min(df_appxs$lambda)
lambdaf = max(df_appxs$lambda)
rect_data <- df_appxs[df_appxs$lambda == 0.1,]


# PDF of Exponential Plot and Animation:
q_pdf <- ggplot(rect_data, aes(x, y, fill = xmax)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = xmax), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Exponential Distribution: PDF", 
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
q_pdf2 <-ggplot(df_appxs, aes(x, y, fill = xmax)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = xmax), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Exponential Distribution: PDF", 
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
# CDF of Exponential Plot and Animation:
#rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
#rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y)
q_cdf <- ggplot(rect_data, aes(x, y2, fill = ymax2)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = ymax2), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Exponential Distribution: CDF", 
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
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = ymax2), color = "black", alpha = 0.7) + 
  scale_fill_gradientn(colors = pars$cols) +
  labs(title = "Exponential Distribution: CDF", 
       x = "Y", y = "P(Y <= y)", fill = "CDF") +
  theme_minimal() + 
  transition_states(states = df_appxs$N, state_length = 1) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  )

a4 <- animate(q_cdf2, dpi = 300)




fcn = function(x){2/3*x^3 - 3/2*x^2 + x}
