library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

# Function to create a data frame for plotting
create_rect_data <- function(f_geom, pars) {
  a = pars$a
  b = pars$b
  n = pars$n
  prob = pars$prob
  h <- (b - a) / n
  
  x <- floor(seq(a + h/2, b - h/2, by = h))
  y <- dgeom(x, prob = prob)
  
  df = data.frame(x = x, y = y, xmin = x - h/2, xmax = x + h/2, ymin = 0, ymax = y)
  df$h = h
  df$Area = abs(df$xmax - df$xmin) * abs(df$ymax - df$ymin)
  df$group = seq_along(x)
  df$prob = prob
  return(df)
}

# Example function and parameters
# Optional: use density function instead of dgeom() ->
f_geom <- function(k, pars) {
  prob = pars$prob
  y = (1 - prob)^(k) * prob
  return(y)
}

cols_turbo = viridis::turbo(n)
cols_viridis = viridis::viridis(n)
cols_topo = topo.colors(n)
cols_ylgnbu = RColorBrewer::brewer.pal(n, "YlGnBu") #BrBG, YlGnBu, PRGn, PiYG
cols_gnwtrd = colorRampPalette(c('green', 'white', 'red'))(n)

pars <- list(a = 0, b = 10, n = 10, n_prob = 21, prob = 0.1, cols = cols_topo)

# Variation where lambda varies
df_lst = list()

iters = pars$n_prob
prob_bin = seq(pars$prob, 1 - pars$prob, length.out = pars$n_prob)

for (i in 1:iters){
  pars$prob = prob_bin[[i]]
  rect_data <- create_rect_data(f_geom, pars)
  rect_data$N = i
  rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
  rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y) # or use: pgeom(rect_data$x, rate = pars$prob)
  df_lst[[i]] = rect_data
}

df_appxs = dplyr::bind_rows(df_lst)



# Create the data frame for plotting
prob0 = min(df_appxs$prob)
probf = max(df_appxs$prob)
rect_data <- df_appxs[df_appxs$prob == 0.5,]


# PDF of Geometric Plot and Animation:
q_pdf <- ggplot(rect_data, aes(x, y, fill = xmax)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = xmax), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Geometric Distribution: PDF", 
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
  labs(title = "Geometric Distribution: PDF", 
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
# CDF of Geometric Plot and Animation:
#rect_data$ymax2 = cumsum(rect_data$ymax)/sum(rect_data$ymax)
#rect_data$y2 = cumsum(rect_data$y)/sum(rect_data$y)
q_cdf <- ggplot(rect_data, aes(x, y2, fill = ymax2)) +
  geom_line() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax2, fill = ymax2), color = "black", alpha = 0.7) + 
  #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) + 
  scale_fill_gradientn(colors = pars$cols) + 
  labs(title = "Geometric Distribution: CDF", 
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
  labs(title = "Geometric Distribution: CDF", 
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


