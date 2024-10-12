library(ggplot2)
library(gganimate)

# Define the function
f <- function(x) {
  2 * x * exp(-x^2)
}

# Riemann sum function
riemann_sum <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  sum(y) * h
}

# Set parameters
a <- 0
b <- 4
n <- 40
h <- (b - a) / n

# Alternative expression:
riemann_sum_grouped <- function(f, a, b, n, groups) {
  h <- (b - a) / n
  x <- seq(a, b, by = h)
  y <- f(x)
  group <- floor((x - a) / (1*h)) + 1  # Group based on rectangle index
  data.frame(x, y, group)
}

f <- function(x) {
  2*x * exp(-x^2)
}

f <- function(x) {
  cos(x)
}

# Parameters
a <- 0
b <- 3 * pi / 2
n <- 40
h <- (b - a) / n

# Create a data frame for plotting
df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
df$group = as.factor(seq_along(df$x))
df2 <- riemann_sum_grouped(f=f, a=a, b=b, n=n)



# Basic Riemann Sum
q0 <- animate(
  ggplot(df, aes(x, y)) +
    geom_line() +
    geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y), fill = 'blue', color = "blue", alpha = 0.2) +
    labs(title = "Riemann Sum Approximation", 
         subtitle = "Cos(x)", 
         x = "x", y = "f(x)") +
    transition_manual(group, cumulative = TRUE) + 
    theme_minimal() + 
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      axis.subtitle = element_text(size = 15),
      plot.title = element_text(size = 16)
    )
)


# Dual color Riemann Sum
# The Following Code needs revision:
df$group = as.factor(seq_along(df$x))
q1 <- animate(
  ggplot(df, aes(x, y)) +
    geom_line() +
    geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y, fill = area > 0), color = "blue", alpha = 0.2) +
    scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) +
    labs(title = "Riemann Sum Approximation", 
         subtitle = "Cos(x)", 
         x = "x", y = "f(x)") +
    transition_manual(group, cumulative = TRUE) + 
    theme_minimal() + 
    theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
    )
)



# Run Animation
q1





# Create an animation
# p <- ggplot(df, aes(x, y)) +
#   geom_line() +
#   geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y), color = "blue", fill = "blue", alpha = 0.2) +
#   labs(title = "Riemann Sum Approximation", x = "x", y = "f(x)") +
#   theme_minimal()
# 
# 
# anim <- p + gganimate::transition_reveal(along = df$x)
# anim2 <- p + gganimate::transition_states(state = 1:n)
# anim3 <- p + gganimate::transition_layers(layer_length = n)
# anim4 <- p + gganimate::transition_manual(row(df)[,1])
# 
# 
# anim4
# 
# 
# p2 <- animate(
#   ggplot(df, aes(x, y)) +
#   geom_line() +
#   geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y), color = "blue", fill = "blue", alpha = 0.2) +
#   labs(title = "Riemann Sum Approximation", x = "x", y = "f(x)") +
#   transition_states(group, state_length = 0.1) + 
#   theme_minimal()
# )
# 
# p3 <- animate(
#   ggplot(df, aes(x, y)) +
#     geom_line() +
#     geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y), color = "blue", fill = "blue", alpha = 0.2) +
#     labs(title = "Riemann Sum Approximation", x = "x", y = "f(x)") +
#     transition_layers(layer_length = 1) + 
#     theme_minimal()
# )

