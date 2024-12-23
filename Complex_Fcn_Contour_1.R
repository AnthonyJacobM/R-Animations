# Install and load necessary packages
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gganimate)) install.packages("gganimate")
library(ggplot2)
library(gganimate)

# Create a function to generate the data for the contour plot
generate_data <- function(pars) {
  l <- pars$l 
  a <- pars$a 
  x <- seq(-1.5, 1.5, length.out = 20)
  y <- seq(-1.5, 1.5, length.out = 20)
  df <- expand.grid(x = x, y = y, z = (df$x^2 + df$y^2)^2 - 2 * a^2 * (df$x^2 - df$y^2) + a^4 - l^4, 
                    a = a, l = l)
  return(df)
}

a_values <- seq(1, 5, by = 0.1) # Set the range of 'a' values for the animation
l <- 2  # Set a value for 'l' (you can change this as needed)
a <- 0.5
l_values <- seq(0.8 * a, 1.7 * a, length.out = 101)
cols = topo.colors(20)
pars$a = a
pars$l = l_values[[1]]
pars$cols = cols
df_lst <- list() 

for (i in 1:length(l_values)){
  l <- l_values[[i]]
  pars$l = l
  df_lst[[i]] <- generate_data(pars)
  df_lst[[i]]$N = i
}

df_cons <- dplyr::bind_rows(df_lst)
df_tmp <- df_cons[df_cons$l == 0.8 * a,]

# Create an animated contour plot
s <- ggplot(df_cons, aes(x = x, y = y, z = z)) +
  stat_contour(aes(color = ..level..), bins = 20) +
  scale_color_gradientn(colors = pars$cols, name = "Magnitude of Slope") + 
  theme_bw() +
  labs(title = paste("Contour Plot of ", "(x^2 + y^2)^2 - 2*a^2*(x^2 - y^2) + a^4 - l^4"),
       subtitle = paste0("a = ", df_cons$a)) + 
  transition_states(df_cons$N)

animate(s)
