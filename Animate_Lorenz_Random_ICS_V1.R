library(ggplot2)
library(deSolve)


# Define the Lorenz system
lorenz <- function(t, state, parameters) {
  x = state[[1]]
  y = state[[2]]
  z = state[[3]]
  sigma = parameters[[1]] 
  rho = parameters[[2]]
  beta = parameters[[3]]
  
  dx <- sigma*(y - x)
  dy <- x*(rho - z) - y
  dz <- x*y - beta*z
  list(c(dx, dy, dz))
}

# Set the parameters
sigma <- 10
rho <- 22 # 13, 14, 15, 28 
beta <- 8/3

# Solve the Lorenz system
times <- seq(0, 40, by = 0.02)

# Assuming you have a list of initial conditions
initial_conditions <- list(c(1, 0, 0), c(-1, 0, 0), c(0, 1, 1), c(0, -1, 1), c(0, 1, -1), c(0, -1, -1))

# Create a list to store the data frames for each trajectory
df_lst <- list()

# Solve the Lorenz system for each initial condition
for (i in 1:length(initial_conditions)) {
  sol <- ode(y = initial_conditions[[i]], times = times, func = lorenz, parms = c(sigma, rho, beta))
  df_lst[[i]] <- data.frame(t = sol[, 1], x = sol[, 2], y = sol[, 3], z = sol[, 4], N = seq_along(sol[, 1]), n_ics = rep(i, times = length(times)))
}

# Combine the trajectories into a single data frame
combined_df <- do.call(rbind, df_lst)

# Choose a color palette
cols = viridis::turbo(max(combined_df$N))
cols = viridis::viridis(max(combined_df$N))
cols = RColorBrewer::brewer.pal(max(combined_df$N), "YlGnBu") #BrBG, YlGnBu, PRGn, PiYG


# Create the plot with multiple trajectories
# Animate in xy
q_xy <- ggplot(combined_df, aes(x = x, y = y, color = combined_df$n_ics)) +
  geom_point(aes(x = x, y = y, color = combined_df$n_ics), alpha = 0.75) +
  scale_color_gradientn(colors = cols, name = "Initial Condition") +
  labs(title = "Lorenz Attractor: Multiple Trajectories", x = "X", y = "Y") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  ) +
  transition_manual(N, cumulative = TRUE) 
#transition_states(combined_df$N, state_length = 1, transition_length = 1)

# Animate the plot
animate(q_xy, dpi = 300)

# Animate in yz
q_yz <- ggplot(combined_df, aes(x = y, y = z, color = combined_df$n_ics)) +
  geom_point(aes(x = y, y = z, color = combined_df$n_ics), alpha = 0.75) +
  scale_color_gradientn(colors = cols, name = "Initial Condition") +
  labs(title = "Lorenz Attractor: Multiple Trajectories", x = "Y", y = "Z") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  ) +
  transition_manual(N, cumulative = TRUE) 
#transition_states(combined_df$N, state_length = 1, transition_length = 1)

# Animate the plot
animate(q_yz, dpi = 300)


# Animate in xz
q_xz <- ggplot(combined_df, aes(x = x, y = z, color = combined_df$n_ics)) +
  geom_point(aes(x = x, y = z, color = combined_df$n_ics), alpha = 0.75) +
  scale_color_gradientn(colors = cols, name = "Initial Condition") +
  labs(title = "Lorenz Attractor: Multiple Trajectories", x = "X", y = "Z") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.subtitle = element_text(size = 15),
    plot.title = element_text(size = 16)
  ) +
  transition_manual(N, cumulative = TRUE) 
  #transition_states(combined_df$N, state_length = 1, transition_length = 1)

# Animate the plot
animate(q_xz, dpi = 300)
