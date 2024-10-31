library(deSolve)
library(ggplot2)
library(gganimate)
library(dplyr)


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



# Jacobian of the system obtained by taking partials
Jac <- function(state, parameters) {
  x = state[[1]]
  y = state[[2]]
  z = state[[3]]
  sigma = parameters[[1]] 
  rho = parameters[[2]]
  beta = parameters[[3]]
  
  # dx <- sigma*(y - x)
  # dy <- x*(rho - z) - y
  # dz <- x*y - beta*z
  # list(c(dx, dy, dz))
  
  J11 = -sigma 
  J12 = sigma 
  J13 = 0
  J21 = rho - z
  J22 = -1 
  J23 = -x 
  J31 = y 
  J32 = x
  J33 = -beta
  
  J = matrix(c(J11, J12, J13, J21, J22, J23, J31, J32, J33), nrow = 3, byrow = TRUE)
  return(J)
}

# Classify stability and behavior of the equilibrium points
classify_eq_pts <- function(df, parameters){
  sigma = parameters[[1]]
  rho = parameters[[2]]
  beta = parameters[[3]]
  
  ## -- Solve equilibrium points analytically (by hand) -- ##
  eq_pts = list(x = c(0, sqrt(as.complex(beta * (rho - 1))), sqrt(-as.complex(beta * (rho - 1)))), 
                y = c(0, sqrt(as.complex(beta * (rho - 1))), sqrt(-as.complex(beta * (rho - 1)))), 
                z = c(0, rho - 1, rho - 1))
  
  eq_pts_1 = c(eq_pts$x[[1]], eq_pts$y[[1]], eq_pts$z[[1]])
  eq_pts_2 = c(eq_pts$x[[2]], eq_pts$y[[2]], eq_pts$z[[2]])
  eq_pts_3 = c(eq_pts$x[[3]], eq_pts$y[[3]], eq_pts$z[[3]])
  
  J1 = Jac(state = eq_pts_1, parameters = c(sigma, rho, beta)) # Jacobian at 1st eq pt
  J2 = Jac(state = eq_pts_2, parameters = c(sigma, rho, beta)) # Jacobian at 2nd eq pt
  J3 = Jac(state = eq_pts_3, parameters = c(sigma, rho, beta)) # Jacobian at 3rd eq pt
  
  # Get the eigenvalues around the eq pts
  eigs_1 = eigen(J1)
  eigs_2 = eigen(J2)
  eigs_3 = eigen(J3)
  
  df$eval1_x = eigs_1$values[[1]]
  df$eval1_y = eigs_1$values[[2]]
  df$eval1_z = eigs_1$values[[3]]
  
  df$eval2_x = eigs_2$values[[1]]
  df$eval2_y = eigs_2$values[[2]]
  df$eval2_z = eigs_2$values[[3]]
  
  df$eval3_x = eigs_3$values[[1]]
  df$eval3_y = eigs_3$values[[2]]
  df$eval3_z = eigs_3$values[[3]]
  
  
  
  eig_vals = list(eigs_1$values, eigs_2$values, eigs_3$values)
  eig_vecs = list(eigs_1$vectors, eigs_2$vectors, eigs_3$vectors)
  
  
  df$x_eq_1 = rep(eq_pts[[1]][1], length(t))
  df$x_eq_2 = rep(eq_pts[[1]][2], length(t))
  df$x_eq_3 = rep(eq_pts[[1]][3], length(t))
  
  df$y_eq_1 = rep(eq_pts[[2]][1], length(t))
  df$y_eq_2 = rep(eq_pts[[2]][2], length(t))
  df$y_eq_3 = rep(eq_pts[[2]][3], length(t))
  
  df$z_eq_1 = rep(eq_pts[[3]][1], length(t))
  df$z_eq_2 = rep(eq_pts[[3]][2], length(t))
  df$z_eq_3 = rep(eq_pts[[3]][3], length(t))
  
  
  ## -- Classify the eq pts -- ##
  
  eq_type = c()
  for (i in 1:length(eq_pts)){
    if (all(Im(eig_vals[[i]]) == 0)){
      # Real Case: a) unstable node, b) stable node, c) saddle 
      if (max(Re(eig_vals[[i]])) > 0 && min(Re(eig_vals[[i]])) >= 0){
        eq_type[[i]] = "Unstable Node"
      }
      else if (max(Re(eig_vals[[i]])) <= 0 && min(Re(eig_vals[[i]])) < 0){
        eq_type[[i]] = "Stable Node"
      }
      else if (max(Re(eig_vals[[i]])) >= 0 && min(Re(eig_vals[[i]])) < 0){
        eq_type[[i]] = "Saddle"
      }
      else {
        eq_type[[i]] = "Other"
      }
    }
    if (any(Im(eig_vals[[i]]) != 0)) {
      # Imaginary Case: a) unstable node, b) stable node, c) saddle 
      if (max(Re(eig_vals[[i]])) > 0) {
        eq_type[[i]] = "Unstable Focus"
      }
      else if (max(Re(eig_vals[[i]])) < 0){
        eq_type[[i]] = "Stable Focus"
      }
      else if (max(Re(eig_vals[[i]])) == 0){
        eq_type[[i]] = "Hopf"
      }
      else {
        eq_type[[i]] = "Other"
      }
    }
  }
  
  # Add classifications to df
  df$eq_type_1 = eq_type[[1]]
  df$eq_type_2 = eq_type[[2]]
  df$eq_type_3 = eq_type[[3]]
  
  # -- # Add Eigenvectors -- #
  # Linearized around 1st eq pt
  df$eq_1_v11 = rep(eig_vecs[[1]][1,1], length(t))
  df$eq_1_v12 = rep(eig_vecs[[1]][1,2], length(t))
  df$eq_1_v13 = rep(eig_vecs[[1]][1,3], length(t))
  
  df$eq_1_v21 = rep(eig_vecs[[1]][2,1], length(t))
  df$eq_1_v22 = rep(eig_vecs[[1]][2,2], length(t))
  df$eq_1_v23 = rep(eig_vecs[[1]][2,3], length(t))
  
  df$eq_1_v31 = rep(eig_vecs[[1]][3,1], length(t))
  df$eq_1_v32 = rep(eig_vecs[[1]][3,2], length(t))
  df$eq_1_v33 = rep(eig_vecs[[1]][3,3], length(t))
  
  # Linearized around 2nd eq pt
  df$eq_2_v11 = rep(eig_vecs[[2]][1,1], length(t))
  df$eq_2_v12 = rep(eig_vecs[[2]][1,2], length(t))
  df$eq_2_v13 = rep(eig_vecs[[2]][1,3], length(t))
  
  df$eq_2_v21 = rep(eig_vecs[[2]][2,1], length(t))
  df$eq_2_v22 = rep(eig_vecs[[2]][2,2], length(t))
  df$eq_2_v23 = rep(eig_vecs[[2]][2,3], length(t))
  
  df$eq_2_v31 = rep(eig_vecs[[2]][3,1], length(t))
  df$eq_2_v32 = rep(eig_vecs[[2]][3,2], length(t))
  df$eq_2_v33 = rep(eig_vecs[[2]][3,3], length(t))
  
  # Linearized around 3rd eq pt
  df$eq_3_v11 = rep(eig_vecs[[3]][1,1], length(t))
  df$eq_3_v12 = rep(eig_vecs[[3]][1,2], length(t))
  df$eq_3_v13 = rep(eig_vecs[[3]][1,3], length(t))
  
  df$eq_3_v21 = rep(eig_vecs[[3]][2,1], length(t))
  df$eq_3_v22 = rep(eig_vecs[[3]][2,2], length(t))
  df$eq_3_v23 = rep(eig_vecs[[3]][2,3], length(t))
  
  df$eq_3_v31 = rep(eig_vecs[[3]][3,1], length(t))
  df$eq_3_v32 = rep(eig_vecs[[3]][3,2], length(t))
  df$eq_3_v33 = rep(eig_vecs[[3]][3,3], length(t))
  
  # Add spectral radius of the Jacobian: max(|\lambda|) where Jx = lambda * x
  df$spectral_radius_1 = max(abs(eig_vals[[1]]))
  df$spectral_radius_2 = max(abs(eig_vals[[2]]))
  df$spectral_radius_3 = max(abs(eig_vals[[3]]))
  df$spectral_radius = max(c(abs(eig_vals[[1]]), abs(eig_vals[[2]]), abs(eig_vals[[3]])))
  df$spectral_radius_id = which.max(c(abs(eig_vals[[1]]), abs(eig_vals[[2]]), abs(eig_vals[[3]])))
  
  return(df)
  
}



# Set the parameters
parameters = list(sigma <- 10,
                  rho <- 31, # 13, 14, 15, 28 
                  beta <- 8/3)


# Set the initial condition
state <- c(1, 0, 0)

# Solve the Lorenz system
times <- seq(0, 40, by = 0.02)
sol <- ode(y = state, times = times, func = lorenz, parms = c(sigma, rho, beta))

# Random initial conditions
n_ics = 51 # number of initial conditions
xu = rnorm(n_ics, mean = mean(sol[, 2]), sd = sqrt(var(sol[, 2])))
yu = rnorm(n_ics, mean = mean(sol[, 3]), sd = sqrt(var(sol[, 3])))
zu = runif(n_ics, min = min(sol[, 4]) - sqrt(var(sol[, 4])), max = max(sol[, 4]) + sqrt(var(sol[, 4])))


## -- Master Fcn -- ##

# Run the Master code #

out_1 = animate_ODE_system(lorenz, parameters, version = 1, iters = n_ics)
out_2 = animate_ODE_system(lorenz, parameters, version = 2, iters = n_ics)
out_3 = animate_ODE_system(lorenz, parameters, version = 3, iters = n_ics)
out_4 = animate_ODE_system(lorenz, parameters, version = 4, iters = n_ics)
out_5 = animate_ODE_system(lorenz, parameters, version = 5, iters = n_ics)
out_6 = animate_ODE_system(lorenz, parameters, version = 6, iters = n_ics)
out_7 = animate_ODE_system(lorenz, parameters, version = 7, iters = n_ics)

 


# Equation to animate Lorenz System
animate_ODE_system <- function(system, parameters, version, iters){
  times <- seq(0, 40, by = 0.02) 
  state <- c(1, 0, 0) 
  n_state <- 1001 # Number of initial conditions
  
  # Numerically integrate
  sol <- ode(y = state, times = times, func = system, parms = parameters)
  
  # Extract the solution
  x <- sol[, 2]
  y <- sol[, 3]
  z <- sol[, 4]
  
  # Differential to evaluate norms
  dx <- sigma*(y - x)
  dy <- x*(rho - z) - y
  dz <- x*y - beta*z
  
  df = data.frame(t = times, x = x, y = y, z = z, dx = dx, dy = dy, dz = dz)
  
  # Add distance from the origin to the coordinates of the trajectory
  df$dist_2 = sqrt(df$x^2 + df$y^2 + df$z^2)
  df$dist_1 = abs(df$x) + abs(df$y) + abs(df$z)
  df$dist_I = df %>% select(x, y, z) %>% apply(., 1, max) # row max - infty norm
  
  # Iterations along the system
  df$N = seq_along(x)
  
  # Take the norms of the differentials:
  df$norm_2 = sqrt(df$dx^2 + df$dy^2 + df$dz^2) # L2 norm
  df$norm_1 = abs(df$dx) + abs(df$dy) + abs(df$dz) # L1 norm
  df$norm_I <- df %>% select(dx, dy, dz) %>% apply(., 1, max) %>% abs()
  
  
  # Classify the properties of the equilibrium coordinates and add eigenvalues: 
  df = classify_eq_pts(df, parameters)
  
  
  
  ## -- Plotting Section -- ##
  
  if (version == 1){
    # Plot the trajectories
    q1 <- ggplot(df, aes(x = x, y = z, color = N, size = norm_1)) +
      geom_point(aes(x = x, y = z, color = N), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Iteration") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Iteration") +
      
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in XZ", x = "x", y = "z") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) +
      transition_manual(N, cumulative = TRUE) 
    
    
    a1 <- animate(q1, dpi = 300)
    
    output_lst <- list(pl = q1, 
                       an = a1,
                       df = df)
    
  }
  
  else if (version == 2){
    q2 <- ggplot(df, aes(x = x, y = y, color = N, size = norm_1)) +
      geom_point(aes(x = x, y = y, color = N), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Iteration") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Iteration") +
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in XY", x = "x", y = "y") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE)
    
    a2 <- animate(q2, dpi = 300)
    
    output_lst <- list(pl = q2, 
                       an = a2,
                       df = df)
  }
  
  else if (version == 3){
    q3 <- ggplot(df, aes(x = y, y = z, color = N, size = norm_1)) +
      geom_point(aes(x = y, y = z, color = N), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Iteration") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Iteration") +
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in YZ", x = "y", y = "z") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE)
    
    a3 <- animate(q3, dpi = 300)
    
    output_lst <- list(pl = q3, 
                       an = a3,
                       df = df)
    
  }
  
  else if (version == 4){
    
    q4 <- ggplot(df, aes(x = x, y = z, color = y, size = norm_I)) +
      geom_point(aes(x = x, y = z, color = y), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Distance to Origin") +
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in XZ", x = "x", y = "z") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE) 
    
    a4 <- animate(q4, dpi = 300)
    
    output_lst <- list(pl = q4, 
                       an = a4,
                       df = df)
    
  }
  
  else if (version == 5){
    q5 <- ggplot(df, aes(x = x, y = y, color = z, size = norm_I)) +
      geom_point(aes(x = x, y = y, color = z), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Distance to Origin") +
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in XY", x = "x", y = "y") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE) 
    
   a5 <-  animate(q5, dpi = 300)
    
    output_lst <- list(pl = q5, 
                       an = a5,
                       df = df)
    
  }
  
  else if (version == 6){
    q6 <- ggplot(df, aes(x = y, y = z, color = x, size = norm_I)) +
      geom_point(aes(x = y, y = z, color = x), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Distance to Origin") +
      #scale_color_gradientn(colors = wesanderson::wes_palette("Zissou1", length(x), type = "continuous"), name = "Norm") +
      
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in YZ", x = "y", y = "z") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE) 
    
    a6 <- animate(q6, dpi = 300)
    
    output_lst <- list(pl = q6, 
                       an = a6,
                       df = df)
    
  }
  
  else if (version == 7){
    q7 <- ggplot(df, aes(x = t, y = x, color = norm_1, size = norm_I)) +
      geom_point(aes(x = t, y = x, color = norm_1), alpha = 0.75) +
      #scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
      scale_color_gradientn(colors = topo.colors(length(x)), name = "Distance to Origin") +
      scale_radius(range = c(1, 5)) + 
      labs(title = "Lorenz Attractor: Trajectory in time - x", x = "t", y = "x") +
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      ) + 
      transition_manual(N, cumulative = TRUE) 
    
    a7 <- animate(q7, dpi = 300)
    
    output_lst <- list(pl = q7, 
                       an = a7,
                       df = df)
    
  }
  return(output_lst)
}










# Add eigenvectors and eq points to animation 
# for (i in 1:length(eig_vecs)) {
#   q6 <- q6 +
#     geom_segment(aes(y_eq = eq_pts[[i]][2], z_eq = eq_pts[[i]][3],
#                      yend = eq_pts[[i]][2] + eig_vecs[[i]][2,2],
#                      zend = eq_pts[[i]][3] + eig_vecs[[i]][3,2]),
#                  color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "open"))
# }
# 
# 
# q6 <- ggplot(df, aes(x = y, y = z, color = dist, size = norm_2)) +
#   geom_point(aes(x = y, y = z, color = dist), alpha = 0.75) +
#   scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
#   scale_radius(range = c(1, 5)) + 
#   labs(title = "Lorenz Attractor: Trajectory in YZ", x = "y", y = "z") +
#   theme_minimal() + 
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 14),
#     axis.subtitle = element_text(size = 15),
#     plot.title = element_text(size = 16)
#   ) + 
#   transition_manual(N, cumulative = TRUE) 
# 
# 
# 
# q6 <- ggplot(df, aes(x = y, y = z, color = dist, size = norm_2)) +
#   geom_point(aes(x_eq = eq_pts[[1]][2], z_eq = eq_pts[[1]][3], color = 'black'), alpha = 0.75) + 
#   geom_point(aes(x = y, y = z, color = dist), alpha = 0.75) +
#   scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
#   scale_radius(range = c(1, 5)) + 
#   labs(title = "Lorenz Attractor: Trajectory in YZ", x = "y", y = "z") +
#   theme_minimal() + 
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 14),
#     axis.subtitle = element_text(size = 15),
#     plot.title = element_text(size = 16)
#   ) + 
#   transition_manual(N, cumulative = TRUE) 
# 
# 
# # -- # -- # -- #
# plt <- ggplot(df, aes(x = y, y = z, color = dist, size = norm_2)) +
#   geom_point(aes(x = y, y = z, color = dist), alpha = 0.75) +
#   geom_point(aes(x = as.numeric(y_eq_1), y = as.numeric(z_eq_1)), alpha = 0.7, size = 3, color = 'black') +
#   geom_point(aes(x = as.numeric(y_eq_2), y = as.numeric(z_eq_2)), alpha = 0.7, size = 3, color = 'black') +
#   geom_point(aes(x = as.numeric(y_eq_3), y = as.numeric(z_eq_3)), alpha = 0.7, size = 3, color = 'black') +
#   scale_color_gradientn(colors = viridis::turbo(length(x)), name = "Distance to Origin") +
#   scale_radius(range = c(1, 5)) + 
#   labs(title = "Lorenz Attractor: Trajectory in YZ", x = "y", y = "z") +
#   theme_minimal() + 
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text = element_text(size = 14),
#     axis.subtitle = element_text(size = 15),
#     plot.title = element_text(size = 16)
#   )
# 
# # -- # -- Needs revision:
# 
# for (i in 1:3) {
#   plt2 <- plt +
#     geom_segment(aes(x = as.numeric(eq_pts[[2]][i]), y = as.numeric(eq_pts[[2]][i]),
#                      xend = as.numeric(eq_pts[[2]][i]) + as.numeric(eig_vecs[[i]][2,i]),
#                      yend = as.numeric(eq_pts[[3]][i]) + as.numeric(eig_vecs[[i]][3,i])),
#                  color = "black", arrow = arrow(length = unit(0.2, "cm"), type = "open"))
# }
# 
# 
# 
# # -- # -- Create a mesh of coordinates and arrows to plot
# x_grid = seq(min(df$x) - 2, max(df$x) + 2, length.out = 20)
# y_grid = seq(min(df$y) - 2, max(df$y) + 2, length.out = 20)
# grid = expand.grid(x1 = x_grid, x2 = y_grid)
# 
# fcn_eval = apply(grid, 1, function(x) lorenz(t, x, parameters = c(sigma, rho, beta))[[1]])
# fcn_eval = apply(grid, 1, function(x) lorenz_dx_dy(t, x, parameters = c(sigma, rho, beta), eq_pts = eq_pts)[[1]])
# 
# ggplot(data.frame(grid, z = fcn_eval), aes(x = x1, y = x2, z = fcn_eval)) +
#   geom_contour(aes(color = ..level..)) +
#   labs(title = "Contour Plot", x = "x1", y = "x2", color = "z")