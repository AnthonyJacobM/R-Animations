library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

## -- Master Function -- ##

# Inputs: f, pars, version
pars <- list(
  a = 0, # lower bound
  b = 3 * pi / 2, # upper bound
  n = 41, # number of sub-intervals (rectangles)
  h = (b - a) / (n - 1), # step size
  x0 = 0,
  X0 = seq(-2.5, 2.5, by = 0.05),
  x = seq(-3, 3, 0.01),
  y = f(x),
  N = length(X0) 
)


# Define the equation f(x) = x^3 (as a simple example)
f <- function(x) { -x^3 - x^2 + 2*x + 1 }
dfcn <- function(x) { -3*x^2 - 2*x + 2 } # Derivative
F <- function(x) { -x^4/4 - x^3/3 + x^2 + x } # Anti-Derivative

# Calculate the definite integral using the Fundamental Theorem of Calculus
definite_integral <- F(b) - F(a)

output = animate_equ_tangent_line(f, pars, version = 1)
output$an

animate_equ_tangent_line <- function(f, pars, version){
  # Fcn to animate the tangent line of a function as ics varies
  # Version 1: Equ of the tangent as ics varies
  # Version 2: Secant converges to the equ of the tangent
  
  a <- pars$a
  b <- pars$b 
  n <- pars$n
  h <- pars$h 
  N <- pars$N 
  x <- pars$x 
  y <- pars$y 
  X0 <- pars$X0 
  
  if (version == 1){
    df_list <- list()
    for (i in 1:N){
      df <- data.frame(x = x, y = f(x))
      df$X0 = rep(X0[[i]], times = length(x))
      df$Y0 = rep(f(X0[[i]]), times = length(x))
      df$slope = rep(dfcn(X0[[i]]), times = length(x))
      df$intercept = rep(f(X0[[i]]) - dfcn(X0[[i]]) * X0[[i]], times = length(x))
      df$N = rep(i, times = length(x))
      df_list[[i]] = df
    }
    
    df_list <- dplyr::bind_rows(df_list)
    
    tbl_tmp <- df_list %>% group_by(N) %>% summarize(x0 = mean(X0), y0 = mean(Y0), Slope = mean(slope))
    tbl_tmp <- tbl_tmp %>% mutate_if(is.numeric, round, digits = 2)
    
    q0 <- ggplot(df_list, aes(x, y)) +
      geom_line(aes(x = x, y = y), color = "blue", lwd = 1.5) +
      geom_point(aes(x = X0, y = Y0), color = "orange", size = 6) +
      geom_abline(aes(intercept = intercept, slope = slope), color = "black", lwd = 1) +
      labs(title = "Derivative as the Slope of the Tangent", x = "x", y = "y") + 
      theme_minimal() + 
      transition_states(N, state_length = 1)
    
    a0 <- animate(q0)
    
    output_lst <- list(pl = q0, 
                       an = a0, 
                       tbl = tbl_tmp, 
                       df = df_list)
    
  }
  
  else if (version == 2){
    
    x0 <- 2.25
    y0 <- f(x0)
    H <- seq(-5, -0.001, length.out = N)
    
    df_list <- list()
    for (i in 1:N){
      x1 <- x0 + H[[i]]
      y1 <- f(x1)
      slope <- (y1 - y0) / (x1 - x0)
      int <- y1 - slope * x1
      df <- data.frame(x = x, y = f(x))
      df$X0 = rep(x0, times = length(x))
      df$X1 = rep(x1, times = length(x))
      df$Y0 = rep(f(x0), times = length(x))
      df$Y1 = rep(f(x1), times = length(x))
      df$h = rep(H[[i]], times = length(x))
      df$slope = rep(slope, times = length(x))
      df$intercept <- rep(int, times = length(x))
      df$N = rep(i, times = length(x))
      df_list[[i]] = df
    }
    
    df_list <- dplyr::bind_rows(df_list)
    
    tbl_tmp <- df_list %>% group_by(N) %>% summarize(x0 = mean(X0), y0 = mean(Y0),
                                                     x1 = mean(X1), y1 = mean(Y1), 
                                                     h = mean(h), Slope = mean(slope))
    
    tbl_tmp <- tbl_tmp %>% mutate_if(is.numeric, round, digits = 2)
    
    q1 <- ggplot(df_list, aes(x, y)) +
      geom_line(aes(x = x, y = y), color = "blue", lwd = 1.5) +
      geom_point(aes(x = X0, y = Y0), color = "orange", size = 6) + 
      geom_point(aes(x = X1, y = Y1), color = "tan", size = 6) +
      geom_abline(aes(intercept = intercept, slope = slope), color = "black", lwd = 1) +
      labs(title = "Secant Line Approaches the Tangent Line", x = "x", y = "y") + 
      theme_minimal() + 
      transition_states(N, state_length = 1)
    
    a1 <- animate(q1, dpi = 300, fps = 60)
    
    output_lst <- list(pl = q1, 
                       an = a1,
                       tbl = tbl_tmp,
                       df = df_list)
  }
  
  return(output_lst)
}