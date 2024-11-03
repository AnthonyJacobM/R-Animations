library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

## -- Master Function -- ##

# Inputs: f, pars, version
pars <- list(
  a = -2.5, # lower bound
  b = 2, # upper bound
  n = 41, # number of sub-intervals (rectangles)
  h = (b - a) / (n - 1), # step size
  N = 60 # number of frames
)


# Define the equation
f <- function(x) {
  cos(x)/(1 + x)
}

f <- function(x) { -x^3 - x^2 + 2*x + 1 }



# Run the code: 
output <- animate_riemann_sum(f, pars, version = 4)
output$df

animate_riemann_sum <- function(f, pars, version){
  # Work in progress - check each of the versions for compatability 
  # Increase number of parameters to improve user creativity
  
  a <- pars$a
  b <- pars$b 
  n <- pars$n 
  h <- pars$h
  N <- pars$N
  
  # Riemann Sum Function
  riemann_sum <- function(f, a, b, n) {
    h <- (b - a) / n
    x <- seq(a, b, by = h)
    y <- f(x)
    sum(y) * h
  }
  
  # Alternative Expression:
  riemann_sum_grouped <- function(f, a, b, n, groups) {
    h <- (b - a) / n
    x <- seq(a, b, by = h)
    y <- f(x)
    group <- floor((x - a) / (1*h)) + 1  # Group based on rectangle index
    data.frame(x, y, group)
  }
  
  if (version == 1){
    
    # Create a df for plotting
    df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
    df$group = as.factor(seq_along(df$x))
    df$Area = as.numeric(h * df$y)
    
    
    # Basic Riemann Sum
    q0 <- ggplot(df, aes(x, y)) +
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
    
    a0 <- animate(q0, dpi = 300)
    
    # List of outputs
    output_lst <- list(
      pl = q0, 
      an = a0, 
      df = df
    )
    
  }
  
  else if (version == 2){
    
    # Create a df for plotting
    df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
    df$group = as.factor(seq_along(df$x))
    df$Area = as.numeric(h * df$y)
    
    q1 <- ggplot(df, aes(x, y, fill = Area)) +
      geom_line() +
      geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y, fill = Area), color = "black", alpha = 0.7) +
      #scale_fill_manual(values = c("blue", "red"), labels = c("Positive Area", "Negative Area")) +
      scale_fill_gradientn(colors = rev(viridis::turbo(n))) +
      labs(title = "Riemann Sum Approximation", 
           x = "x", y = "f(x)") +
      transition_manual(group, cumulative = TRUE) + 
      theme_minimal() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      )
    
    # Run Animation
    a1 <- animate(q1, dpi = 300)
    
    # List of outputs
    output_lst <- list(
      pl = q1, 
      an = a1, 
      df = df
    )
    
  }
  
  else if (version == 3){
    
    # Create a list of data frames with varying number of subrectangles:
    df_list <- list()
    
    for (i in 1:N){
      n = i + 5
      h = (b - a) / (n - 1)
      df <- data.frame()
      df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
      df$Area = as.vector(h * df$y)
      df$N = rep(n, times = n)
      df_list[[i]] = df
    }
    
    # Combine the dataframes into a single dataframe
    df_list <- dplyr::bind_rows(df_list)
    
    q2 <-ggplot(df_list, aes(x, y, fill = Area)) +
      geom_line() +
      geom_rect(aes(xmin = x, xmax = x + (b - a) / n, ymin = 0, ymax = y, fill = Area), color = "black", alpha = 0.7) +
      scale_fill_gradientn(colors = rev(viridis::turbo(n))) +
      labs(title = "Riemann Sum Approximation", 
           x = "x", y = "f(x)") +
      #labs(subtitle = paste("Number of Rectangles:", ~ df_list$N, "\nArea:", round(sum(df_list$Area), 3))) + 
      theme_minimal() + 
      #transition_manual(df$N, cumulative = TRUE) + 
      transition_states(states = df_list$N, state_length = 1) +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      )
    
    a2 <- animate(q2, dpi = 300)
    
    # List of outputs:
    output_lst <- list(
      pl = q2, 
      an = a2, 
      df = df_list
    )
    
  }
  
  else if (version == 4){
    
    # Create a list of data frames with varying number of subrectangles:
    df_list <- list()
    
    for (i in 1:N){
      n = i + 5
      h = (b - a) / (n - 1)
      df <- data.frame()
      df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
      df$Area = as.vector(h * df$y)
      df$N = rep(n, times = n)
      df_list[[i]] = df
    }
    
    # Combine the dataframes into a single dataframe
    df_list <- dplyr::bind_rows(df_list)
    
    # Variation color scheme per frame:
    
    q3 <- ggplot(df_list, aes(x, y, fill = Area)) +
        geom_line() +
        geom_rect(aes(xmin = x, xmax = x + (b - a) / df_list$N, ymin = 0, ymax = y, fill = Area), color = "black", alpha = 0.7) +
        scale_fill_gradientn(colors = rev(viridis::turbo(n))) +
        labs(title = "Riemann Sum Approximation", 
             x = "x", y = "f(x)") +
        #labs(subtitle = paste("Number of Rectangles:", ~ df_list$N, "\nArea:", round(sum(df_list$Area), 3))) + 
        theme_minimal() + 
        #transition_manual(df$N, cumulative = TRUE) + 
        transition_states(states = df_list$N, state_length = 1) +
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.subtitle = element_text(size = 15),
          plot.title = element_text(size = 16)
        )
    
    a3 <- animate(q3, dpi = 300)
    
    # List of outputs:
    output_lst <- list(
      pl = q3, 
      an = a3, 
      df = df_list
    )
    
  }
  
  else if (version == 5){
    # Create a list of data frames with varying number of subrectangles:
    df_list <- list()
    
    for (i in 1:N){
      n = i + 5
      h = (b - a) / (n - 1)
      df <- data.frame()
      df <- data.frame(x = seq(a, b, length.out = n), y = f(seq(a, b, length.out = n)))
      df$Area = as.vector(h * df$y)
      df$N = rep(n, times = n)
      df_list[[i]] = df
    }
    
    # Combine the dataframes into a single dataframe
    df_list <- dplyr::bind_rows(df_list)
    
    # Group the dataframes by number of rectangles and sum the Area.
    df_tbl <- df_list %>% group_by(N) %>% summarize(Area = sum(Area))
    
    # Solve the analytical solution (f is cosine):
    analytical <- integrate(f, a, b)[1]$value
    error <- abs(df_tbl$Area - analytical)
    df_tbl$Error <- error
    df_tbl <- df_tbl %>% mutate_if(is.numeric, round, digits = 3)
    df_tbl %>% flextable()
    tmp <- as.data.frame(df_tbl)
    
    # Dynamic visualization of error:
    q4 <- ggplot(tmp, aes(N, Error, fill = Error)) +
      geom_line() +
      geom_rect(aes(xmin = N, xmax = N + (max(N) - min(N)) / (length(N) - 1), ymin = 0, ymax = Error, fill = Error), color = "black", alpha = 0.7) +
      scale_fill_gradientn(colors = (viridis::turbo(n))) +
      labs(title = "Error of Riemann Sum Approximation", 
           x = "Number of Rectangles (n)", y = "Error") +
      #labs(subtitle = paste("Number of Rectangles:", ~ df_list$N, "\nArea:", round(sum(df_list$Area), 3))) + 
      theme_minimal() + 
      transition_manual(tmp$N, cumulative = TRUE) + 
      #transition_states(states = tmp$N, state_length = 1) +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.subtitle = element_text(size = 15),
        plot.title = element_text(size = 16)
      )
    
    a4 <- animate(q4, dpi = 300)
    
    # List of outputs:
    output_lst <- list(
      pl = q4, 
      an = a4, 
      df = tmp
    )
    
  }
  
  else if (version == 6){
    
    # Group the dataframes by number of rectangles and sum the Area.
    df_tbl <- df_list %>% group_by(N) %>% summarize(Area = sum(Area))
    
    # Solve the analytical solution (f is cosine):
    analytical <- integrate(f, a, b)[1]$value
    error <- abs(df_tbl$Area - analytical)
    df_tbl$Error <- error
    df_tbl <- df_tbl %>% mutate_if(is.numeric, round, digits = 3)
    df_tbl %>% flextable()
    tmp <- as.data.frame(df_tbl)
    
    q5 <- ggplot(tmp, aes(N, Error, fill = Error)) +
        geom_line() +
        geom_rect(aes(xmin = min(N), xmax = N + (max(N) - min(N)) / (length(N) - 1), ymin = 0, ymax = Error, fill = Error), color = "black", alpha = 0.7) +
        scale_fill_gradientn(colors = (viridis::turbo(n))) +
        labs(title = "Error of Riemann Sum Approximation", 
             x = "N", y = "Error") +
        #labs(subtitle = paste("Number of Rectangles:", ~ df_list$N, "\nArea:", round(sum(df_list$Area), 3))) + 
        theme_minimal() + 
        transition_manual(tmp$N, cumulative = TRUE) + 
        #transition_states(states = tmp$N, state_length = 1) +
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.subtitle = element_text(size = 15),
          plot.title = element_text(size = 16)
        )
    
    a5 <- animate(q5, dpi = 300)
    
    # List of outputs:
    output_lst <- list(
      pl = q5, 
      an = a5, 
      df = tmp
    )
    
  }
  
  return (output_lst)
}