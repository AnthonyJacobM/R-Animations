# Load necessary libraries
library(ggplot2)
library(gganimate)
library(viridis)

# Set up parameters for the rectangle
top_length <- 7
bottom_length <- 7
height <- 3

# Define the vertices of the rectangle
coords <- data.frame(
  x = c(0, top_length, top_length, 0),
  y = c(0, 0, height, height)
)

vertices <- coords

# Generate frames by filling the rectangle from bottom to top or left to right
frames <- 100  # Number of frames for animation
filled_heights <- seq(0, height, length.out = frames)
filled_widths = seq(0, top_length, length.out = frames)

# Create a frame-by-frame dataset - create a sequence along vert axis
fill_data <- do.call(rbind, lapply(filled_heights, function(h) {
  data.frame(
    x = c(0, 7, 7, 0),
    y = pmin(vertices$y, h),  # Take the minimum between vertices and fill height
    fill_height = h
  )
}))

# Similar, but create a sequence along horiz axis
fill_data_w <- do.call(rbind, lapply(filled_widths, function(w) {
  data.frame(
    x = pmin(vertices$x, w), # take minimum between vertices of x and fill width
    y = vertices$y,
    fill_width = w
  )
}))

# Generate the plot with animation
animate_rect <- ggplot(fill_data, aes(x, y, color = fill_height)) +
  #geom_polygon(data = coords, aes(fill = NA, color = "black")) + 
  geom_polygon(aes(x, y, fill = fill_height), color = "black") +
  #scale_fill_identity() +
  scale_fill_gradientn(colors = (topo.colors(nrow(fill_data)))) + 
  #labs(title = paste0('Height: ', round(fill_data$fill_height, 0))) +
  coord_fixed() +
  theme_void() +
  transition_states(fill_height, transition_length = 2, state_length = 1) +
  enter_fade() + 
  exit_fade() + 
  theme(legend.position = 'none') + 
  ease_aes('cubic-in-out')

# Render animation
animate(animate_rect, nframes = frames, fps = 10)


# Version with width as a parameter
animate_rect_w <- ggplot(fill_data_w, aes(x, y, color = fill_width)) +
  #geom_polygon(data = coords, aes(fill = NA, color = "black")) + 
  geom_polygon(aes(x, y, fill = fill_width), color = "black") +
  #scale_fill_identity() +
  scale_fill_gradientn(colors = rev(topo.colors(nrow(fill_data_w)))) + 
  #labs(title = paste0('Height: ', round(fill_data$fill_height, 0))) +
  coord_fixed() +
  theme_void() +
  transition_states(fill_width, transition_length = 2, state_length = 1) +
  enter_fade() + 
  exit_fade() + 
  theme(legend.position = 'none') + 
  ease_aes('cubic-in-out')

# Render animation
animate(animate_rect_w, nframes = frames, fps = 10)

