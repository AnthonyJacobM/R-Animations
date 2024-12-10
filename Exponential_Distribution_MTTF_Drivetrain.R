library(ggplot2)
library(gganimate)
library(viridis)
library(dplyr)
library(flextable)

# Define the rate parameter
lambda <- 1/5000

# Generate random data from the exponential distribution
binwidth <- 500
df <- data.frame(value = sort(rexp(100, rate = lambda))) 
df$group = ifelse(df$value < 3000, "Y < 3000", "Y >= 3000")
df$pdf = dexp(df$value, rate = lambda)
df$cdf = pexp(df$value, rate = lambda)
df$iterate = seq_along(df$value)
df$xmin = df$value - binwidth/2
df$xmax = df$value + binwidth/2
df$ymin = 0
df$ymax_pdf = df$pdf
df$ymax_cdf = df$cdf


ggplot(df, aes(x = value, fill = value)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..), 
                     fill = cumsum(..count..)/sum(..count..)), 
                 binwidth = 500, 
                 color = "black") + 
  scale_fill_gradient(low = 'green', high = 'orange') + 
  labs(x = "Miles until Drivetrain Failure", 
       y = bquote("P(" ~ Y ~ "<" ~ y ~ ")"), 
       title = "PDF: Exponential Distribution") + 
  theme_bw()


# Probability drivetrain will fail within 3000 miles
q1 = round(pexp(3000, rate = 1/5000), 4)

ggplot(df, aes(x = value, fill = value)) + 
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..), 
                     fill = cumsum(..count..)/sum(..count..)), 
                 binwidth = 500, 
                 color = "black") + 
  scale_fill_gradient(low = 'green', high = 'orange') + 
  labs(x = "Miles until Drivetrain Failure (Y)", 
       y = bquote("P(" ~ Y ~ "<" ~ y ~ ")"), 
       title = "CDF: Exponential Distribution") + 
  theme_bw() + 
  geom_vline(xintercept = 3000, color = 'blue', lwd = 1.75)


# Probability drivetrain will last longer than 7000 miles
q2 = round(1 - pexp(7000, rate = 1/5000))

ggplot(df, aes(x = value, fill = value)) + 
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..), 
                     fill = cumsum(..count..)/sum(..count..)), 
                 binwidth = 500, 
                 color = "black") + 
  scale_fill_gradient(low = 'green', high = 'orange') + 
  labs(x = "Miles until Drivetrain Failure (Y)", 
       y = bquote("P(" ~ Y ~ "<" ~ y ~ ")"), 
       title = "CDF: Exponential Distribution") + 
  theme_bw() + 
  geom_vline(xintercept = 7000, color = 'blue', lwd = 1.75)



# Determine the distance at which 20$ of drivetrains fail
q3 = round(qexp(0.2, rate = 1/5000), 2)

ggplot(df, aes(x = value, fill = value)) + 
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..), 
                     fill = cumsum(..count..)/sum(..count..)), 
                 binwidth = 500, 
                 color = "black") + 
  scale_fill_gradient(low = 'green', high = 'orange') + 
  labs(x = "Miles until Drivetrain Failure (Y)", 
       y = bquote("P(" ~ Y ~ "<" ~ y ~ ")"), 
       title = "CDF: Exponential Distribution") + 
  theme_bw() + 
  geom_vline(xintercept = q3, color = 'blue', lwd = 1.75)

