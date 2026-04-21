#' Monte Carlo Simulation Points
#'
#'Description:
#' Generates random points for use in Monte Carlo Numerical Integration.
#' Can be called at four resolutions (n = 10, 100, 1000, 10000) and combined
#' to create a small multiple plot showing integration accuracy at each level.
#'
#' @param n A numeric value giving the number of random points to generate.
#' @param xmin A numeric value giving the minimum x-coordinate.
#' @param xmax A numeric value giving the maximum x-coordinate.
#' @param ymin A numeric value giving the minimum y-coordinate.
#' @param ymax A numeric value giving the maximum y-coordinate.
#'
#' @return A data frame with two columns: \code{x} for the randomly generated
#'   x values and \code{y} for the randomly generated y values.
#'
#' @examples
#' monte_carlo_points(n = 10, xmin = 2, xmax = 6, ymin = 0, ymax = 4)
#'
#' monte_carlo_points(n = 100, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
#'
#' @export

## Part 1

monte_carlo_points <- function(n, xmin, xmax, ymin, ymax) {
  
  # Generate random x and y values
  x_vals <- runif(n, min = xmin, max = xmax)
  y_vals <- runif(n, min = ymin, max = ymax)
  
  # Combine into a data frame
  mc_data <- data.frame(
    x = x_vals,
    y = y_vals
  )
  
  return(mc_data)
}

## Part 2

library(ggplot2)

# Generate points
mc_data <- monte_carlo_points(n = 1000, xmin = -4, xmax = 4, ymin = 0, ymax = 0.4)

# Classify each point 
mc_data$flag <- ifelse(mc_data$y <= dnorm(mc_data$x, mean = 0, sd = 1),
                       "on/below", "above")

# Calculate the area
rect_area <- (4 - (-4)) * (0.4 - 0)

# Calculate the proportion of points on or below the curve
p <- mean(mc_data$flag == "on/below")

# Estimate the integral
est_integral <- rect_area * p

# reate the plot
ggplot(mc_data, aes(x = x, y = y, color = flag)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    linewidth = 1.2
  ) +
  labs(
    title = "Monte Carlo Integration Example, n = 1000",
    x = "x",
    y = "y",
    color = "flag",
    caption = paste("Est. Numerical Integration:", round(est_integral, 4))
  ) +
  theme_minimal()