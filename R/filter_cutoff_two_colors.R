#' filter_two_color
#'
#' This function applies a grayscale filter to a pixel or converts it to a two-color effect
#' (using a cutoff value for decision-making). If no cutoff is provided, the pixel is converted
#' to grayscale.
#'
#' @param pixel_rgb_numeric A numeric vector containing RGB values (not raw).
#' @param cutoff A numeric value for the grayscale cutoff. If NULL, revert to grayscale (default: NULL).
#' @return A raw vector with the modified RGB values.
#' @export
filter_two_color <- function(pixel_rgb_numeric, cutoff = NULL,...) {

  # Check if the cutoff is NULL, indicating grayscale filter
  if (is.null(cutoff)) {
    warning("cutoff is NULL, applying grayscale filter.")
  }

  # Ensure that pixel_rgb_numeric is a numeric vector
  if (length(pixel_rgb_numeric) != 3) {
    stop("Input must be a numeric vector of length 3 (R, G, B).")
  }

  # Extract RGB values from the input vector
  R <- pixel_rgb_numeric[1]
  G <- pixel_rgb_numeric[2]
  B <- pixel_rgb_numeric[3]

  # Convert to grayscale using the luminance formula
  grayscale_value <- round(R * 0.299 + G * 0.587 + B * 0.114)

  # Apply the cutoff for two-color effect
  if (!is.null(cutoff)) {
    if (grayscale_value > cutoff) {
      # Color if greater than cutoff (e.g., pink)
      return(as.raw(c(255, 0, 255)))  # Pink
    } else {
      # Color if less than cutoff (e.g., green)
      return(as.raw(c(0, 255, 0)))    # Green
    }
  } else {
    # If no cutoff, return grayscale
    return(as.raw(rep(grayscale_value, 3)))  # Grayscale in R, G, B
  }
}
