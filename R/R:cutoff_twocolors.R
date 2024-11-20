#' Apply Color Cutoff or Grayscale Filter
#'
#' @param rgb_vector A raw vector containing RGB values.
#' @param cutoff A numeric value for the grayscale cutoff. If NULL, revert to grayscale (default: NULL).
#' @return A raw vector with the modified RGB values.
#' @export
color_cutoff_filter <- function(rgb_vector, cutoff = NULL) {
  # Extract RGB values
  R <- as.numeric(rgb_vector[1])
  G <- as.numeric(rgb_vector[2])
  B <- as.numeric(rgb_vector[3])
  
  # Convert to grayscale
  grayscale_value <- round(R * 0.299 + G * 0.587 + B * 0.114)
  
  if (!is.null(cutoff)) {
    # Apply cutoff for two-color effect
    if (grayscale_value > cutoff) {
      return(as.raw(c(255, 0, 255)))  # Pink
    } else {
      return(as.raw(c(0, 255, 0)))    # Green
    }
  } else {
    # Revert to grayscale
    return(as.raw(rep(grayscale_value, 3)))  # Grayscale in R, G, B
  }
}
