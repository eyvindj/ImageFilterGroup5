#' Convert RGB to Grayscale
#'
#' @param rgb_vector A raw vector containing RGB values.
#' @return A raw vector representing the grayscale value.
#' @export
convert_to_grayscale <- function(rgb_vector) {
  R <- as.numeric(rgb_vector[1])
  G <- as.numeric(rgb_vector[2])
  B <- as.numeric(rgb_vector[3])
  grayscale_value <- round(R * 0.299 + G * 0.587 + B * 0.114)
  return(as.raw(c(grayscale_value, grayscale_value, grayscale_value)))
}