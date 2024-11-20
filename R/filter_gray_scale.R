#' filter_gray_scale
#' Convert a given RGB pixel to grayscale.
#'
#' This function converts a color pixel to grayscale using a standard
#' formula for luminance: `0.299*R + 0.587*G + 0.114*B`.
#'
#' @param pixel_rgb_numeric A numeric vector containing RGB values (not raw).
#' @return A raw vector with the modified RGB values.
#' @export


# filter_function gray scale
filter_gray_scale <- function(pixel_rgb_numeric) {

  if (length(pixel_rgb_numeric) != 3) {
    stop("Input must be a numeric vector with 3 elements (R, G, B).")
  }

  gray_value <- round(pixel_rgb_numeric[1] * 0.299 +
                        pixel_rgb_numeric[2] * 0.587 +
                        pixel_rgb_numeric[3] * 0.114)
 # print(gray_value)
 # print(class(gray_value))
  return(as.raw(rep(gray_value, 3)))
}

