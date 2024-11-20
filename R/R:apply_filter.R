#' Apply a Filter to a Bitmap
#'
#' @param bitmap A bitmap image object.
#' @param filter_function A function to apply to each pixel.
#' @param ... Additional arguments to pass to the filter function.
#' @return A modified bitmap image.
#' @export
apply_filter <- function(bitmap, filter_function, ...) {
  width <- dim(bitmap)[2]
  height <- dim(bitmap)[3]
  filtered_bitmap <- bitmap
  for (row in 1:height) {
    for (col in 1:width) {
      pixel_index <- ((row - 1) * width + col - 1) * 3 + 1
      rgb_vector <- bitmap[pixel_index:(pixel_index + 2)]
      filtered_bitmap[pixel_index:(pixel_index + 2)] <- filter_function(rgb_vector, ...)
    }
  }
  return(filtered_bitmap)
}
