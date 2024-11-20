#' Replace Pixels with a Filter Function
#'
#' This function applies a pixel-level transformation to a bitmap image using a specified filter function.
#' The filter function can be `filter_gray_scale`, `filter_two_color`, or any custom filter function that returns a raw RGB triplet
#'
#' @param bitmap A bitmap image (e.g., from the magick package).
#' @param filter_function A transformation function to apply to each pixel (e.g., `filter_gray_scale` or `filter_two_color`).
#' @param ... Additional arguments passed to `filter_function` (e.g., `cutoff` for `filter_two_color`).
#' @return A filtered bitmap with modified RGB values.
#' @export

replace_pixels <- function(bitmap, filter_function = filter_gray_scale, ...) {

  if (is.null(filter_function)) {
    stop("A valid filter_function (filter_gray_scale or filter_two_color) must be provided.")
  }

  width <- dim(bitmap)[2]  # Number of columns
  height <- dim(bitmap)[3] # Number of rows

 # print(paste("Width (columns):", width))
  #print(paste("Height (rows):", height))

  # Create a copy of the bitmap to store filtered values
  filtered_bitmap <- bitmap

  # Loop through each pixel
  for (row in 1:height) {
    for (col in 1:width) {
      # Extract RGB values for the current pixel
      R <- as.numeric(bitmap[1, col, row])
      G <- as.numeric(bitmap[2, col, row])
      B <- as.numeric(bitmap[3, col, row])

      #print(c(R,G,B))
      # Apply the filter function
      filtered_rgb_triplet <- filter_function(c(R, G, B), ...)

      # Update the filtered bitmap
      filtered_bitmap[1, col, row] <- as.raw(filtered_rgb_triplet[1])
      filtered_bitmap[2, col, row] <- as.raw(filtered_rgb_triplet[2])
      filtered_bitmap[3, col, row] <- as.raw(filtered_rgb_triplet[3])

     # print(paste("Filtered Pixel - R:", filtered_rgb_triplet[1], "G:", filtered_rgb_triplet[2], "B:", filtered_rgb_triplet[3]))
    }
  }

  return(filtered_bitmap)
}





