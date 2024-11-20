library(testthat)

test_that("filter_two_color applies the cutoff correctly", {

  # Test case 1: Pixel above the cutoff
  # RGB values that should result in a color greater than the cutoff
  pixel_above_cutoff <- c(200, 100, 50)  # This pixel is expected to be pink (R = 255, G = 0, B = 255)
  cutoff_value <- 100  # Set cutoff for testing
  result_above <- filter_two_color(pixel_above_cutoff, cutoff = cutoff_value)

  # Check that the pixel result is pink (R = 255, G = 0, B = 255) as it is above the cutoff
  expect_equal(result_above, as.raw(c(255, 0, 255)))

  # Test case 2: Pixel below the cutoff
  # RGB values that should result in a color less than the cutoff
  pixel_below_cutoff <- c(50, 50, 50)  # This pixel is expected to be green (R = 0, G = 255, B = 0)
  result_below <- filter_two_color(pixel_below_cutoff, cutoff = cutoff_value)

  # Check that the pixel result is green (R = 0, G = 255, B = 0) as it is below the cutoff
  expect_equal(result_below, as.raw(c(0, 255, 0)))
})
