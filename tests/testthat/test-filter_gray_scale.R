test_that("filter_gray_scale converts to grayscale", {
  # Test with a sample RGB value (let's use a red pixel)
  rgb_val <- c(255, 0, 0)
  gray_val <- filter_gray_scale(rgb_val)

  # Check that the result is a grayscale value (R, G, B should all be the same)
  expect_equal(gray_val[1], gray_val[2])
  expect_equal(gray_val[1], gray_val[3])
})
