test_that("load_data works correctly", {

  # Test valid data loading with processed data
  df <- load_data(region = "br", file = "match_history")
  expect_true(is.data.frame(df))  # Check if the result is a data frame
  expect_true(ncol(df) > 0)      # Ensure the data has columns (assuming it's not empty)

  # Test error for invalid region
  expect_error(load_data(region = "xyz", file = "match_history"))

  # Test error for invalid file
  expect_error(load_data(region = "br", file = "invalid_file"))

})
