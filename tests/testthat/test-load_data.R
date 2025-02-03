test_that("load_data works with valid inputs", {
  df <- load_data(country = "brazil", league = "brasileirao_a", file = "match_history")
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)  # Ensures data is not empty
})

test_that("load_data correctly handles invalid country", {
  expect_error(load_data(country = "invalid_country", league = "brasileirao_a", file = "match_history"))
})

test_that("load_data correctly handles invalid league", {
  expect_error(load_data(country = "brazil", league = "invalid_league", file = "match_history"))
})

test_that("load_data correctly handles invalid file type", {
  expect_error(load_data(country = "brazil", league = "brasileirao_a", file = "invalid_file"))
})
