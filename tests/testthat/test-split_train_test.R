# Mock data for testing
create_mock_data <- function() {
  data.frame(
    season = c(rep('2022-2023', 10), rep('2023-2024', 10)),
    round = rep(1:10, 2),
    team_name = rep(c('Team A', 'Team B', 'Team C', 'Team A', 'Team B', 'Team C', 'Team A', 'Team B', 'Team C', 'Team A'), 2),
    opponent = rep(c('Team X', 'Team Y', 'Team Z', 'Team X', 'Team Y', 'Team Z', 'Team X', 'Team Y', 'Team Z', 'Team X'), 2),
    venue = rep(c('Home', 'Away'), 10),
    some_other_col = runif(20)
  )
}

# Test suite
describe("split_train_test function", {
  # Setup mock data before each test
  mock_df <- create_mock_data()

  test_that("function returns a list with train and test data frames", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)

    expect_type(result, "list")
    expect_true(all(c("train", "test") %in% names(result)))
    expect_s3_class(result$train, "data.frame")
    expect_s3_class(result$test, "data.frame")
  })

  test_that("training data contains only specified rounds", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)

    expect_true(all(result$train$round %in% 1:5))
    expect_true(all(result$test$round %in% 5:10))
  })

  test_that("only home venue data is retained", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)

    expect_true(all(result$train$venue == 1))
    expect_true(all(result$test$venue == 1))
  })

  test_that("only specified season is retained", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)
    print(result$train$season)
    print(result$test$season)
    expect_true(all(result$train$season == '2022-2023'))
    expect_true(all(result$test$season == '2022-2023'))
  })

  test_that("team indexing works correctly", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)

    expect_true(all(!is.na(result$train$team_name_idx)))
    expect_true(all(!is.na(result$train$opponent_idx)))
    expect_true(all(!is.na(result$test$team_name_idx)))
    expect_true(all(!is.na(result$test$opponent_idx)))
  })

  test_that("function handles different round specifications", {
    # Test with different round values
    expect_no_error(split_train_test(mock_df, season = '2022-2023', rounds_train = 3))
    expect_no_error(split_train_test(mock_df, season = '2022-2023', rounds_train = 7))
  })

  test_that("function throws error for invalid season", {
    expect_error(
      split_train_test(mock_df, season = '2021-2022', rounds_train = 5),
      "should be one of"
    )
  })

  test_that("function throws error for invalid rounds", {
    expect_error(
      split_train_test(mock_df, season = '2022-2023', rounds_train = 15)
    )
  })

  test_that("returned dataframes preserve other columns", {
    result <- split_train_test(mock_df, season = '2022-2023', rounds_train = 5)

    expect_true("some_other_col" %in% names(result$train))
    expect_true("some_other_col" %in% names(result$test))
  })
})
