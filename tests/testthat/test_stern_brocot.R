GABOR_UNCERTAINTY <- 1 / (4 * pi)

expected_stern_brocot_columns <- c(
  "num",
  "den",
  "approximation",
  "error",
  "depth",
  "path",
  "path_id",
  "x",
  "lower_uncertainty",
  "upper_uncertainty",
  "valid_min",
  "valid_max"
)

# Test with scalar x and scalar uncertainties
test_that("stern_brocot handles scalar inputs correctly", {
  x <- 1.667
  result <- stern_brocot(x, GABOR_UNCERTAINTY, GABOR_UNCERTAINTY)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "1101")
  expect_equal(result$path_id, 13)
  expect_equal(result$lower_uncertainty, GABOR_UNCERTAINTY)
  expect_equal(result$upper_uncertainty, GABOR_UNCERTAINTY)
})

# Test with vector x and scalar uncertainties
test_that("stern_brocot handles vector x with scalar uncertainties", {
  x <- c(1.667, sqrt(2))
  result <- stern_brocot(x, GABOR_UNCERTAINTY, GABOR_UNCERTAINTY)
  expect_equal(result$num, c(5, 7))
  expect_equal(result$den, c(3, 5))
  expect_equal(result$depth, c(4, 5))
  expect_equal(result$path, c("1101", "11001"))
  expect_equal(result$path_id, c(13, 25))
})

# Test with vector x and vector uncertainties
test_that("stern_brocot handles vector x with vector uncertainties", {
  x <- c(1.667, sqrt(2))
  lower_uncertainty <- c(0.067, 0.2)
  upper_uncertainty <- c(0.133, 0.4)
  result <- stern_brocot(x, lower_uncertainty, upper_uncertainty)
  expect_equal(result$num, c(5, 3))
  expect_equal(result$den, c(3, 2))
  expect_equal(result$depth, c(4, 3))
  expect_equal(result$path, c("1101", "110"))
  expect_equal(result$path_id, c(13, 6))
  expect_equal(result$valid_min, x - lower_uncertainty)
  expect_equal(result$valid_max, x + upper_uncertainty)
})

# Test for mismatched vector lengths
test_that("stern_brocot errors on mismatched vector lengths", {
  x <- c(1.667, sqrt(2))
  lower_uncertainty <- c(0.067, 0.068, 0.065)
  upper_uncertainty <- c(0.133, 0.4)
  expect_error(
    stern_brocot(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

# Test for invalid uncertainties
test_that("stern_brocot errors on invalid uncertainties", {
  x <- c(1.667, sqrt(2))

  # Invalid lower_uncertainty (negative value)
  lower_uncertainty <- c(-2, 0.2)
  upper_uncertainty <- c(0.133, 0.4)
  expect_error(
    stern_brocot(x, lower_uncertainty, upper_uncertainty),
    "STOP: x must be greater than valid_min"
  )

  # Valid_min > x
  lower_uncertainty <- c(2, 0.2)
  upper_uncertainty <- c(1.8, 0.4)
  expect_error(
    stern_brocot(x, lower_uncertainty, upper_uncertainty),
    "STOP: valid_min must be greater than 0"
  )
})

# Test for depth correctness
test_that("depth_cpp computes correct values", {
  x <- 1.667
  result <- stern_brocot(x, GABOR_UNCERTAINTY, GABOR_UNCERTAINTY)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "1101")
  expect_equal(result$path_id, 13)

  x <- sqrt(2)
  result <- stern_brocot(x, GABOR_UNCERTAINTY, GABOR_UNCERTAINTY)
  expect_equal(result$num, 7)
  expect_equal(result$den, 5)
  expect_equal(result$depth, 5)
  expect_equal(result$path, "11001")
  expect_equal(result$path_id, 25)
})

# Test close to 0.5
test_that("close to 0.5 returns 1/2 with symmetrical uncertainty", {
  x <- 0.49
  uncertainty <- 0.03
  result <- stern_brocot(x, uncertainty, uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
  expect_equal(result$path, "10")
  expect_equal(result$path_id, 2)
  expect_equal(result$depth, 2)
})

# Test close to 0.5 with asymmetrical uncertainty
test_that("close to 0.5 returns 1/2 with asymmetrical uncertainty", {
  x <- 0.49
  lower_uncertainty <- 0.04
  upper_uncertainty <- 0.02
  result <- stern_brocot(x, lower_uncertainty, upper_uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})

# Test for errors when uncertainties are invalid
test_that("error with uncertainties out of bounds", {
  x <- 0.49
  lower_uncertainty <- -0.04
  upper_uncertainty <- 0.02
  expect_error(
    stern_brocot(x, lower_uncertainty, upper_uncertainty),
    "STOP: x must be greater than valid_min"
  )
})

# Test that result$num and result$den are valid integers
test_that("result$num can be any integer and result$den must be a positive integer", {
  x <- 0.49
  uncertainty <- 0.03
  result <- stern_brocot(x, uncertainty, uncertainty)
  expect_true(is.integer(result$num))
  expect_true(result$num > 0)
  expect_true(is.integer(result$den))
  expect_true(result$den > 0)
})

# Test edge case for x = 1
test_that("x = 1 returns 1/1", {
  x <- 1
  uncertainty <- 0.01
  result <- stern_brocot(x, uncertainty, uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)
})
