# Test with scalar x and scalar uncertainties
test_that("first_coprime handles zero", {
  x <- 0.001
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, 0)
  expect_equal(result$den, 1)
  expect_equal(result$depth, 0)
  expect_equal(result$path, "")
  expect_equal(result$lower_uncertainty, 0.08)
  expect_equal(result$upper_uncertainty, 0.08)
})

# Test with scalar x and scalar uncertainties
test_that("first_coprime handles positive scalar inputs correctly", {
  x <- 1.667
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "RRLR")
  expect_equal(result$lower_uncertainty, 0.08)
  expect_equal(result$upper_uncertainty, 0.08)
})

# Test with scalar x and scalar uncertainties
test_that("first_coprime handles negative scalar inputs correctly", {
  x <- -1.667
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, -5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "LLRL")
  expect_equal(result$lower_uncertainty, 0.08)
  expect_equal(result$upper_uncertainty, 0.08)
})

# Test with vector x and scalar uncertainties
test_that("first_coprime handles vector x with scalar uncertainties", {
  x <- c(1.667, sqrt(2))
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, c(5, 7))
  expect_equal(result$den, c(3, 5))
  expect_equal(result$depth, c(4, 5))
  expect_equal(result$path, c("RRLR", "RRLLR"))
})

# Test with vector x and vector uncertainties
test_that("first_coprime handles vector x with vector uncertainties", {
  x <- c(1.667, sqrt(2))
  lower_uncertainty <- c(0.067, 0.2)
  upper_uncertainty <- c(0.133, 0.4)
  result <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(result$num, c(5, 3))
  expect_equal(result$den, c(3, 2))
  expect_equal(result$depth, c(4, 3))
  expect_equal(result$path, c("RRLR", "RRL"))
  expect_equal(result$valid_min, x - lower_uncertainty)
  expect_equal(result$valid_max, x + upper_uncertainty)
})

# Test for mismatched vector lengths
test_that("first_coprime errors on mismatched vector lengths", {
  x <- c(1.667, sqrt(2))
  lower_uncertainty <- c(0.067, 0.068, 0.065)
  upper_uncertainty <- c(0.133, 0.4)
  expect_error(
    first_coprime(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

# Test for invalid uncertainties
test_that("first_coprime errors on invalid uncertainties", {
  x <- c(1.667, sqrt(2))

  # Invalid lower_uncertainty (negative value)
  lower_uncertainty <- c(-2, 0.2)
  upper_uncertainty <- c(0.133, 0.4)
  expect_error(
    first_coprime(x, lower_uncertainty, upper_uncertainty),
    regexp = "x\\[0\\] = 1\\.667.* must be greater than or equal to valid_min\\[0\\] = 3\\.667.*"
  )
})

# Test for depth correctness
test_that("depth_cpp computes correct values", {
  x <- 1.667
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "RRLR")

  x <- sqrt(2)
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, 7)
  expect_equal(result$den, 5)
  expect_equal(result$depth, 5)
  expect_equal(result$path, "RRLLR")
})

# Test for depth correctness of negative values
test_that("depth_cpp computes correct values for negatives", {
  x <- -1.667
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, -5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "LLRL")

  x <- -sqrt(2)
  result <- first_coprime(x, 0.08, 0.08)
  expect_equal(result$num, -7)
  expect_equal(result$den, 5)
  expect_equal(result$depth, 5)
  expect_equal(result$path, "LLRRL")
})

# Test close to 0.5
test_that("close to 0.5 returns 1/2 with symmetrical uncertainty", {
  x <- 0.49
  uncertainty <- 0.03
  result <- first_coprime(x, uncertainty, uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
  expect_equal(result$path, "RL")
  expect_equal(result$depth, 2)
})

# Test close to -0.5
test_that("close to -0.5 returns 1/2 with symmetrical uncertainty", {
  x <- -0.49
  uncertainty <- 0.03
  result <- first_coprime(x, uncertainty, uncertainty)
  expect_equal(result$num, -1)
  expect_equal(result$den, 2)
  expect_equal(result$path, "LR")
  expect_equal(result$depth, 2)
})

# Test close to 0.5 with asymmetrical uncertainty
test_that("close to 0.5 returns 1/2 with asymmetrical uncertainty", {
  x <- 0.49
  lower_uncertainty <- 0.04
  upper_uncertainty <- 0.02
  result <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})

# Test close to -0.5 with asymmetrical uncertainty
test_that("close to -0.5 returns 1/2 with asymmetrical uncertainty", {
  x <- -0.49
  lower_uncertainty <- 0.04
  upper_uncertainty <- 0.02
  result <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(result$num, -1)
  expect_equal(result$den, 2)
})

# Test for errors when uncertainties are invalid
test_that("error with uncertainties out of bounds", {
  x <- 0.49
  lower_uncertainty <- -0.04
  upper_uncertainty <- 0.02
  expect_error(
    first_coprime(x, lower_uncertainty, upper_uncertainty),
    regexp = "x\\[0\\] = 0\\.490.* must be greater than or equal to valid_min\\[0\\] = 0\\.530.*"
  )
})

# Test that result$num and result$den are valid integers
test_that("result$num can be any integer and result$den must be a positive integer", {
  x <- 0.49
  uncertainty <- 0.03
  result <- first_coprime(x, uncertainty, uncertainty)
  expect_true(is.integer(result$num))
  expect_true(result$num > 0)
  expect_true(is.integer(result$den))
  expect_true(result$den > 0)
})

# Test edge case for x = 1
test_that("x = 1 returns 1/1", {
  x <- 1
  uncertainty <- 0.01
  result <- first_coprime(x, uncertainty, uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)
})

# Test redundancy
test_that("the redundnacy measure returns expected values", {
  x <- c(0,1,1/2,1/3,5/3,6/7)
  x = c(x,-x)
  expected = c(1,0.5,1/3,1/4,1/8,1/13)
  expected = c(expected, expected)
  uncertainty <- 0.01
  result <- first_coprime(x, uncertainty, uncertainty)
  expect_equal(result$redundancy, expected)
})
test_that('paths are symmetrical around 0 and 1', {
  r = first_coprime(106/333, 0.000001, 0.000001)
  expect_equal(r$path,"RLLLRRRRRRRLLLLLLLLLLLLLL")
  r = first_coprime(-106/333, 0.000001, 0.000001)
  expect_equal(r$path,"LRRRLLLLLLLRRRRRRRRRRRRRR")

  r = first_coprime(333/106, 0.000001, 0.000001)
  expect_equal(r$path,"RRRRLLLLLLLRRRRRRRRRRRRRR")
  r = first_coprime(-333/106, 0.000001, 0.000001)
  expect_equal(r$path,"LLLLRRRRRRRLLLLLLLLLLLLLL")

})
