# Test with scalar x and scalar uncertainties
test_that("nearby_coprime behaves as expected", {
  x <- -0.1

  lower_uncertainty = 0.05
  upper_uncertainty = 0.0
  lower_result  <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(lower_result$valid_min, x - lower_uncertainty)
  expect_equal(lower_result$valid_max, x + upper_uncertainty)
  expect_equal(lower_result$num, -1)
  expect_equal(lower_result$den, 7)
  expect_equal(lower_result$approximation, -1/7)

  lower_uncertainty = 0.0
  upper_uncertainty = 0.6
  upper_result  <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(upper_result$valid_min, x - lower_uncertainty)
  expect_equal(upper_result$valid_max, x + upper_uncertainty)
  expect_equal(upper_result$num, 0)
  expect_equal(upper_result$den, 1)
  expect_equal(upper_result$approximation, 0)

  expect_true(abs(lower_result$approximation - x) < abs(upper_result$approximation - x))

  lower_uncertainty = 0.05
  upper_uncertainty = 0.6

  nearby_result = nearby_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(nearby_result$num, -1)
  expect_equal(nearby_result$den,  7)
  expect_equal(nearby_result$approximation, -1/7)
  expect_equal(nearby_result$x, -0.1)
  expect_equal(nearby_result$error, -0.04285714, tolerance = 0.001)
  expect_equal(nearby_result$depth, 7)
  expect_equal(nearby_result$path, "LRRRRRR")
  expect_equal(nearby_result$lower_uncertainty, lower_uncertainty)
  expect_equal(nearby_result$upper_uncertainty, upper_uncertainty)
  expect_equal(nearby_result$valid_min, x-lower_uncertainty)
  expect_equal(nearby_result$valid_max, x+upper_uncertainty)


})
