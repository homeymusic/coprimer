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
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))
  expect_equal(nearby_result$path, "LRRRRRR")
  expect_equal(nearby_result$lower_uncertainty, lower_uncertainty)
  expect_equal(nearby_result$upper_uncertainty, upper_uncertainty)
  expect_equal(nearby_result$valid_min, x-lower_uncertainty)
  expect_equal(nearby_result$valid_max, x+upper_uncertainty)

  lower_result  <- first_coprime(x, lower_uncertainty, upper_uncertainty)
  expect_equal(lower_result$valid_min, x - lower_uncertainty)
  expect_equal(lower_result$valid_max, x + upper_uncertainty)
  expect_equal(lower_result$num, 0)
  expect_equal(lower_result$den, 1)
  expect_equal(lower_result$approximation, 0)

})

test_that("nearby_coprime() works with scalar inputs", {
  x <- 0.25
  lower_uncertainty <- 0.02
  upper_uncertainty <- 0.1
  nearby_result <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_result$num, 1)
  expect_equal(nearby_result$den, 4)
  expect_equal(nearby_result$approximation, 1/4)
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))

})

test_that("nearby_coprime() captures input values correctly", {
  x <- 0.25
  lower_uncertainty <- 0.02
  upper_uncertainty <- 0.1
  nearby_result <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_result$x, x)
  expect_equal(nearby_result$lower_uncertainty, lower_uncertainty)
  expect_equal(nearby_result$upper_uncertainty, upper_uncertainty)
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))

})

test_that("nearby_coprime() computes error and valid range correctly", {
  x <- 0.25
  lower_uncertainty <- 0.02
  upper_uncertainty <- 0.1
  nearby_result <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_result$error, 0, tolerance = 0.001)
  expect_equal(nearby_result$valid_min, x - lower_uncertainty)
  expect_equal(nearby_result$valid_max, x + upper_uncertainty)
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))

})

test_that("nearby_coprime() handles vector x", {
  x <- c(0.1, 0.33, 0.75)
  lower_uncertainty <- 0.01
  upper_uncertainty <- 0.1
  nearby_results <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_results$num, c(1, 1, 3))
  expect_equal(nearby_results$den, c(10, 3, 4))
  expect_equal(nearby_results$approximation, c(1/10, 1/3, 3/4))
  expect_equal(nearby_results$thomae, 1/nearby_results$den)
  expect_equal(nearby_results$euclids_orchard_height,
               1/(abs(nearby_results$num) + nearby_results$den))

})

test_that("nearby_coprime() errors when lower_uncertainty is a vector but x and upper_uncertainty are scalars", {
  x <- 0.2
  lower_uncertainty <- c(0.01, 0.05, 0.1)  # Vector
  upper_uncertainty <- 0.1  # Scalar

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

test_that("nearby_coprime() errors when upper_uncertainty is a vector but x and lower_uncertainty are scalars", {
  x <- 0.2
  lower_uncertainty <- 0.01  # Scalar
  upper_uncertainty <- c(0.05, 0.1, 0.2)  # Vector

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "upper_uncertainty must either be of length 1 or match the length of x"
  )
})
test_that("nearby_coprime() correctly applies scalar lower_uncertainty and upper_uncertainty to vector x", {
  x <- c(0.2, 0.3, 0.4)  # Vector
  lower_uncertainty <- 0.01  # Scalar
  upper_uncertainty <- 0.1  # Scalar

  nearby_result <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_result$lower_uncertainty, rep(lower_uncertainty, length(x)))
  expect_equal(nearby_result$upper_uncertainty, rep(upper_uncertainty, length(x)))
  expect_equal(nearby_result$valid_min, x - lower_uncertainty)
  expect_equal(nearby_result$valid_max, x + upper_uncertainty)
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))

})

test_that("nearby_coprime() correctly applies vector lower_uncertainty and upper_uncertainty of same length as x", {
  x <- c(0.2, 0.3, 0.4)  # Vector
  lower_uncertainty <- c(0.01, 0.02, 0.03)  # Vector of same length
  upper_uncertainty <- c(0.1, 0.15, 0.2)  # Vector of same length

  nearby_result <- nearby_coprime(x, lower_uncertainty, upper_uncertainty)

  expect_equal(nearby_result$lower_uncertainty, lower_uncertainty)
  expect_equal(nearby_result$upper_uncertainty, upper_uncertainty)
  expect_equal(nearby_result$valid_min, x - lower_uncertainty)
  expect_equal(nearby_result$valid_max, x + upper_uncertainty)
  expect_equal(nearby_result$thomae,
               1/nearby_result$den)
  expect_equal(nearby_result$euclids_orchard_height,
               1/(abs(nearby_result$num) + nearby_result$den))

})

test_that("nearby_coprime() errors when lower_uncertainty is a vector of incorrect length", {
  x <- c(0.2, 0.3)  # Vector of length 2
  lower_uncertainty <- c(0.01, 0.02, 0.03)  # Vector of length 3 (incorrect)
  upper_uncertainty <- 0.1  # Scalar

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

test_that("nearby_coprime() errors when upper_uncertainty is a vector of incorrect length", {
  x <- c(0.2, 0.3)  # Vector of length 2
  lower_uncertainty <- 0.01  # Scalar
  upper_uncertainty <- c(0.05, 0.1, 0.15)  # Vector of length 3 (incorrect)

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "upper_uncertainty must either be of length 1 or match the length of x"
  )
})

test_that("nearby_coprime() errors when lower_uncertainty and upper_uncertainty have different vector lengths", {
  x <- c(0.2, 0.3, 0.4)  # Vector of length 3
  lower_uncertainty <- c(0.01, 0.02)  # Vector of length 2 (incorrect)
  upper_uncertainty <- c(0.1, 0.15, 0.2)  # Vector of length 3

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

test_that("nearby_coprime() errors when lower_uncertainty and upper_uncertainty have different vector lengths", {
  x <- c(0.2, 0.3, 0.4)  # Vector of length 3
  lower_uncertainty <- c(0.01, 0.02)  # Vector of length 2 (incorrect)
  upper_uncertainty <- c(0.1, 0.15, 0.2)  # Vector of length 3

  expect_error(
    nearby_coprime(x, lower_uncertainty, upper_uncertainty),
    "lower_uncertainty must either be of length 1 or match the length of x"
  )
})

test_that('the number of inputs and outputs match', {
  num_samples  = 8
  num_bins     = 8
  slit_width   = 0.7

  dx           =  slit_width / num_samples
  min_x        = -slit_width/2 + dx
  max_x        =  slit_width/2 - dx
  x_real       = seq(from=min_x, to=max_x, by=dx)
  sigma_x_lt   = x_real - min_x
  sigma_x_gt   = max_x - x_real
  x = nearby_coprime(x_real, sigma_x_lt, sigma_x_gt)
  expect_equal(length(x_real), nrow(x))
  expect_equal(x$lower_uncertainty, rev(x$upper_uncertainty))
})

test_that('the number of inputs and outputs match', {
  num_samples  = 101
  num_bins     = 101
  slit_width   = 1

  dx           =  slit_width / num_samples
  min_x        = -slit_width/2 + dx
  max_x        =  slit_width/2 - dx
  x_real       = seq(from=min_x, to=max_x, by=dx)
  sigma_x_lt   = x_real - min_x
  sigma_x_gt   = max_x - x_real
  x = nearby_coprime(x_real, sigma_x_lt, sigma_x_gt)
  expect_equal(length(x_real), nrow(x))
  expect_equal(x$lower_uncertainty, rev(x$upper_uncertainty))
})

test_that('the number of inputs and outputs match', {
  num_samples  = 101
  num_bins     = 101
  slit_width   = 3.8

  dx           =  slit_width / num_samples
  min_x        = -slit_width/2 + dx
  max_x        =  slit_width/2 - dx
  x_real       = seq(from=min_x, to=max_x, by=dx)
  sigma_x_lt   = x_real - min_x
  sigma_x_gt   = max_x - x_real
  x = nearby_coprime(x_real, sigma_x_lt, sigma_x_gt)
  expect_equal(length(x_real), nrow(x))
  expect_equal(x$lower_uncertainty, rev(x$upper_uncertainty))
})

test_that('inifinty limits are handled', {
  r = nearby_coprime(3/4, -1/0, 1/0)
  expect_equal(r$approximation, 1)
  expect_equal(r$lower_uncertainty, -Inf)
  expect_equal(r$upper_uncertainty, Inf)
  expect_equal(r$valid_min, -Inf)
  expect_equal(r$valid_max, Inf)
})

test_that('inifinty values are handled', {
  expect_error(nearby_coprime(Inf, -1, 1),
               'x must not be positive or negative infinity.')
})

test_that('the number of inputs and outputs match for momentum', {
  num_samples  = 101
  num_bins     = 101
  slit_width   = 3.8

  dx           =  slit_width / num_samples
  min_x        = -slit_width/2 + dx
  max_x        =  slit_width/2 - dx
  x_real       = seq(from=min_x, to=max_x, by=dx)
  lhd_x   = x_real + slit_width/2
  rhd_x   = slit_width/2 - x_real
  x = nearby_coprime(x_real, lhd_x, rhd_x)

  lhd_p   = 1 / lhd_x
  rhd_p   = 1 / rhd_x
  p_real  = lhd_p - rhd_p
  p = coprimer::nearby_coprime(p_real, lhd_p, rhd_p)

  expect_equal(length(p_real), nrow(p))
  expect_equal(p$lower_uncertainty, rev(p$upper_uncertainty))
})

test_that('valid min and valid max are correct', {
  slit_size      = 1
  particle_count = 11

  dx           = slit_size/particle_count

  min_x        = -slit_size / 2
  max_x        =  slit_size / 2

  x_real       = seq(from=min_x + dx, to=max_x-dx, by=dx)
  lhd_x        = x_real - min_x
  rhd_x        = max_x - x_real
  x = coprimer::nearby_coprime(x_real, lhd_x, rhd_x)

  expect_equal(length(unique(x$approximation)), 3)
  expect_equal(x$lower_uncertainty, lhd_x)
  expect_equal(x$valid_min, x_real - lhd_x)
  expect_equal(x$upper_uncertainty, rhd_x)
  expect_equal(x$valid_max, x_real + rhd_x)
})
test_that('we get interesting values as we change uncertainty tradeoff left and right', {
  samples = 10000
  lhd_x = 1:samples/samples
  rhd_x = rev(lhd_x)
  x = coprimer::nearby_coprime(rep(pi,samples), lhd_x, rhd_x)

  expect_equal(length(unique(x$approximation)), 21)
  expect_equal(x$lower_uncertainty, lhd_x)
  expect_equal(x$valid_min, pi - lhd_x)
  expect_equal(x$upper_uncertainty, rhd_x)
  expect_equal(x$valid_max, pi + rhd_x)
})
test_that('we get interesting values as we change uncertainty', {
  samples = 10000
  lhd_x = 1:samples/samples
  rhd_x = lhd_x
  x = coprimer::nearby_coprime(rep(pi,samples), lhd_x, rhd_x)

  expect_equal(length(unique(x$approximation)), 6)
  expect_equal(x$lower_uncertainty, lhd_x)
  expect_equal(x$valid_min, pi - lhd_x)
  expect_equal(x$upper_uncertainty, rhd_x)
  expect_equal(x$valid_max, pi + rhd_x)
})
