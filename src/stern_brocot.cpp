#include <Rcpp.h>
using namespace Rcpp;

inline double round_to_precision(double value, int precision = 15) {
  double scale = std::pow(10.0, precision);
  return std::round(value * scale) / scale;
}

//' stern_brocot
//'
//' Approximate a real number as a coprime rational fraction using the Stern-Brocot tree
//'
//' @param x A numeric vector of values to approximate as fractions.
//' @param lower_uncertainty A numeric vector (or scalar) specifying the lower uncertainty bound.
//' @param upper_uncertainty A numeric vector (or scalar) specifying the upper uncertainty bound.
//'
//' @return A data frame with the following columns:
//'   - `num`: The numerator of the approximated fraction (an integer).
//'   - `den`: The denominator of the approximated fraction (a natural number > 0).
//'   - `approximation`: The value of the fraction `num / den`.
//'   - `error`: The difference between the approximation and the original value of `x`.
//'   - `depth`: The depth of the Stern-Brocot tree traversal.
//'   - `path`: The path taken in the Stern-Brocot tree as a string of 'L' (left) and 'R' (right).
//'   - `x`: The original value of `x`.
//'   - `lower_uncertainty`: The lower uncertainty.
//'   - `upper_uncertainty`: The upper uncertainty.
//'   - `valid_min`: The lower bound of the uncertainty range (`x - lower_uncertainty`).
//'   - `valid_max`: The upper bound of the uncertainty range (`x + upper_uncertainty`).
//'
// [[Rcpp::export]]
DataFrame stern_brocot(const NumericVector x,
                       const NumericVector lower_uncertainty,
                       const NumericVector upper_uncertainty) {

  int n = x.size();
  if (lower_uncertainty.size() != 1 && lower_uncertainty.size() != n) {
    stop("lower_uncertainty must either be of length 1 or match the length of x");
  }
  if (upper_uncertainty.size() != 1 && upper_uncertainty.size() != n) {
    stop("upper_uncertainty must either be of length 1 or match the length of x");
  }

  NumericVector lower = lower_uncertainty.size() == 1 ? NumericVector(n, lower_uncertainty[0]) : lower_uncertainty;
  NumericVector upper = upper_uncertainty.size() == 1 ? NumericVector(n, upper_uncertainty[0]) : upper_uncertainty;

  NumericVector valid_min = x - lower;
  NumericVector valid_max = x + upper;

  for (int i = 0; i < n; i++) {
    if (x[i] < valid_min[i]) {
      stop("STOP: x[%d] = %f must be greater than or equal to valid_min[%d] = %f", i, x[i], i, valid_min[i]);
    }
    if (valid_max[i] < x[i]) {
      stop("STOP: x[%d] = %f must be less than or equal to valid_max[%d] = %f", i, x[i], i, valid_max[i]);
    }
  }

  IntegerVector nums(n), dens(n), depths(n);
  NumericVector approximations(n), errors(n);
  CharacterVector paths(n);

  for (int i = 0; i < n; i++) {
    std::vector<char> path;
    int left_num = -1, left_den = 0;
    int mediant_num = 0, mediant_den = 1;
    int right_num = 1, right_den = 0;

    double approximation = 0.0;

    while ((approximation < valid_min[i]) || (approximation > valid_max[i])) {
      if (approximation < valid_min[i]) {
        left_num = mediant_num;
        left_den = mediant_den;
        path.push_back('R'); // Move right
      } else {
        right_num = mediant_num;
        right_den = mediant_den;
        path.push_back('L'); // Move left
      }

      mediant_num = left_num + right_num;
      mediant_den = left_den + right_den;
      approximation = static_cast<double>(mediant_num) / mediant_den;
    }

    if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

    nums[i] = mediant_num;
    dens[i] = mediant_den;
    approximations[i] = approximation;
    errors[i] = round_to_precision(approximation - x[i]);
    depths[i] = path.size();
    paths[i] = std::string(path.begin(), path.end());
  }

  return DataFrame::create(
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["x"] = x,
    _["error"] = errors,
    _["depth"] = depths,
    _["path"] = paths,
    _["lower_uncertainty"] = lower,
    _["upper_uncertainty"] = upper,
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max
  );
}
