#include <Rcpp.h>
using namespace Rcpp;

inline double round_to_precision(double value, int precision = 15) {
  double scale = std::pow(10.0, precision);
  return std::round(value * scale) / scale;
}

inline std::string as_string_cpp(const std::vector<int>& bits) {
  std::ostringstream oss;
  for (int bit : bits) {
    oss << bit;
  }
  return oss.str();
}

inline int as_integer_cpp(const std::vector<int>& bits) {
  int result = 0;
  for (size_t i = 0; i < bits.size(); ++i) {
    result = (result << 1) | bits[i]; // Shift left and add current bit
  }
  return result;
}

//' stern_brocot
//'
//' Approximate a real number as a coprime rational fraction using the Stern-Brocot tree
//'
//' This function approximates a real number as a coprime rational fraction
//' using the Stern-Brocot tree. It supports specifying an uncertainty to determine
//' how close the approximation should be to the real number.
//'
//' The method is inspired by the algorithms described in:
//'
//' 1. Stern, M. (1858). Ueber eine zahlentheoretische Funktion. *Journal für die reine und angewandte Mathematik, 55*, 193–220.
//' 2. Brocot, A. (1862). Calcul des rouages par approximation: Nouvelle méthode. *A. Brocot.*
//' 3. Graham, R. L., Knuth, D. E., & Patashnik, O. (1994). *Concrete mathematics* (2nd ed., pp. 115–123). Addison-Wesley.
//' 4. Forišek, M. (2007). Approximating rational numbers by fractions. In *International Conference on Fun with Algorithms* (pp. 156–165). Berlin, Heidelberg: Springer Berlin Heidelberg.
//' 5. Stolzenburg, F. (2015). Harmony perception by periodicity detection. *Journal of Mathematics and Music, 9*(3), 215–238.
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
//'   - `path`: The path taken in the Stern-Brocot tree as a binary string.
//'   - `path_id`: The binary path represented as an integer.
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

  // Check vector lengths
  int n = x.size();
  if (lower_uncertainty.size() != 1 && lower_uncertainty.size() != n) {
    stop("lower_uncertainty must either be of length 1 or match the length of x");
  }
  if (upper_uncertainty.size() != 1 && upper_uncertainty.size() != n) {
    stop("upper_uncertainty must either be of length 1 or match the length of x");
  }

  // Expand scalar uncertainties to match the length of x
  NumericVector lower = lower_uncertainty.size() == 1 ? NumericVector(n, lower_uncertainty[0]) : lower_uncertainty;
  NumericVector upper = upper_uncertainty.size() == 1 ? NumericVector(n, upper_uncertainty[0]) : upper_uncertainty;

  // Compute valid_min and valid_max
  NumericVector valid_min = x - lower;
  NumericVector valid_max = x + upper;

  // Check validity of computed bounds
  for (int i = 0; i < n; i++) {
    if (valid_min[i] <= 0) {
      stop("STOP: valid_min must be greater than 0");
    }
    if (x[i] <= valid_min[i]) {
      stop("STOP: x must be greater than valid_min");
    }
    if (valid_max[i] <= x[i]) {
      stop("STOP: x must be less than valid_max");
    }
  }

  // Initialize result vectors
  IntegerVector nums(n), dens(n), depths(n), path_ids(n);
  NumericVector approximations(n), errors(n);
  CharacterVector paths(n);

  // Compute results for each element in x
  for (int i = 0; i < n; i++) {
    std::vector<int> path = {1};
    int left_num = 0, left_den = 1;
    int mediant_num = 1, mediant_den = 1;
    int right_num = 1, right_den = 0;

    double approximation = 1.0;

    while ((approximation < valid_min[i]) || (approximation > valid_max[i])) {
      if (approximation < valid_min[i]) {
        left_num = mediant_num;
        left_den = mediant_den;
        path.push_back(1);
      } else {
        right_num = mediant_num;
        right_den = mediant_den;
        path.push_back(0);
      }

      mediant_num = left_num + right_num;
      mediant_den = left_den + right_den;
      approximation = static_cast<double>(mediant_num) / mediant_den;
    }

    if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

    // Store results
    nums[i] = mediant_num;
    dens[i] = mediant_den;
    approximations[i] = approximation;
    errors[i] = round_to_precision(approximation - x[i]);
    depths[i] = path.size();
    paths[i] = as_string_cpp(path);
    path_ids[i] = as_integer_cpp(path);
  }

  // Return results as a DataFrame
  return DataFrame::create(
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["error"] = errors,
    _["depth"] = depths,
    _["path"] = paths,
    _["path_id"] = path_ids,
    _["x"] = x,
    _["lower_uncertainty"] = lower,
    _["upper_uncertainty"] = upper,
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max
  );
}
