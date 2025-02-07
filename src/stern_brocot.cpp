#include <Rcpp.h>
#include <queue>
#include <tuple>

using namespace Rcpp;
using namespace std;

// Function to insert mediants into a linked list in-place
void insert_mediants(list<int>& nums,
                     list<int>& dens,
                     list<string>& labels,
                     list<int>& levels,
                     list<string>& left_parents,
                     list<string>& right_parents,
                     int current_depth, int max_depth) {
  if (current_depth > max_depth) return;  // Stop when max depth is reached

  auto num_it = nums.begin();
  auto den_it = dens.begin();
  auto label_it = labels.begin();
  auto level_it = levels.begin();
  auto left_parent_it = left_parents.begin();
  auto right_parent_it = right_parents.begin();

  auto next_num_it = next(num_it);
  auto next_den_it = next(den_it);
  auto next_label_it = next(label_it);
  auto next_level_it = next(level_it);
  auto next_left_parent_it = next(left_parent_it);
  auto next_right_parent_it = next(right_parent_it);

  while (next_num_it != nums.end()) {
    // Compute mediant
    int mediant_num = *num_it + *next_num_it;
    int mediant_den = *den_it + *next_den_it;

    // Insert mediant between current and next fraction
    next_left_parent_it = left_parents.insert(next_left_parent_it, *label_it);
    next_right_parent_it = right_parents.insert(next_right_parent_it, *next_label_it);
    next_num_it = nums.insert(next_num_it, mediant_num);
    next_den_it = dens.insert(next_den_it, mediant_den);
    next_label_it = labels.insert(next_label_it, std::to_string(mediant_num) + "/" + std::to_string(mediant_den));
    next_level_it = levels.insert(next_level_it, current_depth);

    // Move iterators forward (skip the inserted mediant)
    advance(next_num_it, 1);
    advance(next_den_it, 1);
    advance(next_label_it, 1);
    advance(next_level_it, 1);
    advance(next_left_parent_it, 1);
    advance(next_right_parent_it, 1);

    // Move to the next original fraction
    num_it = next_num_it;
    den_it = next_den_it;
    label_it = next_label_it;
    level_it = next_level_it;
    left_parent_it = next_left_parent_it;
    right_parent_it = next_right_parent_it;

    next_num_it = next(num_it);
    next_den_it = next(den_it);
    next_label_it = next(label_it);
    next_level_it = next(level_it);
    next_left_parent_it = next(left_parent_it);
    next_right_parent_it = next(right_parent_it);
  }

  // Recur for the next depth level
  insert_mediants(nums, dens, labels, levels, left_parents, right_parents, current_depth + 1, max_depth);
}

//' stern_brocot_tree
//'
//' Generate the Stern-Brocot tree up to a given depth using `std::list`.
//'
//' @param x The depth of the tree to generate.
//'
//' @return A data frame containing:
//'   - `num`: Numerator of the fraction.
//'   - `den`: Denominator of the fraction.
//'   - `depth`: The depth level in the Stern-Brocot tree.
//'
// [[Rcpp::export]]
DataFrame stern_brocot_tree(const int x) {
  if (x < 0) stop("Depth must be non-negative.");

  // Initialize the list with pre-level
  list<int> nums             = {-1, 0, 1};
  list<int> dens             = { 0, 1, 0};
  list<string> labels        = {"-1/0","0/1","1/0"};
  list<int> levels           = {-1,-0,-1};
  list<string> left_parents  = {"","-1/0",""};
  list<string> right_parents = {"","1/0",""};

  insert_mediants(nums, dens, labels, levels, left_parents, right_parents, 1, x);

  return DataFrame::create(
    _["num"] = nums,
    _["den"] = dens,
    _["label"] = labels,
    _["depth"] = levels,
    _["left_parent"] = left_parents,
    _["right_parent"] = right_parents
  );
}


inline double round_to_precision(double value, int precision = 15) {
  double scale = std::pow(10.0, precision);
  return std::round(value * scale) / scale;
}

//' first_coprime
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
DataFrame first_coprime(const NumericVector x,
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
    if (!(valid_min[i] <= x[i])) {
      stop("STOP: x[%d] = %f must be greater than or equal to valid_min[%d] = %f", i, x[i], i, valid_min[i]);
    }
    if (!(x[i] <= valid_max[i])) {
      stop("STOP: x[%d] = %f must be less than or equal to valid_max[%d] = %f", i, x[i], i, valid_max[i]);
    }
  }

  IntegerVector nums(n), dens(n), depths(n);
  NumericVector approximations(n), errors(n), redundancy(n);
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

    nums[i] = mediant_num;
    dens[i] = mediant_den;
    approximations[i] = approximation;
    errors[i] = round_to_precision(approximation - x[i]);
    redundancy[i] = 1.0 / (abs(mediant_num) + abs(mediant_den));
    depths[i] = path.size();
    paths[i] = std::string(path.begin(), path.end());
  }

  return DataFrame::create(
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["x"] = x,
    _["error"] = errors,
    _["redundancy"] = redundancy,
    _["depth"] = depths,
    _["path"] = paths,
    _["lower_uncertainty"] = lower,
    _["upper_uncertainty"] = upper,
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max
  );
}

//' Find the nearest coprime fraction within uncertainty bounds
//'
//' This function finds the best rational approximation (coprime fraction) to a given value `x`
//' while considering a specified lower and upper uncertainty range. It computes two coprime
//' fractions: one using the lower uncertainty and one using the upper uncertainty, and returns
//' the one closest to `x`.
//'
//' @param x A numeric vector of values for which to find the nearest coprime fraction.
//' @param lower_uncertainty A numeric vector or scalar specifying the lower uncertainty range.
//'   If scalar, it is applied to all `x` values.
//' @param upper_uncertainty A numeric vector or scalar specifying the upper uncertainty range.
//'   If scalar, it is applied to all `x` values.
//'
//' @return A data frame containing:
//'   - `num`: The numerator of the coprime fraction.
//'   - `den`: The denominator of the coprime fraction.
//'   - `approximation`: The computed fraction closest to `x`.
//'   - `x`: The original input value(s).
//'   - `error`: The difference between `approximation` and `x`.
//'   - `depth`: The depth of the Stern-Brocot tree used to find the fraction.
//'   - `path`: The Stern-Brocot path to the selected fraction.
//'   - `lower_uncertainty`: The effective lower uncertainty used.
//'   - `upper_uncertainty`: The effective upper uncertainty used.
//'   - `valid_min`: The minimum valid range (`x - lower_uncertainty`).
//'   - `valid_max`: The maximum valid range (`x + upper_uncertainty`).
//'
//' @details
//' The function computes two rational approximations: one using `x - lower_uncertainty` and one
//' using `x + upper_uncertainty`. The fraction whose approximation is closest to `x` is selected.
//' If `lower_uncertainty` or `upper_uncertainty` is a scalar, it is expanded to match the length
//' of `x`. If their lengths are inconsistent, an error is thrown.
//'
//' @examples
//' nearby_coprime(0.2, 0.01, 0.1)
//' nearby_coprime(c(0.2, 0.3), 0.01, c(0.05, 0.1))
//'
// [[Rcpp::export]]
DataFrame nearby_coprime(const NumericVector x,
                         const NumericVector lower_uncertainty,
                         const NumericVector upper_uncertainty) {

  int n = x.size();

  // Ensure lower_uncertainty and upper_uncertainty are either of length 1 or match x
  if (lower_uncertainty.size() != 1 && lower_uncertainty.size() != n) {
    stop("lower_uncertainty must either be of length 1 or match the length of x");
  }
  if (upper_uncertainty.size() != 1 && upper_uncertainty.size() != n) {
    stop("upper_uncertainty must either be of length 1 or match the length of x");
  }

  // Expand scalar uncertainties if necessary
  NumericVector final_lower(n), final_upper(n);
  for (int i = 0; i < n; i++) {
    final_lower[i] = (lower_uncertainty.size() == 1) ? lower_uncertainty[0] : lower_uncertainty[i];
    final_upper[i] = (upper_uncertainty.size() == 1) ? upper_uncertainty[0] : upper_uncertainty[i];
  }

  // Compute min and max valid ranges
  NumericVector valid_min(n), valid_max(n);
  for (int i = 0; i < n; i++) {
    valid_min[i] = x[i] - final_lower[i];
    valid_max[i] = x[i] + final_upper[i];
  }

  // Call first_coprime() with one uncertainty forced to zero
  DataFrame lower_cp = first_coprime(x, final_lower, NumericVector(n, 0.0));
  DataFrame upper_cp = first_coprime(x, NumericVector(n, 0.0), final_upper);

  // Allocate output vectors
  IntegerVector num(n), den(n), depth(n);
  NumericVector approx(n), err(n), redun(n);
  CharacterVector path(n);

  // For each element, choose the fraction whose approximation is closest to x
  for (int i = 0; i < n; i++) {
    double d_lower = std::abs(as<NumericVector>(lower_cp["approximation"])[i] - x[i]);
    double d_upper = std::abs(as<NumericVector>(upper_cp["approximation"])[i] - x[i]);

    if (d_lower <= d_upper) {
      num[i]    = as<IntegerVector>(lower_cp["num"])[i];
      den[i]    = as<IntegerVector>(lower_cp["den"])[i];
      approx[i] = as<NumericVector>(lower_cp["approximation"])[i];
      err[i]    = as<NumericVector>(lower_cp["error"])[i];
      depth[i]  = as<IntegerVector>(lower_cp["depth"])[i];
      path[i]   = as<CharacterVector>(lower_cp["path"])[i];
    } else {
      num[i]    = as<IntegerVector>(upper_cp["num"])[i];
      den[i]    = as<IntegerVector>(upper_cp["den"])[i];
      approx[i] = as<NumericVector>(upper_cp["approximation"])[i];
      err[i]    = as<NumericVector>(upper_cp["error"])[i];
      depth[i]  = as<IntegerVector>(upper_cp["depth"])[i];
      path[i]   = as<CharacterVector>(upper_cp["path"])[i];
    }
    redun[i] = 1.0 / (abs(num[i]) + abs(den[i]));
  }

  return DataFrame::create(
    _["num"] = num,
    _["den"] = den,
    _["approximation"] = approx,
    _["x"] = x,
    _["error"] = err,
    _["redundancy"] = redun,
    _["depth"] = depth,
    _["path"] = path,
    _["lower_uncertainty"] = final_lower,
    _["upper_uncertainty"] = final_upper,
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max
  );
}
