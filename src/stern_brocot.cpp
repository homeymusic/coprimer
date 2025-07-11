#include <Rcpp.h>
#include <queue>
#include <tuple>
#include <cmath>
#include <vector>
#include <string>

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

// -------------------------------------------------------------------------
// Helper: Validate and expand uncertainties and compute valid ranges
// -------------------------------------------------------------------------
List prepare_uncertainties(const NumericVector& x,
                           const NumericVector& lower_uncertainty,
                           const NumericVector& upper_uncertainty) {
  int n = x.size();

  if(lower_uncertainty.size() != 1 && lower_uncertainty.size() != n)
    stop("lower_uncertainty must either be of length 1 or match the length of x");
  if(upper_uncertainty.size() != 1 && upper_uncertainty.size() != n)
    stop("upper_uncertainty must either be of length 1 or match the length of x");

  NumericVector lower = (lower_uncertainty.size() == 1) ?
  NumericVector(n, lower_uncertainty[0]) : lower_uncertainty;
  NumericVector upper = (upper_uncertainty.size() == 1) ?
  NumericVector(n, upper_uncertainty[0]) : upper_uncertainty;

  NumericVector valid_min = x - lower;
  NumericVector valid_max = x + upper;

  for (int i = 0; i < x.size(); i++) {
    // Handle Inf and -Inf properly
    if (std::isinf(lower[i])) {
      valid_min[i] = R_NegInf;
    } else {
      valid_min[i] = x[i] - lower[i];
    }

    if (std::isinf(upper[i])) {
      valid_max[i] = R_PosInf;
    } else {
      valid_max[i] = x[i] + upper[i];
    }
  }

  for (int i = 0; i < n; i++) {
    if (valid_min[i] > x[i])
      stop("x[%d] = %f must be greater than or equal to valid_min[%d] = %f",
           i, x[i], i, valid_min[i]);
    if (x[i] > valid_max[i])
      stop("x[%d] = %f must be less than or equal to valid_max[%d] = %f",
           i, x[i], i, valid_max[i]);
  }

  return List::create(_["lower"] = lower,
                      _["upper"] = upper,
                      _["valid_min"] = valid_min,
                      _["valid_max"] = valid_max);
}

// -------------------------------------------------------------------------
// Helper structure and function to compute the Stern-Brocot fraction
// -------------------------------------------------------------------------
struct SternBrocotResult {
  int num;
  int den;
  double approximation;
  double error;
  double thomae;
  double euclids_orchard_height;
  int depth;
  std::string path;
};

inline double round_to_precision(double value, int precision = 15) {
  double scale = std::pow(10.0, precision);
  return std::round(value * scale) / scale;
}

SternBrocotResult compute_fraction(double x, double valid_min, double valid_max) {

  if (std::isinf(x)) {
    stop("x must not be positive or negative infinity.");
  }

  valid_min = round_to_precision(valid_min);
  valid_max = round_to_precision(valid_max);

  if (valid_min == valid_max) {
    stop("valid_min cannot equal valid_max ~ valid_min %.20f, valid_max %.20f",
         valid_min, valid_max);
  }

  std::vector<char> path;
  int left_num = -1, left_den = 0;
  int mediant_num = 0, mediant_den = 1;
  int right_num = 1, right_den = 0;
  int depth = 0;
  double approximation = 0.0;
  const double max_iterations = std::abs(x) + 10000;  // Circuit breaker

  while ((approximation < valid_min) || (approximation > valid_max)) {
    depth++;
    if (depth >= max_iterations){
      stop("x %.20f valid_max - valid_min %.20f ~ depth %d >= max_iterations %d ~ \
             valid_min %.20f, approximation %.20f, valid_max %.20f ~ %s",
           x,
           valid_max - valid_min,
           depth, max_iterations, valid_min, approximation, valid_max,
           std::string(path.begin(),path.end()));
    }
    if (approximation < valid_min) {
      left_num = mediant_num;
      left_den = mediant_den;
      path.push_back('R'); // move right
    } else if (approximation > valid_min) {
      right_num = mediant_num;
      right_den = mediant_den;
      path.push_back('L'); // move left
    }

    mediant_num = left_num + right_num;
    mediant_den = left_den + right_den;
    approximation = round_to_precision(static_cast<double>(mediant_num) / mediant_den);

  }

  SternBrocotResult result;
  result.num = mediant_num;
  result.den = mediant_den;
  result.approximation = approximation;
  result.error = round_to_precision(approximation - x);
  result.depth = path.size();
  result.path = std::string(path.begin(), path.end());
  result.thomae = 1.0 / mediant_den;
  result.euclids_orchard_height = 1.0 / (std::abs(mediant_num) + mediant_den);
  return result;
}

// -------------------------------------------------------------------------
// Helper: Create a consistent DataFrame from computed vectors
// -------------------------------------------------------------------------
DataFrame create_result_dataframe(const IntegerVector &nums,
                                  const IntegerVector &dens,
                                  const NumericVector &approximations,
                                  const NumericVector &x,
                                  const NumericVector &errors,
                                  const NumericVector &thomae,
                                  const NumericVector &euclids_orchard_height,
                                  const IntegerVector &depths,
                                  const CharacterVector &paths,
                                  const NumericVector &final_lower,
                                  const NumericVector &final_upper,
                                  const NumericVector &valid_min,
                                  const NumericVector &valid_max) {
  return DataFrame::create(_["num"] = nums,
                           _["den"] = dens,
                           _["approximation"] = approximations,
                           _["x"] = x,
                           _["error"] = errors,
                           _["thomae"] = thomae,
                           _["euclids_orchard_height"] = euclids_orchard_height,
                           _["depth"] = depths,
                           _["path"] = paths,
                           _["lower_uncertainty"] = final_lower,
                           _["upper_uncertainty"] = final_upper,
                           _["valid_min"] = valid_min,
                           _["valid_max"] = valid_max);
}

// -------------------------------------------------------------------------
// Exported function: first_coprime
// -------------------------------------------------------------------------

//' first_coprime
 //'
 //' Approximate a real number as a coprime rational fraction using the Stern-Brocot tree.
 //'
 //' @param x A numeric vector of values to approximate as fractions.
 //' @param lower_uncertainty A numeric vector (or scalar) specifying the lower uncertainty bound.
 //' @param upper_uncertainty A numeric vector (or scalar) specifying the upper uncertainty bound.
 //' @return A DataFrame with columns: num, den, approximation, x, error, thomae, euclids_orchard_height, depth, path,
 //'         lower_uncertainty, upper_uncertainty, valid_min, valid_max.
 //'
 // [[Rcpp::export]]
 DataFrame first_coprime(const NumericVector x,
                         const NumericVector lower_uncertainty,
                         const NumericVector upper_uncertainty) {
   int n = x.size();
   List uncert = prepare_uncertainties(x, lower_uncertainty, upper_uncertainty);
   NumericVector final_lower = uncert["lower"];
   NumericVector final_upper = uncert["upper"];
   NumericVector valid_min   = uncert["valid_min"];
   NumericVector valid_max   = uncert["valid_max"];

   // Pre-allocate result vectors.
   IntegerVector nums(n), dens(n), depths(n);
   NumericVector approximations(n), errors(n), thomae(n), euclids_orchard_height(n);
   CharacterVector paths(n);

   for (int i = 0; i < n; i++) {
     SternBrocotResult res = compute_fraction(x[i], valid_min[i], valid_max[i]);
     nums[i]           = res.num;
     dens[i]           = res.den;
     approximations[i] = res.approximation;
     errors[i]         = res.error;
     depths[i]         = res.depth;
     paths[i]          = res.path;
     thomae[i]         = res.thomae;
     euclids_orchard_height[i] = res.euclids_orchard_height;
   }

   return create_result_dataframe(nums, dens, approximations, x, errors,
                                  thomae, euclids_orchard_height, depths, paths,
                                  final_lower, final_upper, valid_min, valid_max);
 }

// -------------------------------------------------------------------------
// Exported function: nearby_coprime
// -------------------------------------------------------------------------

//' nearby_coprime
 //'
 //' Find the nearest coprime fraction within uncertainty bounds.
 //'
 //' @param x A numeric vector of values.
 //' @param lower_uncertainty A numeric vector (or scalar) specifying the lower uncertainty bound.
 //' @param upper_uncertainty A numeric vector (or scalar) specifying the upper uncertainty bound.
 //' @return A DataFrame with columns: num, den, approximation, x, error, thomae, euclids_orchard_height, depth, path,
 //'         lower_uncertainty, upper_uncertainty, valid_min, valid_max.
 //'
 // [[Rcpp::export]]
 DataFrame nearby_coprime(const NumericVector x,
                          const NumericVector lower_uncertainty,
                          const NumericVector upper_uncertainty) {
   int n = x.size();
   List uncert = prepare_uncertainties(x, lower_uncertainty, upper_uncertainty);
   NumericVector final_lower = uncert["lower"];
   NumericVector final_upper = uncert["upper"];

   // For reporting, compute overall valid ranges.
   NumericVector valid_min(n), valid_max(n);

   for (int i = 0; i < n; i++) {
     // Handle Inf and -Inf properly
     if (std::isinf(final_lower[i])) {
       valid_min[i] = R_NegInf;
     } else {
       valid_min[i] = x[i] - final_lower[i];
     }

     if (std::isinf(final_upper[i])) {
       valid_max[i] = R_PosInf;
     } else {
       valid_max[i] = x[i] + final_upper[i];
     }
   }



   // Pre-allocate result vectors.
   IntegerVector nums(n), dens(n), depths(n);
   NumericVector approximations(n), errors(n), thomae(n), euclids_orchard_height(n);
   CharacterVector paths(n);

   for (int i = 0; i < n; i++) {
     // Lower candidate: force the fraction to be <= x.
     SternBrocotResult lowerRes = compute_fraction(x[i], valid_min[i], x[i]);
     // Upper candidate: force the fraction to be >= x.
     SternBrocotResult upperRes = compute_fraction(x[i], x[i], valid_max[i]);

     double diffLower = std::abs(lowerRes.approximation - x[i]);
     double diffUpper = std::abs(upperRes.approximation - x[i]);

     SternBrocotResult chosen = (diffLower <= diffUpper) ? lowerRes : upperRes;

     nums[i]           = chosen.num;
     dens[i]           = chosen.den;
     approximations[i] = chosen.approximation;
     errors[i]         = chosen.error;
     depths[i]         = chosen.depth;
     paths[i]          = chosen.path;
     thomae[i]         = chosen.thomae;
     euclids_orchard_height[i] = chosen.euclids_orchard_height;
   }

   return create_result_dataframe(nums, dens, approximations, x, errors,
                                  thomae, euclids_orchard_height, depths, paths,
                                  final_lower, final_upper, valid_min, valid_max);
 }

#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

// -------------------------------------------------------------------------
// Helper: pack results into a DataFrame
// -------------------------------------------------------------------------
DataFrame rational_fraction_dataframe(const IntegerVector &nums,
                                      const IntegerVector &dens,
                                      const NumericVector &approximations,
                                      const NumericVector &x,
                                      const NumericVector &errors,
                                      const NumericVector &thomae,
                                      const NumericVector &euclids_orchard_height,
                                      const IntegerVector &depths,
                                      const CharacterVector &paths,
                                      const NumericVector &uncertainty) {
  return DataFrame::create(_["num"] = nums,
                           _["den"] = dens,
                           _["approximation"] = approximations,
                           _["x"] = x,
                           _["error"] = errors,
                           _["thomae"] = thomae,
                           _["euclids_orchard_height"] = euclids_orchard_height,
                           _["depth"] = depths,
                           _["path"] = paths,
                           _["uncertainty"] = uncertainty);
}

// -------------------------------------------------------------------------
// Main: Stern–Brocot with direct uncertainty test matching paper pseudocode
// -------------------------------------------------------------------------
//' rational_fractions
 //'
 //' Approximate each x[i]/x_ref by a coprime fraction num/den within an uncertainty,
 //' using the exact test |x/x_ref - num/den| >= uncertainty in the loop.
 //'
 //' @param x Numeric vector of values to approximate.
 //' @param x_ref Reference scalar value.
 //' @param uncertainty Uncertainty threshold for |x/x_ref - num/den|.
 //' @return DataFrame with columns: num, den, approximation, x, error, thomae,
 //'         euclids_orchard_height, depth, path, uncertainty.
 //' @export
 //' @export
 // [[Rcpp::export]]
 DataFrame rational_fractions(const NumericVector& x,
                              double x_ref,
                              double uncertainty) {
   int n = x.size();
   IntegerVector nums(n), dens(n), depths(n);
   NumericVector approximations(n), errors(n), thomae(n), euclids_orchard_height(n);
   CharacterVector paths(n);
   NumericVector unc(n, uncertainty);

   const int MAX_ITERATIONS = 10000;
   const double FLOATING_POINT_ERR = std::numeric_limits<double>::epsilon();
   const double ROUND_FACTOR = 1e15;

   for (int i = 0; i < n; ++i) {
     double ratio = x[i] / x_ref;

     // Stern–Brocot endpoints
     int left_num = 0, left_den = 1;
     int right_num = 1, right_den = 0;

     // initial mediant num/den
     int mediant_num = left_num + right_num;
     int mediant_den = left_den + right_den;
     double mediant = static_cast<double>(mediant_num) / mediant_den;

     int iteration = 1;
     string path;

     while ((FLOATING_POINT_ERR +

            std::abs(x[i] / x_ref - static_cast<double>(mediant_num) / mediant_den) >= uncertainty)

              && iteration < MAX_ITERATIONS) {
       if (mediant < ratio) {
         left_num = mediant_num;  left_den = mediant_den;
         path.push_back('R');
       } else {
         right_num = mediant_num; right_den = mediant_den;
         path.push_back('L');
       }
       mediant_num = left_num + right_num;
       mediant_den = left_den + right_den;
       if (mediant_den == 0) break;
       mediant = std::round((static_cast<double>(mediant_num) / mediant_den)
                              * ROUND_FACTOR) / ROUND_FACTOR;
       ++iteration;
     }
     if (iteration >= MAX_ITERATIONS) {
       Rcpp::warning("rational_fractions: max iterations reached for index %d", i);
     }

     nums[i] = mediant_num;
     dens[i] = mediant_den;
     approximations[i] = mediant;
     errors[i] = mediant - ratio;
     depths[i] = iteration;
     paths[i] = path;
     thomae[i] = (mediant_den ? 1.0 / mediant_den : NA_REAL);
     euclids_orchard_height[i] =
       (mediant_den ? 1.0 / (std::abs(mediant_num) + mediant_den) : NA_REAL);
   }

   return rational_fraction_dataframe(
     nums, dens, approximations,
     x, errors, thomae, euclids_orchard_height,
     depths, paths, unc
   );
 }

