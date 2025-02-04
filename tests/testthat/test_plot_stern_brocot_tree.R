test_that("mismatch of path length and depth throws an error", {
  depth = 3
  expect_error(plot_stern_brocot_tree(3, paste0(rep("R", depth+1), collapse = "")),
               "Depth and the length of the path must be the same.")
  expect_error(plot_stern_brocot_tree(3, paste0(rep("R", depth-1), collapse = "")),
               "Depth and the length of the path must be the same.")
})

test_that("plot_stern_brocot_tree() default parameters match reference", {
  vdiffr::expect_doppelganger("default_tree", plot_stern_brocot_tree())
})

test_that("plot_stern_brocot_tree(2) matches reference", {
  vdiffr::expect_doppelganger("tree_depth_2", plot_stern_brocot_tree(2))
})

test_that("plot_stern_brocot_tree(3, 'RLR') matches reference", {
  vdiffr::expect_doppelganger("tree_depth_3_path_RLR", plot_stern_brocot_tree(3, "RLR"))
})

test_that("plot_stern_brocot_tree(10) matches reference", {
  vdiffr::expect_doppelganger("tree_depth_10_path_LRLRLRLRLR", plot_stern_brocot_tree(10, "LRLRLRLRLR"))
})
