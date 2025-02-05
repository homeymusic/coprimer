test_that("plot_stern_brocot_tree(10) matches reference", {
  vdiffr::expect_doppelganger("coprime_quantum_blob",
                              plot_coprime_quantum_blob(x = 0, p = 0,
                                                        lower_x_uncertainty = 0.167, upper_x_uncertainty = 0.167,
                                                        lower_p_uncertainty = 0.152, upper_p_uncertainty = 0.152,
                                                        depth = 10)
  )
})
