test_that("CAM02-UCS anchors: black is 0, white is 100", {
  u <- cam02ucs(c("black", "white"))
  expect_equal(u$Jp, c(0, 100), tolerance = 1e-6)
  expect_lt(sqrt(u$ap[2]^2 + u$bp[2]^2), 5)        # white is near neutral (adaptation is incomplete, D < 1)
})

test_that("a gray ramp is ordered in lightness and has no chroma", {
  g <- grDevices::gray(seq(0, 1, length.out = 20))
  u <- cam02ucs(g)
  expect_true(all(diff(u$Jp) > 0))
  expect_lt(max(sqrt(u$ap^2 + u$bp^2)), 5)
})

test_that("viridis comes out nearly uniform and lightness-ordered, as designed", {
  m <- syr_metrics(viridisLite::viridis(256), "none")
  expect_true(m$J_monotone)
  expect_lt(m$dE_cv, 0.25)                 # neighbouring-colour differences are nearly constant
  expect_gt(m$J_r2, 0.97)                  # lightness is nearly linear in position
})

test_that("delta_e is a distance", {
  expect_equal(delta_e("#F76700", "#F76700"), 0)
  expect_gt(delta_e("#000E54", "#F76700"), 20)
  expect_equal(delta_e("#000E54", "#F76700"), delta_e("#F76700", "#000E54"))
})

test_that("syr_compare and syr_plasma_keys return the documented shapes", {
  cmp <- syr_compare()
  expect_setequal(unique(cmp$scale), c("plasma", "SYR"))
  expect_equal(nrow(cmp), 8)
  expect_true(all(cmp$J_monotone[cmp$deficiency == "none"]))
  k <- syr_plasma_keys()
  expect_equal(nrow(k), 5)
  # plasma start, purple and yellow are exact; pink is close
  expect_equal(k$closest_hex[c(1, 2, 5)], c("#0D0887", "#7E03A8", "#F0F921"))
  expect_equal(k$closest_dE[c(1, 2, 5)], c(0, 0, 0), tolerance = 1e-6)
  expect_equal(k$closest_position[c(1, 2, 5)], c(0.05, 0.2875, 1))
  expect_lt(k$closest_dE[3], 2)
  expect_equal(k$equivalent_position, 0.05 + 0.95 * c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(syr_at(k$closest_position), k$closest_hex)
})