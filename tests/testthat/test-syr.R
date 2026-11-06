test_that("the four key colours are exact and at the documented positions", {
  k <- syr_keys()
  expect_equal(k$key, c("su_navy", "viridis_purple", "su_orange", "viridis_yellow"))
  expect_equal(k$position, c(0, 0.25, 0.673, 1))
  expect_equal(k$hex, c("#000E54", "#7E03A8", "#F76700", "#F0F921"))
  expect_equal(syr_su(), c(navy = "#000E54", orange = "#F76700"))
  expect_equal(syr_su("orange"), c(orange = "#F76700"))
})

test_that("the purple and yellow keys are the viridis plasma colours", {
  pl <- substr(viridisLite::plasma(5), 1, 7)
  expect_equal(syr_at(0.25), pl[2])
  expect_equal(syr_at(1), pl[5])
})

test_that("the shipped map equals its construction", {
  expect_identical(syr_build(), viridisSYR::syr.map$hex)
  expect_equal(nrow(viridisSYR::syr.map), 1001)
  expect_equal(viridisSYR::syr.map$position[c(1, 251, 674, 1001)], c(0, 0.25, 0.673, 1))
})

test_that("syr() behaves like viridis()", {
  expect_length(syr(7), 7)
  expect_equal(syr(0), character(0))
  expect_equal(syr(2), c("#000E54", "#F0F921"))
  expect_equal(syr(5, direction = -1), rev(syr(5)))
  expect_equal(syr(3, begin = 0.25, end = 1)[c(1, 3)], c("#7E03A8", "#F0F921"))
  expect_match(syr(3, alpha = 0.5), "^#[0-9A-F]{6}80$")
  expect_error(syr(3, begin = -1))
  expect_error(syr(3, direction = 2))
  expect_error(syr_at(1.2))
})

test_that("syr_value_at returns the data value drawn in a key colour", {
  v <- syr_value_at(c("su_navy", "su_orange"), limits = c(0, 100))
  expect_equal(unname(v), c(0, 67.3))
  expect_equal(syr_at(unname(v) / 100)[2], "#F76700")
  # reversed scale: orange is at 1 - 0.673 of the way
  expect_equal(unname(syr_value_at("su_orange", c(0, 1), direction = -1)), 1 - 0.673)
  expect_error(syr_value_at("su_navy", begin = 0.25))
  expect_error(syr_value_at("not_a_key"))
})

test_that("lightness rises monotonically along the scale", {
  u <- cam02ucs(syr(256))
  expect_true(all(diff(u$Jp) >= -0.15))      # no reversal larger than 0.15 J' (a fraction of one step)
  expect_gt(u$Jp[256], u$Jp[1] + 60)
})

test_that("ggplot2 scales build", {
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  d <- data.frame(x = 1:10, y = 1:10, z = 1:10, g = rep(c("a", "b"), 5))
  p1 <- ggplot(d, aes(x, y, colour = z)) + geom_point() + scale_colour_syr_c()
  p2 <- ggplot(d, aes(x, y, fill = g)) + geom_point(shape = 21) + scale_fill_syr_d()
  expect_s3_class(ggplot_build(p1), "ggplot_built")
  expect_s3_class(ggplot_build(p2), "ggplot_built")
  cols <- ggplot_build(p1)$data[[1]]$colour
  expect_equal(toupper(cols[1]), "#000E54"); expect_equal(toupper(cols[10]), "#F0F921")
})
