#' viridisSYR colour scales for ggplot2
#'
#' Continuous (`_c`) and discrete (`_d`) colour and fill scales using
#' [syr()]. The interface follows `ggplot2::scale_colour_viridis_c()` and
#' `scale_colour_viridis_d()`.
#'
#' With the continuous scale, Syracuse orange is drawn at 0.673 of the way from
#' the lowest to the highest value (use [syr_value_at()] to get that data
#' value). With the discrete scale, `n` classes are spread evenly over
#' `begin`-`end`; for four classes in the exact key colours use
#' `scale_*_manual(values = syr_keys()$hex)`.
#'
#' @inheritParams syr
#' @param ... Passed to [ggplot2::continuous_scale()] or
#'   [ggplot2::discrete_scale()] (for example `name`, `limits`, `breaks`,
#'   `labels`).
#' @param values Optional positions (0 to 1) of the colours along the scale, as
#'   in [scales::gradient_n_pal()].
#' @param na.value Colour for missing values.
#' @param guide Type of legend. `"colourbar"` for a colour bar (continuous).
#' @param aesthetics Aesthetic(s) the scale applies to.
#' @name scale_syr
#' @examples
#' library(ggplot2)
#' d <- expand.grid(x = 1:20, y = 1:20)
#' d$z <- with(d, sin(x / 4) + cos(y / 5))
#' ggplot(d, aes(x, y, fill = z)) + geom_raster() + scale_fill_syr_c()
#'
#' ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#'   geom_point(size = 2) + scale_colour_syr_d(end = 0.9)
NULL

#' @rdname scale_syr
#' @export
scale_colour_syr_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                               values = NULL, na.value = "grey50",
                               guide = "colourbar", aesthetics = "colour") {
  ggplot2::continuous_scale(
    aesthetics,
    palette = scales::gradient_n_pal(syr(256, alpha, begin, end, direction), values, "Lab"),
    na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_syr
#' @export
scale_color_syr_c <- scale_colour_syr_c

#' @rdname scale_syr
#' @export
scale_fill_syr_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                             values = NULL, na.value = "grey50",
                             guide = "colourbar", aesthetics = "fill") {
  ggplot2::continuous_scale(
    aesthetics,
    palette = scales::gradient_n_pal(syr(256, alpha, begin, end, direction), values, "Lab"),
    na.value = na.value, guide = guide, ...
  )
}

#' @rdname scale_syr
#' @export
scale_colour_syr_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                               aesthetics = "colour") {
  ggplot2::discrete_scale(aesthetics, palette = syr_pal(alpha, begin, end, direction), ...)
}

#' @rdname scale_syr
#' @export
scale_color_syr_d <- scale_colour_syr_d

#' @rdname scale_syr
#' @export
scale_fill_syr_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                             aesthetics = "fill") {
  ggplot2::discrete_scale(aesthetics, palette = syr_pal(alpha, begin, end, direction), ...)
}
