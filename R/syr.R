#' viridisSYR colour scale
#'
#' `syr()` returns `n` colours from the viridisSYR scale: Syracuse University
#' navy (`#000E54`) at position 0, the viridis 'plasma' purple (`#7E03A8`) at
#' 0.25, Syracuse University orange (`#F76700`) at 0.673, and the viridis
#' 'plasma' yellow (`#F0F921`) at 1, with smooth transitions between them
#' (see [syr_build()]). The interface follows [viridisLite::viridis()].
#'
#' @param n Number of colours.
#' @param alpha Opacity, 0 to 1. If less than 1, colours are returned as
#'   `#RRGGBBAA`.
#' @param begin,end Positions on the scale (0 to 1) where the palette starts and
#'   ends. Use them to use only part of the scale.
#' @param direction `1` (default) goes navy to yellow; `-1` reverses.
#' @return A character vector of hex colours.
#' @seealso [syr_at()] for colours at exact positions, [syr_keys()] and
#'   [syr_su()] for the Syracuse and viridis key colours.
#' @examples
#' syr(5)
#' syr(256)[c(1, 256)]
#' syr(3, begin = 0.25, end = 1)
#' syr(5, direction = -1)
#' @export
syr <- function(n, alpha = 1, begin = 0, end = 1, direction = 1) {
  if (n <= 0) return(character(0))
  .check_unit(c(alpha, begin, end), "alpha, begin and end")
  if (!direction %in% c(1, -1)) stop("`direction` must be 1 or -1.", call. = FALSE)
  pos <- seq(begin, end, length.out = n)
  if (direction == -1) pos <- rev(pos)
  syr_at(pos, alpha = alpha)
}

#' Colours at exact positions on the viridisSYR scale
#'
#' Returns the colour at any position between 0 (navy end) and 1 (yellow end).
#' Use [syr_keys()] for the positions of the Syracuse and viridis key colours.
#'
#' @param position Numeric vector of positions in `[0, 1]`.
#' @param alpha Opacity, 0 to 1.
#' @return A character vector of hex colours, one per position.
#' @examples
#' syr_at(c(0, 0.25, 0.673, 1))
#' syr_at(syr_keys()$position)
#' @export
syr_at <- function(position, alpha = 1) {
  .check_unit(position, "`position`")
  .check_unit(alpha, "`alpha`")
  m <- viridisSYR::syr.map
  rgb <- cbind(stats::approx(m$position, m$R, xout = position)$y,
               stats::approx(m$position, m$G, xout = position)$y,
               stats::approx(m$position, m$B, xout = position)$y)
  hex <- grDevices::rgb(round(rgb[, 1]), round(rgb[, 2]), round(rgb[, 3]), maxColorValue = 255)
  if (alpha < 1) hex <- paste0(hex, sprintf("%02X", round(alpha * 255)))
  hex
}

#' Palette function for viridisSYR
#'
#' @inheritParams syr
#' @return A function of one argument (`n`) that returns `n` colours, for use
#'   with `scales`/`ggplot2`.
#' @examples
#' pal <- syr_pal(begin = 0.1)
#' pal(4)
#' @export
syr_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1) {
  function(n) syr(n, alpha, begin, end, direction)
}

#' Key colours of the viridisSYR scale
#'
#' The four colours that define the scale and their positions: two Syracuse
#' University brand colours and two viridis 'plasma' colours.
#'
#' @return A data frame with columns `key`, `position` (0 to 1), `hex` and
#'   `origin`.
#' @examples
#' syr_keys()
#' syr_keys()$hex                      # a 4-class categorical palette
#' @export
syr_keys <- function() {
  key <- c("su_navy", "viridis_purple", "su_orange", "viridis_yellow")
  position <- c(0, .syr$purple_at, .syr$orange_at, 1)
  data.frame(key = key, position = position, hex = syr_at(position),
             origin = c("Syracuse University navy", "viridis plasma(5)[2]",
                        "Syracuse University orange", "viridis plasma(5)[5]"),
             stringsAsFactors = FALSE)
}

#' The Syracuse University colours, retrieved from the scale
#'
#' Returns the two Syracuse University brand colours as they appear on the
#' viridisSYR scale. They are exactly `#000E54` (navy) and `#F76700` (orange).
#'
#' @param which Which colour(s): `"navy"`, `"orange"`, or both (default).
#' @return A named character vector of hex colours.
#' @examples
#' syr_su()
#' syr_su("orange")
#' @export
syr_su <- function(which = c("navy", "orange")) {
  which <- match.arg(which, several.ok = TRUE)
  k <- syr_keys()
  out <- stats::setNames(k$hex[match(paste0("su_", which), k$key)], which)
  out
}

#' Data values at which the scale shows a key colour
#'
#' Given the limits of a continuous colour scale (what the colour bar spans),
#' returns the data values that are drawn in a key colour, for example the value
#' that appears in Syracuse orange.
#'
#' @param key One or more of `"su_navy"`, `"viridis_purple"`, `"su_orange"`,
#'   `"viridis_yellow"`.
#' @param limits Length-2 numeric vector: the data values at the two ends of the
#'   colour scale (default `c(0, 1)`).
#' @param begin,end,direction As in [syr()], if the scale uses only part of the
#'   palette or is reversed. An error is raised if a key colour is outside the
#'   part used.
#' @return A named numeric vector of data values.
#' @examples
#' syr_value_at(c("su_navy", "su_orange"), limits = c(0, 100))
#' syr_value_at("su_orange", limits = c(-3, 3))
#' @export
syr_value_at <- function(key, limits = c(0, 1), begin = 0, end = 1, direction = 1) {
  k <- syr_keys()
  if (!all(key %in% k$key)) stop("Unknown `key`; see syr_keys()$key.", call. = FALSE)
  pos <- k$position[match(key, k$key)]
  rel <- (pos - begin) / (end - begin)
  if (direction == -1) rel <- 1 - rel
  if (any(rel < -1e-9 | rel > 1 + 1e-9)) {
    stop("A requested key colour lies outside the part of the scale used (`begin`/`end`).", call. = FALSE)
  }
  stats::setNames(limits[1] + rel * (limits[2] - limits[1]), key)
}

# ---- construction -----------------------------------------------------------------------

.syr <- list(
  navy = "#000E54", orange = "#F76700",
  purple_at = 0.25,       # viridis plasma(5)[2], #7E03A8
  orange_at = 0.673,      # where plasma is closest to Syracuse orange (OKLab), rounded
  orange_width = 0.25,    # half-width of the correction that bends plasma through the orange
  n = 1001
)

#' Rebuild the viridisSYR colour map
#'
#' The construction behind [syr.map]. It is exported so the map can be
#' reproduced and checked; the package uses the stored map, so results do not
#' depend on the installed version of 'viridisLite'.
#'
#' The map is the viridis 'plasma' map with two changes, made in the OKLab
#' colour space (Ottosson, 2020):
#' 1. From position 0 to 0.25, plasma's blue-violet start is replaced by a
#'    smooth (smoothstep) ramp from Syracuse navy to the plasma purple at 0.25.
#' 2. Around position 0.673 an offset is added, with cosine-squared falloff and a
#'    half-width of 0.25, equal to (Syracuse orange minus plasma) at its centre,
#'    so the scale passes exactly through Syracuse orange there. The offset is
#'    zero at 0.423 and below and at 0.923 and above, so the plasma purple at
#'    0.25 and the plasma yellow at 1 are unchanged.
#'
#' Out-of-gamut values are clipped to sRGB, and the four key colours are set to
#' their exact hex values.
#'
#' @param n Number of colours (positions `seq(0, 1, length.out = n)`). The key
#'   positions are only exactly on the grid for `n = 1001`.
#' @return A character vector of hex colours.
#' @references
#' Ottosson, B. (2020). *A perceptual color space for image processing.*
#' <https://bottosson.github.io/posts/oklab/>
#' @examples
#' identical(syr_build(), viridisSYR::syr.map$hex)
#' @export
syr_build <- function(n = .syr$n) {
  p <- .syr
  t <- seq(0, 1, length.out = n)
  base <- substr(viridisLite::plasma(n), 1, 7)          # drop the alpha suffix
  to_lab <- function(hex) farver::convert_colour(t(grDevices::col2rgb(hex)), "rgb", "oklab")
  pl <- to_lab(base)
  lab <- pl
  i_p <- which.min(abs(t - p$purple_at))
  i_o <- which.min(abs(t - p$orange_at))
  u <- pmin(t / p$purple_at, 1); u <- u^2 * (3 - 2 * u)
  seg <- t < p$purple_at
  lab[seg, ] <- outer(1 - u[seg], as.numeric(to_lab(p$navy))) + outer(u[seg], pl[i_p, ])
  bump <- ifelse(abs(t - p$orange_at) < p$orange_width,
                 cos(pi / 2 * (t - p$orange_at) / p$orange_width)^2, 0)
  lab <- lab + outer(bump, as.numeric(to_lab(p$orange)) - pl[i_o, ])
  rgb <- pmin(pmax(farver::convert_colour(lab, "oklab", "rgb"), 0), 255)
  hex <- toupper(farver::encode_colour(rgb, from = "rgb"))
  if (n == p$n) {                       # key colours are exact
    hex[1] <- toupper(p$navy); hex[i_o] <- toupper(p$orange)
    hex[i_p] <- toupper(base[i_p]); hex[n] <- toupper(base[n])
  }
  hex
}

.check_unit <- function(x, what) {
  if (!is.numeric(x) || anyNA(x) || any(x < -1e-12 | x > 1 + 1e-12)) {
    stop(what, " must be numeric and between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}
