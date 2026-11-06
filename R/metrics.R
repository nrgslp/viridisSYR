#' Evaluate a colour scale with the criteria used to design viridis
#'
#' The viridis maps were designed to be (1) perceptually uniform: equal steps in
#' the data look like equal steps in colour, measured as the colour difference
#' \eqn{\Delta E'} between neighbouring colours in CAM02-UCS; (2) ordered by
#' lightness: lightness \eqn{J'} rises monotonically, which also makes the map
#' work in grayscale; (3) colourful, spanning a wide range of hue and chroma; and
#' (4) robust to colour vision deficiency, judged by simulating deuteranomaly,
#' protanomaly and tritanomaly (Smith & van der Walt, 2015; Machado et al.,
#' 2009). `syr_metrics()` computes these for any vector of colours listed from
#' the low to the high end of a scale.
#'
#' Columns of the result:
#' \describe{
#'   \item{deficiency}{`none`, or the simulated deficiency (`deutan`, `protan`,
#'     `tritan`)}
#'   \item{J_start, J_end, J_range}{CAM02-UCS lightness J' at the ends and its range}
#'   \item{J_monotone}{`TRUE` if J' never decreases (tolerance `tol`)}
#'   \item{J_min_step}{smallest J' step between neighbours (negative = reversal)}
#'   \item{J_r2}{R-squared of J' on position (linearity of lightness)}
#'   \item{dE_total}{path length: sum of neighbour \eqn{\Delta E'}}
#'   \item{dE_mean, dE_sd}{mean and SD of neighbour \eqn{\Delta E'}}
#'   \item{dE_cv}{coefficient of variation of neighbour \eqn{\Delta E'}: 0 means
#'     perfectly uniform steps}
#'   \item{dE_min_ratio, dE_max_ratio}{smallest and largest neighbour
#'     \eqn{\Delta E'} relative to the mean (1 = uniform)}
#'   \item{chroma_mean}{mean CAM02-UCS chroma M' (colourfulness)}
#'   \item{hue_span}{extent of the hue angle in degrees, unwrapped along the scale}
#' }
#'
#' @param colors Character vector of colours from low to high (use many, for
#'   example 256).
#' @param deficiency Which simulations to include; `"none"` is normal vision.
#' @param severity Severity of the simulated deficiency, 0 to 1 (1 = complete).
#' @param tol Tolerance for the monotonicity check, in J' units. The default (0.2) is the
#'   rounding noise of 8-bit hex colours: changing one sRGB channel by 1/255 changes J'
#'   by up to about 0.15, so tiny reversals below this are quantization, not design.
#' @return A data frame with one row per `deficiency`.
#' @references
#' Machado, G. M., Oliveira, M. M., & Fernandes, L. A. F. (2009). A
#' physiologically-based model for simulation of color vision deficiency.
#' *IEEE Transactions on Visualization and Computer Graphics, 15*(6), 1291-1298.
#'
#' Smith, N., & van der Walt, S. (2015). *A better default colormap for
#' Matplotlib* [Conference presentation]. SciPy 2015, Austin, TX.
#' @examples
#' syr_metrics(viridisLite::plasma(256))
#' syr_metrics(syr(256), deficiency = "none")
#' @export
syr_metrics <- function(colors, deficiency = c("none", "deutan", "protan", "tritan"),
                        severity = 1, tol = 0.2) {
  deficiency <- match.arg(deficiency, several.ok = TRUE)
  rows <- lapply(deficiency, function(d) {
    cols <- switch(d, none = colors,
                   deutan = colorspace::deutan(colors, severity),
                   protan = colorspace::protan(colors, severity),
                   tritan = colorspace::tritan(colors, severity))
    .metrics_one(cols, d, tol)
  })
  do.call(rbind, rows)
}

.metrics_one <- function(cols, d, tol) {
  u <- cam02ucs(cols)
  n <- length(cols); pos <- seq(0, 1, length.out = n)
  step <- sqrt(diff(u$Jp)^2 + diff(u$ap)^2 + diff(u$bp)^2)
  dJ <- diff(u$Jp)
  hue <- atan2(u$bp, u$ap)
  chroma <- sqrt(u$ap^2 + u$bp^2)
  ok <- chroma > 2                                  # hue is unstable for near-neutral colours
  hue_u <- if (sum(ok) > 1) {
    h <- hue[ok]; h <- h[1] + cumsum(c(0, (diff(h) + pi) %% (2 * pi) - pi)); h * 180 / pi
  } else 0
  data.frame(
    deficiency = d,
    J_start = u$Jp[1], J_end = u$Jp[n], J_range = u$Jp[n] - u$Jp[1],
    J_monotone = all(dJ >= -tol), J_min_step = min(dJ),
    J_r2 = stats::cor(u$Jp, pos)^2,
    dE_total = sum(step), dE_mean = mean(step), dE_sd = stats::sd(step),
    dE_cv = stats::sd(step) / mean(step),
    dE_min_ratio = min(step) / mean(step), dE_max_ratio = max(step) / mean(step),
    chroma_mean = mean(chroma), hue_span = diff(range(hue_u)),
    row.names = NULL
  )
}

#' Compare colour scales with viridis' criteria
#'
#' Runs [syr_metrics()] for several scales and stacks the results. By default it
#' compares viridis-SYR with viridis 'plasma' (the scale it is derived from),
#' using 256 colours each.
#'
#' @param ... Named character vectors of colours (low to high). If none are
#'   given, `plasma = viridisLite::plasma(256)` and `SYR = syr(256)`.
#' @inheritParams syr_metrics
#' @return A data frame: a `scale` column followed by the [syr_metrics()] columns.
#' @examples
#' syr_compare()
#' @export
syr_compare <- function(..., deficiency = c("none", "deutan", "protan", "tritan"),
                        severity = 1, tol = 0.2) {
  pals <- list(...)
  if (!length(pals)) pals <- list(plasma = viridisLite::plasma(256), SYR = syr(256))
  if (is.null(names(pals)) || any(names(pals) == "")) stop("Name every scale.", call. = FALSE)
  out <- lapply(names(pals), function(nm) {
    m <- syr_metrics(pals[[nm]], deficiency, severity, tol); cbind(scale = nm, m)
  })
  do.call(rbind, out)
}

#' SYR colours closest to the viridis 'plasma' key colours
#'
#' The five key colours of viridis 'plasma' are `viridisLite::plasma(5)`:
#' `#0D0887`, `#7E03A8`, `#CC4678`, `#F89441` and `#F0F921` at positions 0,
#' 0.25, 0.5, 0.75 and 1. For each, this finds the colour on the viridis-SYR
#' scale that is closest in CAM02-UCS (smallest \eqn{\Delta E'}), and also reports
#' the SYR colour at the same position. The `call` column gives the code that
#' returns the closest colour directly.
#'
#' @return A data frame with one row per plasma key colour: `plasma_position`,
#'   `plasma_hex`, `closest_position`, `closest_hex`, `closest_dE` (\eqn{\Delta E'}
#'   between them), `same_position_hex`, `same_position_dE`, and `call`.
#' @examples
#' syr_plasma_keys()
#' syr_at(syr_plasma_keys()$closest_position)    # call them directly
#' @export
syr_plasma_keys <- function() {
  pos <- c(0, 0.25, 0.5, 0.75, 1)
  pl  <- viridisLite::plasma(5)
  m   <- viridisSYR::syr.map
  u_map <- cam02ucs(m$hex); u_pl <- cam02ucs(pl)
  closest <- vapply(seq_along(pos), function(i) {
    d <- sqrt((u_map$Jp - u_pl$Jp[i])^2 + (u_map$ap - u_pl$ap[i])^2 + (u_map$bp - u_pl$bp[i])^2)
    near <- which(d <= min(d) + 0.05)              # colours that are indistinguishable from the best match
    near[which.min(abs(m$position[near] - pos[i]))]  # ... take the one nearest the plasma position
  }, integer(1))
  same <- syr_at(pos)
  data.frame(
    plasma_position = pos, plasma_hex = substr(pl, 1, 7),
    closest_position = m$position[closest], closest_hex = m$hex[closest],
    closest_dE = delta_e(pl, m$hex[closest]),
    same_position_hex = same, same_position_dE = delta_e(pl, same),
    call = sprintf("syr_at(%s)", format(m$position[closest], nsmall = 3)),
    stringsAsFactors = FALSE
  )
}
