#' CAM02-UCS coordinates of colours
#'
#' Converts colours to the CAM02-UCS uniform colour space (Luo, Cui & Li, 2006),
#' the space in which the viridis colour maps were designed and checked
#' (Smith & van der Walt, 2015). Distances between colours in this space
#' (Euclidean, \eqn{\Delta E'}) approximate perceived colour differences, and
#' \eqn{J'} is perceptual lightness.
#'
#' The CIECAM02 appearance model (Moroney et al., 2002) is evaluated for sRGB
#' viewing conditions: D65 white point (X, Y, Z = 95.047, 100, 108.883),
#' background luminance factor `Y_b` = 20, adapting luminance
#' `L_A` = 64 / pi / 5 cd/m2, and an average surround (`c` = 0.69, `N_c` = 1,
#' `F` = 1). These are the conditions used by the `colorspacious` Python
#' package that the viridis authors used. This is an independent
#' implementation; see the `compare-with-plasma` vignette for sanity checks
#' (black is J' = 0, white is J' = 100, and viridis comes out nearly uniform).
#'
#' @param col A vector of colours (anything [grDevices::col2rgb()] accepts).
#' @return A data frame with one row per colour and columns `Jp`, `ap`, `bp`
#'   (CAM02-UCS J', a', b'), plus `J` (CIECAM02 lightness), `M` (colourfulness)
#'   and `h` (hue angle in degrees).
#' @references
#' Luo, M. R., Cui, G., & Li, C. (2006). Uniform colour spaces based on the
#' CIECAM02 colour appearance model. *Color Research & Application, 31*(4),
#' 320-330.
#'
#' Moroney, N., Fairchild, M. D., Hunt, R. W. G., Li, C., Luo, M. R., & Newman,
#' T. (2002). The CIECAM02 color appearance model. *IS&T/SID 10th Color Imaging
#' Conference*, 23-27.
#'
#' Smith, N., & van der Walt, S. (2015). *A better default colormap for
#' Matplotlib* [Conference presentation]. SciPy 2015, Austin, TX.
#' @examples
#' cam02ucs(c("black", "white", "#000E54", "#F76700"))
#' @export
cam02ucs <- function(col) {
  rgb <- t(grDevices::col2rgb(col)) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  m_xyz <- matrix(c(0.4124564, 0.3575761, 0.1804375,
                    0.2126729, 0.7151522, 0.0721750,
                    0.0193339, 0.1191920, 0.9503041), 3, byrow = TRUE)
  xyz <- 100 * lin %*% t(m_xyz)

  vc <- .cam02_conditions()
  adapt <- function(r) {                                   # n x 3 in HPE space
    x <- (vc$FL * abs(r) / 100)^0.42
    sign(r) * 400 * x / (27.13 + x) + 0.1
  }
  rgb_c  <- sweep(xyz %*% t(vc$Mcat), 2, vc$Dc, `*`)
  rgb_p  <- rgb_c %*% t(vc$Mhpe %*% solve(vc$Mcat))
  ra     <- adapt(rgb_p)
  a <- ra[, 1] - 12 * ra[, 2] / 11 + ra[, 3] / 11
  b <- (ra[, 1] + ra[, 2] - 2 * ra[, 3]) / 9
  h <- atan2(b, a) %% (2 * pi)
  et <- (cos(h + 2) + 3.8) / 4
  A  <- (2 * ra[, 1] + ra[, 2] + ra[, 3] / 20 - 0.305) * vc$Nbb
  J  <- 100 * (pmax(A, 0) / vc$Aw)^(vc$c * vc$z)
  t  <- (50000 / 13 * vc$Nc * vc$Ncb * et * sqrt(a^2 + b^2)) /
    (ra[, 1] + ra[, 2] + 21 / 20 * ra[, 3])
  C  <- t^0.9 * sqrt(J / 100) * (1.64 - 0.29^vc$n)^0.73
  M  <- C * vc$FL^0.25

  Jp <- 1.7 * J / (1 + 0.007 * J)
  Mp <- log(1 + 0.0228 * M) / 0.0228
  data.frame(Jp = Jp, ap = Mp * cos(h), bp = Mp * sin(h),
             J = J, M = M, h = h * 180 / pi)
}

.cam02_conditions <- function() {
  Xw <- c(95.047, 100, 108.883)
  LA <- 64 / pi / 5; Yb <- 20; F <- 1; c <- 0.69; Nc <- 1
  k  <- 1 / (5 * LA + 1)
  FL <- 0.2 * k^4 * (5 * LA) + 0.1 * (1 - k^4)^2 * (5 * LA)^(1 / 3)
  n  <- Yb / Xw[2]
  Nbb <- 0.725 * (1 / n)^0.2
  z  <- 1.48 + sqrt(n)
  D  <- F * (1 - (1 / 3.6) * exp((-LA - 42) / 92))
  Mcat <- matrix(c(0.7328, 0.4296, -0.1624,
                   -0.7036, 1.6975, 0.0061,
                   0.0030, 0.0136, 0.9834), 3, byrow = TRUE)
  Mhpe <- matrix(c(0.38971, 0.68898, -0.07868,
                   -0.22981, 1.18340, 0.04641,
                   0, 0, 1), 3, byrow = TRUE)
  Rw <- as.vector(Mcat %*% Xw)
  Dc <- D * Xw[2] / Rw + 1 - D
  rw_p <- as.vector(Mhpe %*% solve(Mcat) %*% (Dc * Rw))
  x  <- (FL * abs(rw_p) / 100)^0.42
  raw <- sign(rw_p) * 400 * x / (27.13 + x) + 0.1
  Aw <- (2 * raw[1] + raw[2] + raw[3] / 20 - 0.305) * Nbb
  list(FL = FL, n = n, Nbb = Nbb, Ncb = Nbb, z = z, c = c, Nc = Nc,
       Mcat = Mcat, Mhpe = Mhpe, Dc = Dc, Aw = Aw)
}

#' Perceptual colour difference in CAM02-UCS
#'
#' Euclidean distance \eqn{\Delta E'} between pairs of colours in CAM02-UCS
#' (see [cam02ucs()]).
#'
#' @param col1,col2 Colours, recycled to a common length.
#' @return A numeric vector of \eqn{\Delta E'} values.
#' @examples
#' delta_e("#000E54", "#F76700")
#' @export
delta_e <- function(col1, col2) {
  n <- max(length(col1), length(col2))
  a <- cam02ucs(rep_len(col1, n)); b <- cam02ucs(rep_len(col2, n))
  sqrt((a$Jp - b$Jp)^2 + (a$ap - b$ap)^2 + (a$bp - b$bp)^2)
}
