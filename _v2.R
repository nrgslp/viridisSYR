suppressMessages({pkgload::load_all(".", quiet = TRUE); library(ggplot2); library(farver)})
navy <- "#000E54"; orange <- "#F76700"
build_v2 <- function(p0, n = 1001, orange_at = 0.673, orange_width = 0.25) {
  t <- seq(0, 1, length.out = n)
  pl_hex <- substr(viridisLite::plasma(n), 1, 7)
  to_lab <- function(h) farver::convert_colour(t(grDevices::col2rgb(h)), "rgb", "oklab")
  pl <- to_lab(pl_hex); x0 <- seq(0, 1, length.out = n)
  plasma_at <- function(s) cbind(approx(x0, pl[, 1], s)$y, approx(x0, pl[, 2], s)$y, approx(x0, pl[, 3], s)$y)
  lab <- matrix(0, n, 3)
  seg <- t < p0
  lab[!seg, ] <- plasma_at((t[!seg] - p0) / (1 - p0))                      # plasma, compressed into [p0, 1]
  u <- t[seg] / p0                                                          # navy -> plasma start: constant-speed OKLab ramp
  lab[seg, ] <- outer(1 - u, as.numeric(to_lab(navy))) + outer(u, as.numeric(plasma_at(0)))
  c_o <- p0 + (1 - p0) * orange_at; w_o <- orange_width * (1 - p0)
  bump <- ifelse(abs(t - c_o) < w_o, cos(pi / 2 * (t - c_o) / w_o)^2, 0)
  i_o <- which.min(abs(t - c_o))
  lab <- lab + outer(bump, as.numeric(to_lab(orange)) - lab[i_o, ])
  hex <- toupper(farver::encode_colour(pmin(pmax(farver::convert_colour(lab, "oklab", "rgb"), 0), 255), from = "rgb"))
  attr(hex, "pos") <- c(navy = 0, plasma_start = p0, plasma_purple = p0 + (1 - p0) * 0.25, su_orange = t[i_o], yellow = 1)
  hex
}
res <- do.call(rbind, lapply(c(0.04, 0.05, 0.06, 0.08, 0.10), function(p0) {
  h <- build_v2(p0); s <- h[round(seq(1, 1001, length.out = 256))]
  m <- syr_metrics(s, "none"); data.frame(scale = paste0("v2 p0=", p0), m[, c("J_start", "J_range", "J_monotone", "J_min_step", "J_r2", "dE_cv", "dE_max_ratio")])
}))
base <- rbind(cbind(scale = "plasma", syr_metrics(viridisLite::plasma(256), "none")[, c("J_start", "J_range", "J_monotone", "J_min_step", "J_r2", "dE_cv", "dE_max_ratio")]),
              cbind(scale = "current SYR", syr_metrics(syr(256), "none")[, c("J_start", "J_range", "J_monotone", "J_min_step", "J_r2", "dE_cv", "dE_max_ratio")]))
out <- rbind(base, res); num <- sapply(out, is.numeric); out[num] <- lapply(out[num], round, 3); print(out, row.names = FALSE)
cat("delta E navy -> plasma start:", round(delta_e("#000E54", "#0D0887"), 2), "\n")

p0 <- 0.05; v2 <- build_v2(p0); print(attr(v2, "pos"))
cat("key hex: navy", v2[1], "| plasma start at p0:", v2[which.min(abs(seq(0, 1, length.out = 1001) - p0))], "| orange", v2[which.min(abs(seq(0, 1, length.out = 1001) - attr(v2, "pos")["su_orange"]))], "| end", v2[1001], "\n")
saveRDS(v2, file.path(tempdir(), "v2.rds"))
n <- 256; xs <- seq(0, 1, length.out = n); ix <- round(seq(1, 1001, length.out = n))
pals <- list("viridis plasma" = viridisLite::plasma(n), "viridisSYR (current)" = syr(n),
             "viridisSYR v2: navy -> plasma start -> purple -> SU orange -> yellow" = substr(v2[ix], 1, 7))
gray <- function(cols) grDevices::gray(cam02ucs(cols)$Jp / 100)
d <- do.call(rbind, lapply(seq_along(pals), function(i) rbind(
  data.frame(x = xs, y = 3.2 - (i - 1) * 1.55, col = pals[[i]], k = "c"),
  data.frame(x = xs, y = 3.2 - (i - 1) * 1.55 - 0.47, col = gray(pals[[i]]), k = "g"))))
lab <- data.frame(y = 3.2 - (seq_along(pals) - 1) * 1.55 + 0.52, txt = names(pals))
pos <- attr(v2, "pos")
p <- ggplot() + geom_tile(data = d[d$k == "c", ], aes(x, y, fill = col), width = 1 / n * 1.05, height = 0.6) +
  geom_tile(data = d[d$k == "g", ], aes(x, y, fill = col), width = 1 / n * 1.05, height = 0.3) + scale_fill_identity() +
  geom_text(data = lab, aes(x = -0.02, y = y, label = txt), hjust = 0, size = 3.9, fontface = "bold") +
  annotate("point", x = pos[-1][1:3], y = 3.2 - 2 * 1.55 - 0.78, shape = 17, size = 2.2) +
  annotate("text", x = pos[-1][1:3], y = 3.2 - 2 * 1.55 - 1.0, size = 3, label = c("#0D0887\n(plasma start)", "#7E03A8\n(plasma purple)", "#F76700\n(SU orange)"), hjust = c(0.2, 0.5, 0.5)) +
  annotate("point", x = 0, y = 3.2 - 2 * 1.55 - 0.78, shape = 17, size = 2.2) +
  annotate("segment", x = 0, xend = 0, y = 3.2 - 2 * 1.55 - 0.9, yend = 3.2 - 2 * 1.55 - 1.25, linewidth = 0.3) +
  annotate("text", x = 0.0, y = 3.2 - 2 * 1.55 - 1.5, hjust = 0, size = 3, label = "#000E54 (SU navy)") +
  scale_x_continuous(breaks = seq(0, 1, .25), limits = c(-0.03, 1.04), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.9, 4.0), breaks = NULL) + labs(x = "position on the scale", y = NULL) +
  theme_minimal(base_size = 12) + theme(panel.grid = element_blank())
ggsave("C:/Apps/Local-Software-Dev/temp/su_plasma_palettes_v2.png", p, width = 9, height = 5, dpi = 170, bg = "white")
cat("saved\n")
