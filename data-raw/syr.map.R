# Builds data/syr.map.rda: the fixed 1001-colour viridisSYR map.
# Run from the package root after devtools/pkgload::load_all():
#   source("data-raw/syr.map.R")
pkgload::load_all(".", quiet = TRUE)
hex <- syr_build(1001)
rgb <- t(grDevices::col2rgb(hex))
syr.map <- data.frame(position = seq(0, 1, length.out = 1001), hex = hex,
                      R = rgb[, 1], G = rgb[, 2], B = rgb[, 3],
                      stringsAsFactors = FALSE)
syr.map$position <- round(syr.map$position, 3)
dir.create("data", showWarnings = FALSE)
save(syr.map, file = "data/syr.map.rda", compress = "xz", version = 2)
print(syr.map[syr.map$position %in% c(0, 0.25, 0.673, 1), ])
