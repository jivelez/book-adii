msp <- function (x1, x2, linesat = c(0, 0), plotlines = TRUE, typel = "loess",
          ...)
{
  df <- data.frame(x1, x2)
  d0 <- df[complete.cases(df), ]
  x <- densCols(x1, x2, colramp = colorRampPalette(c("black",
                                                     "white")))
  df$dens <- col2rgb(x)[1, ] + 1L
  cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                             "#FCFF00", "#FF9400", "#FF3100"))(256)
  df$col <- cols[df$dens]
  plot(x2 ~ x1, data = df[order(df$dens), ], col = col, las = 1,
       ...)
  fit <- lm(x2 ~ x1, data = df)
  if (plotlines) {
    abline(v = linesat[1], col = "gray50", lty = 2)
    abline(h = linesat[2], col = "gray50", lty = 2)
  }
  if (typel == "loess") {
    fit <- loess(x2 ~ x1, data = d0)
    v0 <- with(d0, seq(ifelse(min(x1) < 0, 1.2 * min(x1),
                              0.8 * min(x1)), 1.2 * max(x1), length = 500))
    ypred <- predict(fit, new = data.frame(x1 = v0))
    points(v0, ypred, type = "l", lty = 2, col = 2, lwd = 1.5)
  }
  if (typel == "linear") {
    fit <- lm(x2 ~ x1, data = d0)
    abline(fit, col = 2, lwd = 1.5, lty = 2)
  }
  require(MASS)
  z <- MASS:::kde2d(d0[, "x1"], d0[, "x2"], n = ifelse(NROW(d0) <
                                                         500, NROW(d0), 500))
  g <- 5
  contour(z, drawlabels = FALSE, nlevels = g, col = colors()[124],
          add = TRUE, lwd = 1)
}
