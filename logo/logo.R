rm(list = ls())
graphics.off()

# if(!require(extrafont)) install.packages("extrafont")
library(extrafont)
#font_import()
suppressMessages(loadfonts())

mygrid <- function(nx = 4, pch = 15, ndots = 36, cex = 0.45, col = 1) {
  dime <- par("usr")
  
  X <- seq(dime[1], dime[2], length.out = nx + 1)
  Y <- seq(dime[3], dime[4], length.out = nx + 1)
  x <- seq(dime[1], dime[2], length.out = ndots + 1)
  y <- seq(dime[3], dime[4], length.out = ndots + 1)
  
  X <- X[-c(1, length(X))]
  Y <- Y[-c(1, length(Y))]
  x <- x[-length(x)] + diff(x)[1] / 2
  y <- y[-length(y)] + diff(y)[1] / 2
  
  for (ky in Y) lines(x, rep(ky, length(x)), type = 'p', pch = pch, cex = cex, col = col)
  for (kx in X) lines(rep(kx, length(y)), y, type = 'p', pch = pch, cex = cex, col = col)
}

colset <- data.frame(
  background = c('transparent'),
  rect = c('#d45000', '#5656FF'),
  triangle = c('#00a7a4'),
  crc = c('#5656FF', '#d45000'),
  gridc = c('#C2C4Ce'),
  text = c('#008784', '#008784', '#006767', '#006767'),
  text2 = c('#008784', '#008784', '#009999', '#009999'),
  igcex = c(rep(0.85, 4), rep(0.8, 4), rep(0.75, 4)),
  grid_nx = 4
)

j = 44
dpi = 300

for (k in 1:nrow(colset)) {
  for (out in c('.jpeg', '.pdf')) {
    set.seed(j)
    
    if (out == '.jpeg') jpeg(quality = 90, paste0('HMigD_logo_300dpi_', k, out), width = 14 * dpi, height = 3 * dpi, res = dpi, pointsize = 12) else
      if (out == '.pdf') pdf(paste0('HMigD_logo_', k, out), width = 14, height = 3) else stop()
    
    par(mar = c(4.1, 0.2, 0.4, 58.2), oma = c(0, 0, 0, 0))
    
    cex.dot <- 2.3
    X <- seq(0, 1, 0.05)
    Y <- (X + 0.5) / (X * (X - 1) + 1)
    
    plot(X, Y, type = 'l', lwd = 12, xaxs = 'i', yaxs = 'i', ylim = c(0.3, 1.95), xlim = c(-0.05, 1.05), axes = F, xlab = '', ylab = '', pty = 's')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colset$background[k], border = NA)
    
    mygrid(nx = colset$grid_nx[k], col = colset$gridc[k], ndots = ifelse(colset$grid_nx[k] == 4, 36, 30))
    Xw <- c(-0.1, seq(0, 1, 0.05), 1.1)
    Yw <- (Xw + 0.5) / (Xw * (Xw - 1) + 1)
    
    polygon(x = c(Xw, rev(Xw)), y = c(Yw - 0.4, rev(Yw) + 0.4), border = NA, col = '#DDDDDD')
    polygon(x = c(Xw, rev(Xw)), y = c(Yw - 0.17, rev(Yw) + 0.17), border = NA, col = '#A0A0A0')
    
    lines(X, Y, type = 'l', lwd = 9)
    ind <- seq(1, length(X), 2)
    Xi <- X[ind]
    
    Y2 <- Y[ind] * 0.6 - rnorm(length(Xi), 0.05, 0.05) + 0.04
    
    lines(Xi[2:5], Y2[2:5], type = 'p', cex = runif(4, cex.dot - 0.8, cex.dot + 0.2), pch = 15, col = sapply(runif(4, 0.4, 1), adjustcolor, col = colset$rect[k]))
    
    Y3 <- Y[ind] + 0.28 + rnorm(length(Xi), 0, 0.02)
    
    lines(Xi[3:11], Y3[3:11], type = 'p', cex = runif(9, cex.dot - 0.8, cex.dot + 0.2), pch = 16, col = sapply(runif(9, 0.1, 1), adjustcolor, col = colset$crc[k]))
    
    Y4 <- Y[ind] * 0.8 - rnorm(length(Xi), 0.1, 0.14)
    
    lines(Xi[8:11], Y4[8:11], type = 'p', cex = runif(4, cex.dot - 0.8, cex.dot + 0.2), pch = 17, col = sapply(runif(4, 0.5, 1), adjustcolor, col = colset$triangle[k]))
    
    if (colset$background[k] == 'transparent') box(lwd = 2, col = colset$gridc[k])
    
    fontfamily <- "Liberation Sans Narrow"
    fontcex <- 18.5
    X0 <- 1.14 *0.98
    Y0 <- 0.2925
    igs = colset$igcex[k]
    
    Hw_ <- strwidth('H', cex = fontcex, family = fontfamily, font = 2, adj = 0)*0.92
    Mw_ <- strwidth('M', cex = fontcex, family = fontfamily, font = 2, adj = 0)*0.945
    Iw_ <- strwidth('i', cex = fontcex * igs, family = fontfamily, font = 2, adj = 0)*0.92
    Gw_ <- strwidth('g', cex = fontcex * igs, family = fontfamily, font = 2, adj = 0)*0.89
    
    text(X0, Y0, 'H', col = colset$text2[k], cex = fontcex, xpd = TRUE, font = 2, family = fontfamily, adj = c(0, 0))
    text(X0 + Hw_, Y0, 'M', col = colset$text[k], cex = fontcex, xpd = TRUE, font = 2, family = fontfamily, adj = c(0, 0))
    text(X0 + Hw_ + Mw_, Y0, 'i', col = colset$text[k], cex = fontcex * igs, xpd = TRUE, font = 2, family = fontfamily, adj = c(0, 0))
    text(X0 + Hw_ + Mw_+ Iw_, Y0, 'g', col = colset$text[k], cex = fontcex * igs, xpd = TRUE, font = 2, family = fontfamily, adj = c(0, 0))
    text(X0 + Hw_ + Mw_ + Iw_+ Gw_, Y0, 'D', col = colset$text2[k], cex = fontcex, xpd = TRUE, font = 2, family = fontfamily, adj = c(0, 0))
    dev.off()
  }
}
