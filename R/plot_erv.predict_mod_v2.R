plot_erv.predict_mod <- function (x, type = "ypr", xaxis1 = "FM", yaxis1 = "Y_R.rel", 
          yaxis2 = "B_R.rel", yaxis_iso = "Lc", identify = FALSE, 
          mark = FALSE, contour = TRUE, xlab = NA, ylab1 = NA, ylab2 = NA, 
          ylab3 = NA, ...) 
{
  pes <- x
  image.identifier <- function(xyz, markII = TRUE, digits = 2) {
    intiX <- (xyz$x[2] - xyz$x[1])/2
    intiY <- (xyz$y[2] - xyz$y[1])/2
    newX <- c(xyz$x - intiX, xyz$x[length(xyz$x)] + intiX)
    newY <- c(xyz$y - intiY, xyz$y[length(xyz$y)] + intiY)
    nx <- length(xyz$x)
    ny <- length(xyz$y)
    mesi <- data.frame()
    xy <- locator(1)
    while (!is.null(xy)) {
      xbin <- as.numeric(cut(xy$x, newX))
      ybin <- as.numeric(cut(xy$y, newY))
      cat("[", xbin, ",", ybin, "] = ", xyz$z[xbin, ybin], 
          "\n", sep = "")
      lcc <- xy$y * pes$Linf
      mesi <- rbind(mesi, data.frame(i = xbin, j = ybin, 
                                     x = xy$x, y = xy$y, z = xyz$z[xbin, ybin], Lc = lcc))
      rm(xy)
      xy <- locator(1)
    }
    if (markII) {
      points(mesi$x, mesi$y, pch = 19, cex = 0.5, col = "blue")
      text(mesi$x, mesi$y, format(mesi$z, digits = digits), 
           adj = -0.2, col = "blue")
    }
    colnames(mesi) <- c("i", "j", p.FE, "Lc/Linf", p.yield, 
                        "Lc")
    mesi
  }
  if ("totY" %in% names(pes)) {
    df_Es <- pes$df_Es
    if (xaxis1 == "FM") {
      px <- pes$FM_change
      xlabel1 <- "Fishing mortality"
      if (pes$FM_relative) {
        xlabel1 <- "rel. Fishing mortality"
      }
      N05 <- df_Es$F05
      Nmax <- df_Es$Fmsy
      if (length(N05) == 1) {
        legend.lab <- c("F0.5", "Fmsy")
      }
      else legend.lab <- c("Fmsy")
      if ("currents" %in% names(pes)) 
        curr_markX <- pes$currents$curr.F
    }
    else {
      px <- pes$E_change
      xlabel1 <- "Exploitation rate"
      if (pes$FM_relative) {
        xlabel1 <- "rel. Exploitation rate"
      }
      N05 <- df_Es$E05
      Nmax <- df_Es$Emsy
      if (length(N05) == 1) {
        legend.lab <- c("E0.5", "Emsy")
      }
      else legend.lab <- c("Emsy")
      if ("currents" %in% names(pes)) 
        curr_markX <- pes$currents$curr.E
    }
    if (!is.na(xlab[1])) {
      xlabel1 <- xlab
    }
    if (is.na(ylab1[1])) {
      ylabel1 <- "Yield"
    }
    if (!is.na(ylab1[1])) {
      ylabel1 <- ylab1
    }
    if (is.na(ylab2[1])) {
      ylabel2 <- "Biomass"
    }
    if (!is.na(ylab2[1])) {
      ylabel2 <- ylab2
    }
    if (is.na(ylab3[1])) {
      ylabel3 <- "Value"
    }
    if (!is.na(ylab3[1])) {
      ylabel3 <- ylab3
    }
    max_val <- round(max(pes$totV, na.rm = TRUE), digits = 0)
    dim_val <- 10^(nchar(max_val) - 1)
    max_yiel <- round(max(pes$totY, na.rm = TRUE), digits = 0)
    dim_yiel <- 10^(nchar(max_yiel) - 1)
    max_bio <- round(max(pes$meanB, na.rm = TRUE), digits = 0)
    dim_bio <- 10^(nchar(max_bio) - 1)
    py <- pes$totY[1:length(px)]
    py2 <- pes$meanB[1:length(px)]
    plot(px, py, type = "l", ylab = ylabel1, xlab = xlabel1, 
         col = "black", ylim = c(0, ceiling(max_yiel/dim_yiel) * 
                                   dim_yiel), lwd = 1.6)
    segments(x0 = -1, x1 = Nmax, y0 = py[which(px == Nmax)], 
             y1 = py[which(px == Nmax)], col = "goldenrod1", 
             lty = 2, lwd = 1.6)
    segments(x0 = Nmax, x1 = Nmax, y0 = -1, y1 = py[which(px == 
                                                            Nmax)], col = "goldenrod1", lty = 2, lwd = 1.6)
    if (!is.null(pes$currents) & mark) {
      currents <- pes$currents
      if (!is.na(currents$curr.E) & yaxis1 == "Y_R" | 
          yaxis1 == "Y_R.rel") {
        px1 <- ifelse(xaxis1 == "FM", currents$curr.F, 
                      currents$curr.E)
        py1 <- currents$curr.Y
        points(px1, py1, pch = 16, col = "grey30")
        abline(v = px1, col = "grey30", lty = 2)
      }
    }
    par(new = TRUE)
    plot(px, py2, type = "l", ylab = "", xlab = "", col = "blue", 
         lwd = 1.6, axes = FALSE)
    axis(4, at = pretty(c(0, max(pes$meanB))), col = "blue", 
         col.axis = "blue")
    mtext(ylabel2, side = 4, line = 2.5, col = "blue", cex = 1)
    segments(x0 = -1, x1 = N05, y0 = py2[which(px == N05)], 
             y1 = py2[which(px == N05)], col = "red", lty = 3, 
             lwd = 1.5)
    segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px == 
                                                           N05)], col = "red", lty = 3, lwd = 1.5)
    if (!is.null(pes$currents) & mark) {
      currents <- pes$currents
      if (!is.na(currents$curr.E) & yaxis1 == "B_R" | 
          yaxis1 == "B_R.rel") {
        px1 <- ifelse(xaxis1 == "FM", currents$curr.F, 
                      currents$curr.E)
        py1 <- currents$curr.B
        points(px1, py1, pch = 16, col = "grey30")
        abline(v = px1, col = "grey30", lty = 2)
      }
    }
    legend("top", legend = legend.lab, xpd = TRUE, horiz = TRUE, 
           inset = c(0, 0), bty = "n", lty = c(1, 2), col = c("red", 
                                                              "goldenrod1"), seg.len = 1, pt.cex = 2, x.intersp = c(0.7, 
                                                                                                                    0.7), merge = TRUE, y.intersp = -2, box.lty = 0, 
           cex = 0.8, lwd = 2)
    if (any(pes$totV != 0)) {
      py3 <- pes$totV[1:length(px)]
      par(new = TRUE)
      plot(px, py3, type = "l", axes = FALSE, ylab = "", 
           xlab = "", col = "darkgreen", lwd = 1.6, ylim = c(0, 
                                                             ceiling(max_val/dim_val) * dim_val))
      axis(4, at = pretty(c(0, pes$totV)), line = 3.6, 
           col.axis = "darkgreen", col = "darkgreen")
      mtext(ylabel3, side = 4, line = 5.7, col = "darkgreen")
    }
  }
  if ("mat_FM_Lc_com.C" %in% names(pes)) {
    p.yield <- yaxis1
    p.FE <- xaxis1
    p.B <- yaxis2
    xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality", 
                      "Exploitation rate")
    if (pes$FM_relative) {
      xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality", 
                        "rel. Exploitation rate")
    }
    ylabel_iso <- ifelse(yaxis_iso == "Lc", "Lc", "Lc / Linf")
    if (!is.na(xlab[1])) {
      xlabel1 <- xlab
    }
    if (!is.na(ylab1[1])) {
      ylabel_iso <- ylab1
    }
    Lc_change <- pes$Lc_change
    FM_change <- pes$FM_change
    if (p.FE == "FM") {
      px <- FM_change
      if ("currents" %in% names(pes)) 
        curr_markX <- pes$currents$curr.F
    }
    else {
      px <- FM_change/(FM_change + pes$M)
      if ("currents" %in% names(pes)) 
        curr_markX <- pes$currents$curr.E
    }
    if (p.yield == "Y_R.rel" | p.yield == "Y_R") {
      pz <- pes$mat_FM_Lc_com.Y
    }
    else if (p.yield == "B_R.rel" | p.yield == "B_R") {
      pz <- pes$mat_FM_Lc_com.B
    }
    else if (p.yield == "value") {
      pz <- pes$mat_FM_Lc_com.V
    }
    else if (p.yield == "catch") {
      pz <- pes$mat_FM_Lc_com.C
    }
    pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5), rgb(1, 
                                                        1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1))))
    if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
      pal <- colorRampPalette(c(rgb(1, 0.5, 0.5), rgb(1, 
                                                      1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1)))
    }
    m <- list(x = px, y = Lc_change, z = pz)
    if ("currents" %in% names(pes)) 
      curr_markY <- pes$currents$curr.Lc
    if (yaxis_iso != "Lc" & "Linf" %in% names(pes)) {
      m$y <- Lc_change/pes$Linf
      if ("currents" %in% names(pes)) 
        curr_markY <- pes$currents$curr.Lc/pes$Linf
    }
    if (identify == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
    if (is.numeric(contour)) {
      contour(m, add = TRUE, nlevels = contour)
    }
    else if (contour == TRUE) {
      contour(m, add = TRUE)
    }
    if ("currents" %in% names(pes) & mark) {
      points(x = curr_markX, y = curr_markY, pch = 16, 
             col = "grey30")
      abline(v = curr_markX, col = "grey30", lty = 2)
      abline(h = curr_markY, col = "grey30", lty = 2)
    }
    if (identify == TRUE) 
      image.identifier(m)
  }
  if ("list_Lc_runs" %in% names(pes) | "list_tc_runs" %in% 
      names(pes)) {
    FM <- pes$FM
    if ("tc" %in% names(pes)) 
      if (!is.null(pes$tc)) 
        tc_Lc <- pes$tc
      if ("Lc" %in% names(pes)) 
        if (!is.null(pes$Lc)) 
          tc_Lc <- pes$Lc
        if (is.null(pes$tc) & is.null(pes$Lc)) 
          tc_Lc <- names(pes$list_Lc_runs)
        if ("list_tc_runs" %in% names(pes)) 
          list_tc_Lc_runs <- pes$list_tc_runs
        if ("list_Lc_runs" %in% names(pes)) 
          list_tc_Lc_runs <- pes$list_Lc_runs
        p.yield <- yaxis1
        p.FE <- xaxis1
        p.B <- yaxis2
        xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality", 
                          "Exploitation rate")
        if (pes$FM_relative) {
          xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality", 
                            "rel. Exploitation rate")
        }
        ylabel1 <- ifelse(yaxis1 == "Y_R", "Y/R", "rel. Y/R")
        ylabel2 <- ifelse(yaxis2 == "B_R", "B/R", "B/R [%]")
        ylabel_iso <- ifelse(yaxis_iso == "Lc", "Lc", "Lc / Linf")
        if (!is.na(xlab[1])) {
          xlabel1 <- xlab
        }
        if (!is.na(ylab1[1])) {
          ylabel1 <- ylab1
        }
        if (!is.na(ylab2[1])) {
          ylabel2 <- ylab2
        }
        if (!is.na(ylab1[1])) {
          ylabel_iso <- ylab1
        }
        df_Es <- pes$df_Es
        if (xaxis1 == "FM") {
          N01 <- df_Es$F01
          N05 <- df_Es$F05
          Nmax <- df_Es$Fmsy
          if (length(N05) == 1) {
            legend.lab <- c("F0.1", "F0.5", "Fmsy")
          }
          else legend.lab <- c("F0.1", "Fmsy")
          if ("currents" %in% names(pes)) 
            curr_markX <- pes$currents$curr.F
        }
        else {
          N01 <- df_Es$E01
          N05 <- df_Es$E05
          Nmax <- df_Es$Emsy
          if (length(N05) == 1) {
            legend.lab <- c("E0.1", "E0.5", "Emsy")
          }
          else legend.lab <- c("E0.1", "Emsy")
          if ("currents" %in% names(pes)) 
            curr_markX <- pes$currents$curr.E
        }
        if (type == "ypr") {
          label <- ifelse("Lc" %in% names(pes), "Lc", "tc")
          tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
          p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
          py <- p.dat[, which(names(p.dat) == p.yield)]
          px <- p.dat[, which(names(p.dat) == p.FE)]
          offset_text <- py[length(py)] * 0.05
          offset_x <- py[length(px)] * 0.1
          runs <- sapply(strsplit(names(list_tc_Lc_runs), 
                                  split = "_"), "[[", 2)
          plot(px, py, type = "l", ylim = c(0, max(py, na.rm = TRUE) * 
                                              1.8), ylab = ylabel1, xlab = xlabel1, lty = 1, col = 8, lwd = 2,
               ...)
          text(x = px[length(px)] - 30*offset_x, y = py[length(py)] - 
                 0.8*offset_text, labels = "5y (93mm)", col = 8)
          seq_tc_Lc <- 1:length(list_tc_Lc_runs)
          seq_tc_Lc <- seq_tc_Lc[-tc_Lc_start]
          for (j in seq_tc_Lc) {
            p.dat <- list_tc_Lc_runs[[j]]
            py <- p.dat[, which(names(p.dat) == p.yield)]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1, 
                  col = ifelse(j!=7,j,"brown"), lwd = 2)
            if (j==1) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] + 
                                                      0.8*offset_text), labels = "0.4y (38mm)",
                           col = j)
            if (j==2) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] - 
                                                      0.8*offset_text), labels = "1y (50mm)",
                           col = j)
            if (j==3) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] - 
                                                      0.8*offset_text), labels = "1.5y (59mm)",
                           col = j)
            if (j==4) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] - 
                                                      0.8*offset_text), labels = "2y (67mm)",
                           col = j)
            if (j==5) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] + 
                                                      0.8*offset_text), labels = "2.5y (73mm)",
                           col = j)
            if (j==6) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] - 
                                                                   0.8*offset_text), labels = "3y (78mm)",
                           col = j)
            if (j==7) text(x = px[length(px)]- 30*offset_x, y = (py[length(py)] + 
                                                                   0.8*offset_text), labels = "4y (87mm)",
                           col = "brown")
            # text(x = px[length(px)], y = (py[length(py)] + 
                                            # offset_text), labels = bquote(.(label)[.(round(as.numeric(as.character(runs[j])), 
                                                                                           # 2))]))
          }
          if (length(N01) == 1) {
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            py <- p.dat[, which(names(p.dat) == p.yield)]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            if (N01 == Nmax) {
              add_shift <- 1.005
            }
            else add_shift <- 1
            segments(x0 = -1, x1 = N01, y0 = py[which(px == 
                                                        N01)], y1 = py[which(px == N01)], col = "darkgreen", 
                     lty = 1, lwd = 1.5)
            segments(x0 = N01, x1 = N01, y0 = -1, y1 = py[which(px == 
                                                                  N01)], col = "darkgreen", lty = 1, lwd = 1.5)
            segments(x0 = -1, x1 = Nmax, y0 = py[which(px == 
                                                         Nmax)] * add_shift, y1 = py[which(px == Nmax)] * 
                       add_shift, col = "goldenrod1", lty = 2, lwd = 1.5)
            segments(x0 = Nmax * add_shift, x1 = Nmax * 
                       add_shift, y0 = -1, y1 = py[which(px == Nmax)], 
                     col = "goldenrod1", lty = 2, lwd = 1.5)
            if (length(N05) == 1) {
              legend("top", legend = legend.lab, xpd = TRUE, 
                     horiz = TRUE, inset = c(0, 0), bty = "n", 
                     lty = c(1, 3, 2), col = c("darkgreen", "red", 
                                               "goldenrod1"), seg.len = 1, pt.cex = 2, 
                     x.intersp = c(0.7, 0.7, 0.7), merge = TRUE, 
                     y.intersp = -2, box.lty = 0, cex = 0.8, 
                     lwd = 2)
            }
            else {
              legend("top", legend = legend.lab, xpd = TRUE, 
                     horiz = TRUE, inset = c(0, 0), bty = "n", 
                     lty = c(1, 2), col = c("darkgreen", "goldenrod1"), 
                     seg.len = 1, pt.cex = 2, x.intersp = c(0.7, 
                                                            0.7, 0.7), merge = TRUE, y.intersp = -2, 
                     box.lty = 0, cex = 0.8, lwd = 2)
            }
            if (!is.null(pes$currents)) {
              currents <- pes$currents
              if (!is.na(currents$curr.E)) {
                px <- ifelse(p.FE == "FM", currents$curr.F, 
                             currents$curr.E)
                py <- ifelse(p.yield == "Y_R", currents$curr.YPR, 
                             currents$curr.YPR.rel)
                points(px, py, pch = 16)
                abline(v = px, col = "grey30", lty = 2)
              }
            }
          }
          # par(new = T)
          px <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) == 
                                               p.FE)]
          py <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) == 
                                               p.B)]
          # plot(px, py, type = "l", axes = F, ylab = "", xlab = "", 
               # lty = tc_Lc_start, col = "blue")
          # axis(side = 4, at = pretty(range(py, na.rm = TRUE)), 
               # col = "blue", col.axis = "blue")
          # mtext(side = 4, text = ylabel2, line = 3, col = "blue")
          for (j in seq_tc_Lc) {
            p.dat <- list_tc_Lc_runs[[j]]
            py <- p.dat[, which(names(p.dat) == p.B)]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            # lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1, 
                  # col = "blue", lty = j)
          }
          if (length(N05) == 1) {
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            py2 <- p.dat[, which(names(p.dat) == p.B)]
            # segments(x0 = -1, x1 = N05, y0 = py2[which(px == 
                                                         # N05)], y1 = py2[which(px == N05)], col = "red", 
                     # lty = 3, lwd = 1.5)
            # segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px == 
                                                                   # N05)], col = "red", lty = 3, lwd = 1.5)
          }
        }
        if (type == "Isopleth") {
          tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
          p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
          px <- p.dat[, which(names(p.dat) == p.FE)]
          if (length(N01) > 1) {
            Lc_change <- pes$Lc
            list_Lc_runs <- pes$list_Lc_runs
            Y_R.vec <- vector("list", length(list_Lc_runs))
            for (fx in 1:length(list_Lc_runs)) {
              yr <- list_Lc_runs[[fx]][, which(names(list_Lc_runs[[fx]]) == 
                                                 p.yield)]
              Y_R.vec[[fx]] <- yr
            }
            mat_FM_Lc_com.Y <- as.matrix(do.call(cbind, 
                                                 Y_R.vec))
            rownames(mat_FM_Lc_com.Y) <- round(px, digits = 2)
            colnames(mat_FM_Lc_com.Y) <- round(Lc_change/pes$Linf, 
                                               digits = 2)
            m <- list(x = px, y = Lc_change, z = mat_FM_Lc_com.Y)
            if ("currents" %in% names(pes)) 
              curr_markY <- pes$currents$curr.Lc
            if (yaxis_iso != "Lc" & "Linf" %in% names(pes)) {
              m$y <- Lc_change/pes$Linf
              if ("currents" %in% names(pes)) 
                curr_markY <- pes$currents$curr.Lc/pes$Linf
            }
            pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5), 
                                          rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 
                                                                              1))))
            if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
              pal <- colorRampPalette(c(rgb(1, 0.5, 0.5), 
                                        rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 
                                                                            0.5, 1)))
            }
            if (identify == TRUE) {
              dev.new(noRStudioGD = TRUE)
            }
            image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
            if (is.numeric(contour)) {
              contour(m, add = TRUE, nlevels = contour)
            }
            else if (contour == TRUE) {
              contour(m, add = TRUE)
            }
            if ("currents" %in% names(pes) & mark) {
              points(x = curr_markX, y = curr_markY, pch = 16, 
                     col = "grey30")
              abline(v = curr_markX, col = "grey30", lty = 2)
              abline(h = curr_markY, col = "grey30", lty = 2)
            }
            if (identify == TRUE) 
              image.identifier(m)
          }
        }
  }
}
