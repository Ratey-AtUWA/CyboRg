profiles <- read.csv("Deep_Cores.csv")

profiles$Site <- as.factor(profiles$Site)

data1 <- subset(profiles, subset = profiles$Site == 1)
data2 <- subset(profiles, subset = profiles$Site == 2)
data3 <- subset(profiles, subset = profiles$S.bd == 1)
data4 <- subset(profiles, subset = profiles$S.bd == 2)

############################### Plot ################################
par(mfrow = c(1,1), mar = c(1,4,3,1), mgp = c(2.2,0.3,0), font.lab=2, tcl =0.3, las = 1)

#### Clay ####
with(profiles, plot(depth.upper ~ Clay,
                           pch = c(19,2)[Site],
                           col = c(2,4)[Site],
                           ylim = c(300,0), xlim = c(-1,10), 
                           xaxt = "n", xlab = "",
                           ylab = "Depth (cm)",
                           cex.lab = 1.5)
     )
grid()
axis(3)
mtext(expression(bold(paste("Clay (%)"))),
      side = 3, line = 1.5, font = 2)

# set up data.frame to store interpolation results
interps <- data.frame(fixdepth = seq(0,300,20),
                      Core_1 = rep(NA, length(seq(0,300,20))),
                      Core_1_lo = rep(NA, length(seq(0,300,20))),
                      Core_1_hi = rep(NA, length(seq(0,300,20))),
                      Core_2 = rep(NA, length(seq(0,300,20))),
                      Core_2_lo = rep(NA, length(seq(0,300,20))),
                      Core_2_hi = rep(NA, length(seq(0,300,20))))

# generate Core 1 values interpolated to fixed depth using LOESS
loessMod1 <- loess(Clay ~ depth.upper, data = data1)
loessPred1 <- predict(loessMod1, newdata=interps$fixdepth, se = TRUE)
interps$Core_1 <- loessPred1$fit
interps$Core_1_lo <- loessPred1$fit - loessPred1$se.fit
interps$Core_1_hi <- loessPred1$fit + loessPred1$se.fit

# generate Core 2 values interpolated to fixed depth using LOESS
loessMod2 <- loess(Clay ~ depth.upper, data2)
loessPred2 <- predict(loessMod2, newdata=interps$fixdepth, se = TRUE)
interps$Core_2 <- loessPred2$fit
interps$Core_2_lo <- loessPred2$fit - loessPred2$se.fit
interps$Core_2_hi <- loessPred2$fit + loessPred2$se.fit

# add interpolations to plot
lines(interps$Core_1, interps$fixdepth, col = 2)
lines(interps$Core_1_lo, interps$fixdepth, col = 2, lty = 3)
lines(interps$Core_1_hi, interps$fixdepth, col = 2, lty = 3)

lines(interps$Core_2, interps$fixdepth, col = 4)
lines(interps$Core_2_lo, interps$fixdepth, col = 4, lty = 3)
lines(interps$Core_2_hi, interps$fixdepth, col = 4, lty = 3)

legend("topright", bty = "o", inset = 0.02,
       cex = 0.85, y.intersp = 1.2, 
       box.col = "gray", box.lwd = 1, 
       legend = c("Core 1", 
                  "Smoothed (1)", " \u00A0 \u00B1 SE", 
                  "Core 2", 
                  "Smoothed (2)", " \u00A0 \u00B1 SE"), 
       col = c(2,2,2,4,4,4), lty = c(NA,1,3,NA,1,3), 
       lwd = 1, pch = c(19,NA,NA,2,NA,NA))

# tidy up
rm(list = ls(pattern = "loess"))
