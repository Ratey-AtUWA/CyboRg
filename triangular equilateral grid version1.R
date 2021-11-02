SVbound <- read.csv("sv_boundary.csv")
summary(SVbound)
extremes <- c(min(floor(SVbound[, 1])),
              min(floor(SVbound[, 2])),
              max(ceiling(SVbound[, 1])),
              max(ceiling(SVbound[, 2])))
spacing <- 53
y_starts <- seq(extremes[2],extremes[4], 
                      by = spacing*(sqrt(3)/2))
x_zero <- seq(extremes[1],extremes[3], 
                    by = spacing)
x_offset <- seq(extremes[1]+(spacing/2),extremes[3], 
                    by = spacing)
if(length(x_zero)-length(x_offset)>0) {
  x_offset <- append(x_offset, x_offset[length(x_offset)]+spacing)
}
x_all <- x_zero
for (i in 2:length(y_starts)){
  if(i %% 2 == 0) {
    x_all <- append(x_all,x_offset)
  }
    else
      {
        x_all <- append(x_all,x_zero)
  }
}
points <- cbind(x_all, rep(y_starts, each=length(x_zero)))
par(mar=c(3,3,1,1))
plot(points,asp=1, pch=3)
lines(SVbound, col="red3")
