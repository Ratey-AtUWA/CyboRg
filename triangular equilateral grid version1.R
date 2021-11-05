SVbound <- read.csv("sv_boundary.csv") # file is in this repository
summary(SVbound)
extremes <- c(min(floor(SVbound[, 1])),
              min(floor(SVbound[, 2])),
              max(ceiling(SVbound[, 1])),
              max(ceiling(SVbound[, 2])))
spacing <- 53 # adjust to suit

# set y-coordinate spacing (gives no. of rows)
y_starts <- seq(extremes[2],extremes[4], 
                      by = spacing*(sqrt(3)/2))

# x-coordinates for rows alternate to create triangles
x_zero <- seq(extremes[1],extremes[3], 
                    by = spacing)
x_offset <- seq(extremes[1]+(spacing/2),extremes[3], 
                    by = spacing)

# this lne to capture unequal length alternating rows due to offset
if(length(x_zero)-length(x_offset)>0) {
  x_offset <- append(x_offset, x_offset[length(x_offset)]+spacing)
}
# initialise x-coordinate vector
x_all <- x_zero
# loop for no. of rows, odd rows start from zero, even from spacing/2
for (i in 2:length(y_starts)){
  if(i %% 2 == 0) {
    x_all <- append(x_all,x_offset)
  }
    else
      {
        x_all <- append(x_all,x_zero)
  }
}
# make the full grid
points <- cbind(x_all, rep(y_starts, each=length(x_zero)))

# check with a simple plot
par(mar=c(3,3,1,1))
plot(points,asp=1, pch=3)
lines(SVbound, col="red3")
