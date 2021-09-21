library(geoR)
shape <- read.csv("sv_boundary.csv")
str(shape)
extremes <- c(min(floor(shape$Easting)),
              min(floor(shape$Northing)),
              max(ceiling(shape$Easting)),
              max(ceiling(shape$Northing)))

par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.6, 0.3, 0), 
    tcl = 0.25, font.lab = 2, lend=1, ljoin=2)
with(shape, plot(Easting, Northing, asp = 1, type = "l"))
rect(extremes[1], extremes[2], 
     extremes[3], extremes[4], 
     border = "tan", lty = 2)

xlist <- seq(extremes[1], extremes[3], 1)
ylist <- seq(extremes[2], extremes[4], 1)

np <- 100
inside <- data.frame(E = rep(NA, np), N = rep(NA, np))
j = 1

# while(j<100)

for(i in 1:np * 10) {
  data0 <- data.frame(E = sample(xlist, 1), N = sample(ylist, 1))
  check <- locations.inside(locations = data0,
                            borders = shape[, c("Easting", "Northing")])
  if (class(check[1, 1]) == "numeric") {
    inside[j,] <- data0[1,]
    j <- j + 1
    print(j)
  }
  if (j > np) {
    break
    
  }
}
points(inside[,1:2], pch = 3, col = "red3")
legend("bottomleft", bty = "o", inset = 0.05, cex = 0.8,
       box.col = "grey", bg = "white",
       legend = c("Irregular polygon", "Max. extent", "100 random points"),
       pch = c(NA,NA,3), col = c(1, "tan", "red3"), lty = c(1,2,NA))

rm(list = c("xlist", "ylist", "np", "j", "data0", "check"))
