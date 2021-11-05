# function rectBySpacing to populate irregular polygon with 
# rectangular grid points at defined x (and optionally y) spacings
rectBySpacing <-
  function(boundary,
           xspacing,
           yspacing = xspacing,
           xoffset = 0,
           yoffset = xoffset,
           proj4 = NULL
  ) {
    require(sp)
    
    # if projection not defined, set to EPSG:4326 (LongLat)
    if (is.null(proj4)) {
      p4 <-
        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
    } else {
      p4 <- CRS(proj4)
    }
    
    # make boundary coords into SpatialPolygons object
    irregpoly <- Polygon(boundary[, c(1,2)], hole = F)
    irregPolys = Polygons(list(irregpoly),1)
    irregSpatialPolys = SpatialPolygons(list(irregPolys), 
                          proj4string = p4)
    
    # define limirs of initial (maximal) grid
    extremes <- c(min(floor(boundary[, 1])),
                  min(floor(boundary[, 2])),
                  max(ceiling(boundary[, 1])),
                  max(ceiling(boundary[, 2])))
    # calculate areas
    area <- irregpoly@area
    maxarea <-
      (extremes[3] - extremes[1]) * (extremes[4] - extremes[2])
    
    # make initial (maximal) grid
    grid_big <- expand.grid(
      seq(extremes[1],
          extremes[3], by = xspacing) + xoffset,
      seq(extremes[2],
          extremes[4], by = yspacing) + yoffset
    )
    # convert initial grid to SpatialPoints object
    gridSP <- SpatialPoints(grid_big, proj4string = p4)

    # user over() function from sp package to mask initial grid
    inOrOut <- as.vector(over(gridSP, irregSpatialPolys))
    insideSP <- gridSP[which(inOrOut>0)]
    # convert masked coordinates back into data frame
    inside <- as.data.frame(insideSP@coords)
    colnames(inside) <- c("x", "y")
    
    # make output object
  output <-
    list(
      points_in = inside,
      x_spacing = xspacing,
      y_spacing = yspacing,
      x_offset = xoffset,
      y_offset = yoffset,
      npoints = NROW(inside),
      max_points = NROW(grid_big),
      shape_area = area,
      max_area = maxarea,
      extremes = c(min(floor(boundary[, 1])),
                    min(floor(boundary[, 2])),
                    max(ceiling(boundary[, 1])),
                    max(ceiling(boundary[, 2]))),
      proj_4_str = p4
    )
  return(output)
  }

# testing the rectBySpacing function
SVbound <- read.csv("sv_boundary.csv")
RFbound <- read.csv("RFboundary.csv")
RFdesign <-
  rectBySpacing(RFbound,
                xspacing = 320,
                yspacing = 320,
                xoffset = 5,
                yoffset = 50,
                proj4 = "+proj=utm +zone=50 +south")
str(RFdesign)
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
# check with a plot
par(
  mfrow = c(1, 1),
  mar = c(3, 3, 1, 1),
  mgp = c(1.6, 0.3, 0),
  tcl = 0.25,
  font.lab = 2,
  lend = 2,
  ljoin = 1,
  cex.axis = 0.8
)
with(RFbound, plot(Easting, Northing, asp = 1, type = "l"))
rect(RFdesign$extremes[1], RFdesign$extremes[2],
     RFdesign$extremes[3], RFdesign$extremes[4],
     border = "chocolate",
     lty = 2)
points(RFdesign$points_in,
       col = "purple",
       pch = 3,
       cex = 0.8)

legend(
  "topleft",
  inset = 0.04,
  y.intersp = 1.2,
  bty = "o",
  box.col = 8,
  title = expression(bold("Rectangular Grid Sampling Design")),
  legend = c(
    paste0("Spacing = ", RFdesign$x_spacing,"\u00D7",RFdesign$y_spacing), 
    paste0("  offset = ", RFdesign$x_offset,"\u00D7",RFdesign$y_offset),
    paste("Irregular polygon", signif(RFdesign$shape_area, 5), "m\u00B2"),
    paste("Max. extent", signif(RFdesign$max_area, 6), "m\u00B2")
  ),
  cex = 0.9, pch = c(3, NA, NA, NA),
  col = c("purple", NA, 1, "chocolate"),
  lty = c(NA, NA, 1, 2)
)
mtext(paste(
  "Polygon contains",
  RFdesign$npoints,
  "points from maximal grid of",
  RFdesign$max_points, "points"), 
  3, -1, col = "grey50", font = 3)
mtext("UWA Ridgefield Farm, Pingelly", 
  side = 3, line = 0.1, 
  col = "#27348B", font = 2, 
  cex = 0.9, family = "serif")
