# function to convert kml files saved from Google Earth
# to 
kmlconvert <- function(file = NULL, utmzone = 50, hemi = "south") {
  require(sp)
  require(maptools)
  LongLat <- CRS("+proj=longlat +ellps=WGS84 
                 +datum=WGS84 +no_defs")
  UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +",hemi))

# NB: depending on the type of kml file, the output is:
  # a list of single lon-lat pairs for kml of individual points
  # a list containing a matrix/array of lon-lat for kml of a path,
  # line, or polygon

kml_0 <- getKMLcoordinates(file, ignoreAltitude = T)
if(is.matrix(kml_0[[1]])){
    LL_0 <- SpatialPoints(matrix(unlist(kml_0),
                                 length(unlist(kml_0))/2,2), 
                          proj4string = LongLat)
    colnames(LL_0@coords) <- c("Longitude", "Latitude")
    utm_0 <- spTransform(LL_0, CRSobj = UTM)
    colnames(utm_0@coords) <- c("Easting", "Northing")
    rslt_0 <- as.data.frame(cbind(utm_0@coords, LL_0@coords))
    cat("Path, line, or polygon\n")
    print(rslt_0, digits=8, row.names=F)
    write.table(rslt_0, file="clipboard", row.names = F, sep="\t")
    cat("\nOutput data also copied to clipboard\n")
  } else {
    LL_0 <- SpatialPoints(matrix(unlist(kml_0),
                                 length(kml_0),2,
                                 byrow = T), 
                          proj4string = LongLat)
    colnames(LL_0@coords) <- c("Longitude", "Latitude")
    utm_0 <- spTransform(LL_0, CRSobj = UTM)
    colnames(utm_0@coords) <- c("Easting", "Northing")
    rslt_0 <- as.data.frame(cbind(utm_0@coords, LL_0@coords))
    cat("Individual points\n")
    print(rslt_0, digits=8, row.names=F)
    write.table(rslt_0, file="clipboard", row.names = F, sep="\t")
    cat("\nOutput data also copied to clipboard\n")
  }
return(rslt_0)
rm(list = ls(pattern = "_0"))
}
