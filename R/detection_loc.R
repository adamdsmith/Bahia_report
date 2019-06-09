# Project location of detection for receivers, by antenna, 
detection_loc <- function(lat, lon, bearing, elems = 9, distkm = NULL) {
  if (is.null(distkm))
    distkm <- elems/3
  out <- maptools::gcDestination(lon, lat, bearing, distkm, "km", "WGS84", TRUE)
  colnames(out) = c("det_lon", "det_lat")
  rownames(out) <- NULL
  return(as.data.frame(out))
} 

# Create ellipse geometry in list-column 
# For use as rowwise mutate
make_ellipse <- function(x, y, brng, len_km = 2, wid_km = 1.6, crs = 32719) {
  # Put axes lengths in km
  maj <- len_km * 1000 / 2
  min <- wid_km * 1000 / 2
  # Convert bearing to radians
  phi <- pi/180 * (90 - brng)
  # Find ellipse center
  xc <- x + cos(phi) * maj
  yc <- y + sin(phi) * maj
  # Points on a circle
  t <- seq(0, 2 * pi, length.out = 500)
  # Create ellipse
  xi <- xc + maj*cos(t)*cos(phi) - min*sin(t)*sin(phi)
  yi <- yc + maj*cos(t)*sin(phi) + min*sin(t)*cos(phi)
  xi <- c(xi, xi[1])
  yi <- c(yi, yi[1])
  out <- st_polygon(list(cbind(xi,yi)))
  st_sfc(out, crs = crs)
}
