# Project location of detection for receivers, by antenna, 
detection_loc <- function(lat, lon, bearing, distkm) {
  out <- maptools::gcDestination(lon, lat, bearing, distkm, "km", "WGS84", TRUE)
  colnames(out) = c("det_lon", "det_lat")
  rownames(out) <- NULL
  return(as.data.frame(out))
} 