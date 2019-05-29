# Project location of detection for receivers, by antenna, 
detection_loc <- function(lat, lon, bearing, elems = 9, distkm = NULL) {
  if (is.null(distkm))
    distkm <- elems/3
  out <- maptools::gcDestination(lon, lat, bearing, distkm, "km", "WGS84", TRUE)
  colnames(out) = c("det_lon", "det_lat")
  rownames(out) <- NULL
  return(as.data.frame(out))
} 