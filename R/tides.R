# function for creating tidal sine wave
# beta = amplitude
# period = length (h) of tidal period, in h
# phi = phase shift, in h
# start = start series at high tide or low tide
# Uses cosine to start time series at high tide
tides <- function(time_in, beta = 1, period = 12 + 25/60, phi = 0, 
                  start = c("high", "low")){
  
  start <- match.arg(start)
  start <- ifelse(start == "low", period/2, 0)
  
  # timestep per hour
  time_step <- 60 / unique(diff(time_in))
  
  # set phi as difference in hours from start of time_in
  phi  <- min(time_in) + phi * 3600
  phi <- as.numeric(difftime(phi, min(time_in), units = "hours"))

  # get input values to function
  in_vals <- seq(0, length(time_in), length = length(time_in))
  in_vals <- in_vals / time_step

  # tide cycle
  y <- beta * cos(2*pi*(in_vals - phi - start) / period)
  return(y)
}