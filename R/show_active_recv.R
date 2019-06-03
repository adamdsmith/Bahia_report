show_active_recv <- function(base = 0, scale = 5, id = "id", spp = "HUGO", NA_inactive = FALSE, input = dly_recv_log) {
  out <- mutate(input,
                # Add relative, but generic "Y-axis" values for use in adding to plots below
                y = case_when(
                  recvSiteName == "E. Pepita"     ~ base + 3 * scale,
                  recvSiteName == "E. MiraMar"    ~ base + 2 * scale,
                  recvSiteName == "E. El Pantano" ~ base + 1 * scale,
                  TRUE                            ~ base),
                id = recvSiteName)
  if (!NA_inactive) 
    out <- filter(out, active)
  else
    out <- mutate(out, y = ifelse(active, y, NA))
  out <- reshape::expand.grid.df(out, data.frame(spp = spp))
  names(out)[5] <- id
  out
}
  