pacman::p_load(readxl, readr, dplyr, lubridate, oce)
sheets <- "Data/StrMag_tides_2018.xlsx" %>% excel_sheets()
# Tides from: https://tides4fishing.com/ar/tierra-del-fuego/estrecho-de-magallanes
tides <- lapply(sheets, function(i) {
  tmp <- read_xlsx("Data/StrMag_tides_2018.xlsx", i) %>%
    mutate(date = ymd(paste(2018, i, SORT))) %>%
    select(date, TM1:TM4, HT1:HT4) %>%
    as.data.frame() %>%
    reshape(idvar = "date", direction = "long",
            varying = list(time = 2:5, height = 6:9),
            v.names = c("tidetime", "height")) %>%
    filter(!is.na(height)) %>%
    mutate(dt = ymd_hm(paste(date, gsub(" am| pm", "", tidetime)), tz = "UTC") +
             as.difftime(3, units = "hours")) %>%
    select(dt, height) %>%
    arrange(dt)
})
tides <- bind_rows(tides)

tides_sl <- as.sealevel(elevation = tides$height, time = tides$dt)
# plot(tide_sl)

# Primary tidal consituents
# https://tidesandcurrents.noaa.gov/about_harmonic_constituents.html
constituents <- c('M2', 'S2', 'N2', 'K2', 'K1', 'O1', 'P1')

# Estimate all components (including many minor ones)
tide_model_t4f <- tidem(tides_sl)
# save(tide_model_t4f, file = "Output/tide_model_t4f.rda")

# # Get components of interest
# amps <- data.frame(mod@data[c('name', 'amplitude')]) %>%
#   filter(name %in% constituents) %>%
#   arrange(amplitude)
# amps

# Add predictions
tides$estimated <- predict(tide_model_t4f)

# Compare "actual" vs. predicted
with(tides, plot(height, estimated))

# Tides from Patagonia Shelf model
# http://volkov.oce.orst.edu/tides/region.html
pats <- readr::read_fwf("Data/Bahia_Lomas_PatShelf_model.txt", 
                        fwf_empty("Data/Bahia_Lomas_PatShelf_model.txt", skip = 1,
                                  col_names = c("Y", "M", "D", "H", "Min", "height")),
                        skip = 1) %>%
  mutate(dt = ymd_hm(paste(paste(Y, M, D, sep = "-"), paste(H, Min, sep = ":"))))

# Compare to tides4fishing data
pats_t4f <- predict(tide_model, newdata = pats$dt)
plot(pats$height, pats_t4f)

# Use Patagonia Shelf model
pats_sl <- as.sealevel(elevation = pats$height, time = pats$dt)
tide_model <- tidem(pats_sl)
# Add predictions
pats$estimated <- predict(tide_model)

# Compare "actual" vs. predicted
with(pats, plot(height, estimated))
save(tide_model, file = "Output/tide_model.rda")
