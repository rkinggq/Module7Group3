library(tidyverse)
library(rgbif)
library(lubridate)
library(lme4)
library(MuMIn)
library(ggmap)
library(usmap)
library(data.table)
library(car)


# Example species: "TGM" (use actual species name or IDs)
species <- "Baltimore Oriole"

# Download eBird data for the specified species
ebird_data <- occ_data(scientificName = species, 
                       stateProvince = "Massachusetts", 
                       country = "US",
                       basisOfRecord = "HUMAN_OBSERVATION")$data

# Filter and prepare data
ebird_clean <- ebird_data %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  mutate(date = as.Date(eventDate), 
         year = year(date),
         j_day = yday(date))

# NOAA station IDs
stations <- c("Mobile, AL" = "GHCND:USW00013874",
              "Charlotte, NC" = "GHCND:USW00013881",
              "Boston, MA" = "GHCND:USW00014739")

# Download weather data for key locations
weather_list <- lapply(stations, function(station) {
  ncdc(datasetid = "GHCND", 
       stationid = station, 
       startdate = "2018-01-01", 
       enddate = "2018-12-31",
       token = "YOUR_NOAA_TOKEN")$data
})

# Combine weather data
weather_data <- bind_rows(weather_list, .id = "location") %>%
  mutate(date = as.Date(date), 
         year = year(date),
         j_day = yday(date)) %>%
  group_by(location) %>%
  mutate(wdir_rad = abs(wdf2 - 180) * pi / 180,
         wvec = -cos(wdir_rad) * awnd) %>%
  ungroup()

# Combine eBird and weather data, offset by migratory timing
merged_data <- ebird_clean %>%
  left_join(weather_data, by = c("date" = "date", "location")) %>%
  mutate(j_day_offset = case_when(
    location == "Mobile, AL" ~ j_day - 10,
    location == "Charlotte, NC" ~ j_day - 5,
    TRUE ~ j_day
  ))

# Fit linear mixed-effects model
lmm <- lmer(j_day ~ tmin + tmax + wvec + (1 | location), data = merged_data)

# Summarize results
summary(lmm)

# ANOVA for fixed effects
Anova(lmm, type = "III")

# Generate all model combinations
dredged_models <- dredge(lmm, fixed = c("tmin", "tmax", "wvec"))

# Best model
best_model <- get.models(dredged_models, subset = 1)
summary(best_model)

# Logistic growth modeling
log_model <- nls(j_day ~ SSlogis(j_day, Asym, xmid, scal), data = merged_data)
summary(log_model)

# Simulate climate change scenarios
future_conditions <- merged_data %>%
  mutate(tmin = tmin + 2,  # Example temperature increase
         tmax = tmax + 2)

future_pred <- predict(log_model, newdata = future_conditions)

# Compare with current arrival timing
compare_arrival <- data.frame(Current = merged_data$j_day, Future = future_pred)






