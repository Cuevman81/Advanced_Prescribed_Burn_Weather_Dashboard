# =============================================================================
# ENHANCED PRESCRIBED BURN WEATHER DASHBOARD APP
# Version 2.0 - Comprehensive Edition
# =============================================================================

# -----------------------------------------------------------------------------
# 1. PREAMBLE - LOAD LIBRARIES
# -----------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(jsonlite)
library(weathR)
library(ggplot2)
library(plotly)
library(tidyr)
library(lubridate)
library(tidygeocoder)
library(DT)
library(sf)
library(purrr)
library(httr)
library(stringr)
library(tigris)
library(shinycssloaders)
library(leaflet)
library(riem)


# Load the pre-computed station list when the app starts.
us_stations_list <- readRDS("us_stations.rds")

# -----------------------------------------------------------------------------
# 2. HELPER FUNCTIONS FOR BURN CONDITIONS
# -----------------------------------------------------------------------------

# --- START: NEW, INSTANTANEOUS FUNCTION FOR FINDING STATIONS ---


find_nearest_station <- function(target_lat, target_lon) {
  
  # The function now directly uses the 'us_stations_list' object we loaded at startup.
  # No API calls, no memoise, no delay.
  
  # Calculate distance to all stations in the pre-loaded list
  us_stations_with_distance <- us_stations_list %>%
    mutate(
      distance = sqrt((target_lon - lon)^2 + (target_lat - lat)^2)
    )
  
  # Find and return the station with the minimum distance
  closest_station <- us_stations_with_distance %>%
    filter(distance == min(distance, na.rm = TRUE)) %>%
    slice(1)
  
  return(closest_station)
}


# Fetches the latest observation from the nearest station
get_realtime_conditions <- function(lat, lon) {
  tryCatch({
    # Step 1: Find the nearest station
    station_info <- find_nearest_station(lat, lon)
    if (is.null(station_info) || nrow(station_info) == 0) return(NULL)
    
    station_id <- station_info$id
    
    # Step 2: Get all of today's measurements for that station
    todays_data <- riem_measures(station = station_id, date_start = Sys.Date())
    if (is.null(todays_data) || nrow(todays_data) == 0) return(NULL)
    
    # Step 3: Get the single most recent observation (the last row)
    latest_obs <- todays_data %>%
      slice_tail(n = 1) %>%
      # Step 4: Rename columns to match what the UI expects
      rename(
        temp = tmpf,
        humidity = relh
      ) %>%
      # Step 5: Convert wind from knots to mph for consistency
      mutate(
        wind_speed = round(sknt * 1.15078, 0)
      )
    
    return(list(data = latest_obs, station = station_info))
    
  }, error = function(e) {
    message("Could not retrieve real-time data from riem.")
    print(e)
    return(NULL)
  })
}


# --- START: NEW FUNCTION FOR HMS DATA ---
# Fetches and processes NOAA HMS fire and smoke shapefiles for a given date
get_hms_data <- function(selected_date) {
  year <- format(selected_date, "%Y")
  month <- format(selected_date, "%m")
  day_str <- format(selected_date, "%Y%m%d")
  temp_dir <- file.path(tempdir(), paste0("hms_data_", day_str))
  if (!dir.exists(temp_dir)) { dir.create(temp_dir, recursive = TRUE) }
  
  fire_sf <- NULL
  smoke_sf <- NULL
  
  # --- Fetch Fire Points Data ---
  tryCatch({
    fire_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Shapefile/", year, "/", month, "/hms_fire", day_str, ".zip")
    fire_zip_path <- file.path(temp_dir, "hms_fire.zip")
    download.file(fire_url, fire_zip_path, mode = "wb", quiet = TRUE)
    unzip(fire_zip_path, exdir = temp_dir)
    shp_file <- list.files(temp_dir, pattern = "_fire.*\\.shp$", full.names = TRUE)
    if (length(shp_file) > 0) {
      fire_sf <- st_read(shp_file[1], quiet = TRUE) %>% st_transform(4326)
    }
  }, error = function(e) { message("Could not download or process HMS fire data for ", selected_date) })
  
  # --- Fetch Smoke Polygons Data ---
  tryCatch({
    smoke_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/", year, "/", month, "/hms_smoke", day_str, ".zip")
    smoke_zip_path <- file.path(temp_dir, "hms_smoke.zip")
    download.file(smoke_url, smoke_zip_path, mode = "wb", quiet = TRUE)
    unzip(smoke_zip_path, exdir = temp_dir)
    shp_file <- list.files(temp_dir, pattern = "_smoke.*\\.shp$", full.names = TRUE)
    
    if (length(shp_file) > 0) {
      smoke_sf_raw <- st_read(shp_file[1], quiet = TRUE) %>% st_transform(4326)
      
      if (is.numeric(smoke_sf_raw$Density)) {
        smoke_sf <- smoke_sf_raw %>%
          mutate(DensityCategory = case_when(
            Density >= 27 ~ "Heavy",
            Density >= 16 ~ "Medium",
            Density >= 5  ~ "Light",
            TRUE ~ "Unknown"
          ) %>% 
            factor(levels = c("Light", "Medium", "Heavy", "Unknown")))
      } else {
        smoke_sf <- smoke_sf_raw %>%
          mutate(DensityCategory = 
                   factor(Density, levels = c("Light", "Medium", "Heavy", "Unknown")))
      }
    }
  }, error = function(e) { message("Could not download or process HMS smoke data for ", selected_date) })
  
  return(list(fire = fire_sf, smoke = smoke_sf))
}

# Calculate Keetch-Byram Drought Index (simplified version)
calculate_kbdi_trend <- function(temp, humidity) {
  result <- ifelse(is.na(temp) | is.na(humidity), 
                   NA_real_, 
                   (100 - humidity) * 2 + (temp - 60) * 0.5)
  return(round(result, 0))
}

# Check for burn bans and permit requirements
check_burn_restrictions <- function(lat, lon, state_abbr = NULL) {
  # This would ideally connect to state fire marshal APIs
  # For now, return a structured message
  restrictions <- list(
    status = "Check Local Authorities",
    message = "Contact your local fire department or forestry service for current burn restrictions.",
    permit_required = TRUE,
    links = list(
      "InciWeb (Active Fires)" = "https://inciweb.nwcg.gov/",
      "NOAA Fire Weather Outlooks" = "https://www.spc.noaa.gov/products/fire_wx/"
    )
  )
  return(restrictions)
}

# Calculate Fine Fuel Moisture Code (Canadian Forest Fire Weather Index component)
calculate_ffmc <- function(temp, humidity, wind_speed) {
  result <- ifelse(is.na(temp) | is.na(humidity) | is.na(wind_speed), 
                   NA_real_, 
                   85 + (temp - 60) * 0.3 - (humidity - 45) * 0.5 + wind_speed * 0.1)
  result <- pmax(0, pmin(100, result))
  return(round(result, 1))
}

# Calculate time-lag fuel moisture based on temp and humidity
calculate_fuel_moisture <- function(temp, humidity, days_since_rain = 0) {
  # This function is not vectorized and should be called with rowwise()
  if (is.na(temp) || is.na(humidity) || humidity <= 0) {
    return(list(one_hr = NA, ten_hr = NA, hundred_hr = NA))
  }
  
  # Simplified equilibrium moisture content calculation
  emc <- (1.62 * humidity^0.72) + (0.0012 * humidity * temp) + 0.62
  
  # Adjust for days since rain (simplified)
  # Longer time since rain should LOWER the moisture content
  moisture_1hr <- emc
  moisture_10hr <- emc - (days_since_rain * 0.5) # CORRECTED: Subtraction
  moisture_100hr <- emc - (days_since_rain * 2)  # CORRECTED: Subtraction
  
  return(list(
    # Use pmax to ensure moisture doesn't go below a realistic minimum (e.g., 2%)
    one_hr = round(pmax(2, pmin(35, moisture_1hr)), 1),
    ten_hr = round(pmax(2, pmin(35, moisture_10hr)), 1),
    hundred_hr = round(pmax(2, pmin(35, moisture_100hr)), 1)
  ))
}

# Calculate Probability of Ignition (based on NFDRS)
calculate_ignition_probability <- function(temp, humidity, fuel_moisture_1hr) {
  result <- ifelse(is.na(temp) | is.na(humidity) | is.na(fuel_moisture_1hr), 
                   NA_real_, 
                   100 - (fuel_moisture_1hr * 2.5))
  result <- pmax(0, pmin(100, result))
  return(round(result, 0))
}

# Determine smoke dispersion category based on stability class
determine_dispersion_category <- function(mixing_height_ft, transport_wind_mph, time_of_day) {
  # This function is not vectorized and should be called with rowwise()
  if (is.na(mixing_height_ft) || is.na(transport_wind_mph) || is.na(time_of_day)) {
    return(list(category = "Poor", adjusted_vi = NA, description = "Missing data"))
  }
  
  vi <- mixing_height_ft * transport_wind_mph
  
  # Adjust for time of day (stability class approximation)
  stability_factor <- case_when(
    time_of_day >= 10 & time_of_day <= 15 ~ 1.0,  # Most unstable
    time_of_day >= 7 & time_of_day < 10 ~ 0.8,
    time_of_day >= 15 & time_of_day <= 18 ~ 0.8,
    TRUE ~ 0.5  # Night - most stable
  )
  
  adjusted_vi <- vi * stability_factor
  
  category <- case_when(
    adjusted_vi >= 60000 ~ "Excellent",
    adjusted_vi >= 40000 ~ "Good",
    adjusted_vi >= 20000 ~ "Fair",
    adjusted_vi >= 10000 ~ "Poor",
    TRUE ~ "Very Poor"
  )
  
  return(list(
    category = category,
    adjusted_vi = round(adjusted_vi, 0),
    description = case_when(
      category == "Excellent" ~ "Rapid smoke dispersal, minimal impacts",
      category == "Good" ~ "Good dispersal, smoke clears quickly",
      category == "Fair" ~ "Moderate dispersal, some smoke pooling possible",
      category == "Poor" ~ "Limited dispersal, smoke sensitive areas at risk",
      TRUE ~ "Minimal dispersal, avoid burning"
    )
  ))
}

# Determine burn window quality
assess_burn_window <- function(temp, humidity, wind_speed, mixing_height_ft, ventilation_index) {
  
  # --- NA Check ---
  # If any key input is NA, we cannot assess. Return "Poor" immediately.
  if (is.na(temp) || is.na(humidity) || is.na(wind_speed) || is.na(ventilation_index)) {
    return(list(score = 0, category = "Poor", reasons = "Missing data"))
  }
  
  score <- 0
  reasons <- character()
  
  # Temperature (ideal: 40-80°F)
  if (temp >= 40 && temp <= 80) {
    score <- score + 25
  } else if (temp >= 30 && temp <= 85) {
    score <- score + 15
  } else {
    reasons <- c(reasons, "Temperature outside ideal range")
  }
  
  # Humidity (ideal: 30-55%)
  if (humidity >= 30 && humidity <= 55) {
    score <- score + 25
  } else if (humidity >= 25 && humidity <= 60) {
    score <- score + 15
  } else {
    reasons <- c(reasons, "Humidity outside ideal range")
  }
  
  # Wind speed (ideal: 4-15 mph)
  if (wind_speed >= 4 && wind_speed <= 15) {
    score <- score + 25
  } else if (wind_speed >= 2 && wind_speed <= 20) {
    score <- score + 15
  } else {
    reasons <- c(reasons, "Wind speed not optimal")
  }
  
  # Ventilation Index (ideal: > 40,000)
  if (ventilation_index >= 40000) {
    score <- score + 25
  } else if (ventilation_index >= 20000) {
    score <- score + 15
  } else {
    reasons <- c(reasons, "Poor smoke dispersal conditions")
  }
  
  # Determine category
  category <- if (score >= 90) { "Excellent" } 
  else if (score >= 70) { "Good" } 
  else if (score >= 50) { "Fair" } 
  else if (score >= 30) { "Marginal" } 
  else { "Poor" }
  
  return(list(score = score, category = category, reasons = reasons))
}

# Convert cloud cover percentage to NWS abbreviations
get_sky_cover_abbr <- function(sky_cover) {
  case_when(
    is.na(sky_cover) ~ "CLR",
    sky_cover <= 10 ~ "CLR",
    sky_cover <= 30 ~ "FW",
    sky_cover <= 50 ~ "PC",
    sky_cover <= 70 ~ "MC",
    sky_cover <= 90 ~ "MCR", # Changed from MC to MCR for Mostly Cloudy
    TRUE ~ "OVC"
  )
}

# Format time table for spot forecast
format_spot_time_table <- function(df, start_hour, num_hours) {
  df_subset <- df %>%
    filter(hour(time) >= start_hour) %>%
    head(num_hours)
  
  if(nrow(df_subset) == 0) return(NULL)
  
  if (!"wind_gust" %in% names(df_subset)) { df_subset$wind_gust <- NA_real_ }
  if (!"transport_wind_dir" %in% names(df_subset)) { df_subset$transport_wind_dir <- df_subset$wind_dir }
  
  # Create clean, fixed-width time labels like "8P", "10A", etc.
  time_labels <- format(df_subset$time, "%l%p") %>% 
    gsub(" AM", "A", .) %>% 
    gsub(" PM", "P", .) %>%
    trimws()
  
  list(
    times = paste(sprintf("%-4s", time_labels), collapse = ""),
    sky_cover = paste(sprintf("%-4s", sapply(df_subset$sky_cover, get_sky_cover_abbr)), collapse = ""),
    weather_type = paste(sprintf("%-4s", sapply(df_subset$weather_code, get_weather_abbr)), collapse = ""),
    temp = paste(sprintf("%-4d", round(df_subset$temp)), collapse = ""),
    rh = paste(sprintf("%-4d", round(df_subset$humidity)), collapse = ""),
    wind_dir = paste(sprintf("%-4s", df_subset$wind_dir), collapse = ""),
    wind_spd = paste(sprintf("%-4d", round(df_subset$wind_speed)), collapse = ""),
    wind_gust = paste(sprintf("%-4d", round(df_subset$wind_gust)), collapse = ""),
    mix_hgt_m = paste(sprintf("%-4d", round(df_subset$mixing_height_m)), collapse = ""),
    trans_wind_dir = paste(sprintf("%-4s", df_subset$transport_wind_dir), collapse = ""),
    trans_wind_spd = paste(sprintf("%-4d", round(df_subset$transport_wind_mph)), collapse = ""),
    trans_spd_ms = paste(sprintf("%-4d", round(df_subset$transport_wind_ms)), collapse = "")
  )
}

# Generate discussion text based on conditions
generate_spot_discussion <- function(df) {
  next_12hr <- df %>% head(12)
  discussion_parts <- character()
  
  # Check for high winds or gusts
  if(any(next_12hr$wind_speed > 15, na.rm = TRUE) || any(next_12hr$wind_gust > 20, na.rm = TRUE)) {
    discussion_parts <- c(discussion_parts, sprintf("Winds may gust up to %d mph. ", max(next_12hr$wind_gust, na.rm = TRUE)))
  }
  
  # Check for precipitation
  if("precipitation_prob" %in% names(next_12hr) && any(next_12hr$precipitation_prob > 30, na.rm = TRUE)) {
    max_precip_time <- next_12hr$time[which.max(next_12hr$precipitation_prob)]
    discussion_parts <- c(discussion_parts, sprintf("A chance of precipitation exists, especially around %s. ", format(max_precip_time, "%I %p")))
  }
  
  # Check for poor dispersion
  if(any(next_12hr$dispersion_category %in% c("Poor", "Very Poor"), na.rm = TRUE)) {
    discussion_parts <- c(discussion_parts, "Poor smoke dispersion is possible. ")
  }
  
  if(length(discussion_parts) == 0) {
    discussion_parts <- "No significant weather concerns are expected."
  }
  
  return(paste(".DISCUSSION...", paste(discussion_parts, collapse = "")))
}

# Translate API weather codes to NWS-style abbreviations
get_weather_abbr <- function(weather_code) {
  if (is.na(weather_code) || weather_code == "") return(" ")
  # Look for keywords in the weather string
  case_when(
    grepl("thunderstorm", weather_code, ignore.case = TRUE) ~ "T",
    grepl("rain|drizzle", weather_code, ignore.case = TRUE) ~ "RW",
    grepl("snow|flurries", weather_code, ignore.case = TRUE) ~ "S",
    grepl("fog", weather_code, ignore.case = TRUE) ~ "F",
    grepl("smoke", weather_code, ignore.case = TRUE) ~ "K",
    grepl("haze", weather_code, ignore.case = TRUE) ~ "H",
    TRUE ~ " "
  )
}

# Fetches the official NWS Fire Weather Planning Forecast Discussion
get_fire_weather_discussion <- function(nws_office_code) {
  tryCatch({
    api_url <- paste0("https://api.weather.gov/products/types/FWF/locations/", nws_office_code)
    
    res_list <- httr::GET(api_url, timeout(5)) # Add a timeout for safety
    if (status_code(res_list) != 200) return("Fire weather discussion not available.")
    
    product_url <- fromJSON(content(res_list, "text", encoding = "UTF-8"))$`@graph`$`@id`[1]
    if (is.null(product_url)) return("Fire weather discussion not available.")
    
    res_product <- httr::GET(product_url, timeout(5))
    if (status_code(res_product) != 200) return("Fire weather discussion not available.")
    
    product_text <- fromJSON(content(res_product, "text", encoding = "UTF-8"))$productText
    
    # --- IMPROVED LOGIC ---
    # 1. Use a more precise regex to find the .DISCUSSION... block and stop
    #    before the next major section (like .TONIGHT... or $$)
    discussion <- str_extract(product_text, "\\.DISCUSSION...[\\s\\S]*?(?=\\n\\n\\.|&&|\\$\\$)")
    
    # 2. If the discussion is found, just trim the whitespace. Do NOT remove the
    #    internal line breaks, which preserves the paragraph formatting.
    if (!is.na(discussion)) {
      return(trimws(discussion))
    } else {
      return("Official discussion section not found in the latest product.")
    }
    
  }, error = function(e) {
    return("Could not retrieve official fire weather discussion.")
  })
}

get_fire_weather_zone_forecast_by_city <- function(full_product_text, location_text) {
  # Extract the primary city name from the user's input (e.g., "Jackson, MS" -> "Jackson")
  city_name <- trimws(strsplit(location_text, ",")[[1]][1])
  if (is.null(city_name) || city_name == "") return("Invalid location name provided.")
  
  # Split the entire text product by the "$$" delimiter, which separates zones
  sections <- strsplit(full_product_text, "\\$\\$")[[1]]
  
  # Find the section that contains our specific city name in the "Including the cities of" line
  for (section in sections) {
    # MODIFIED: This regex is now more specific. It ensures the city name appears
    # AFTER "Including the cities of" to avoid matching the NWS office name in the header.
    search_pattern <- regex(paste0("Including the cities of[\\s\\S]*?\\b", city_name, "\\b"), ignore_case = TRUE)
    
    if (stringr::str_detect(section, search_pattern)) {
      # If we find a match, clean it up and return it
      return(trimws(section))
    }
  }
  
  # If no specific section was found
  return(paste0("A zone-specific trend forecast for '", city_name, "' was not found in the main NWS product."))
}

# NEW FUNCTION: Get Air Quality Data from AirNow API
get_air_quality_data <- function(lat, lon, api_key) {
  # If the API key is missing, stop immediately.
  if (is.null(api_key) || api_key == "") {
    print("AirNow API key is missing.")
    return(NULL)
  }
  
  # Construct the API request URL (CORRECTED: removed /v3/)
  api_url <- paste0(
    "https://www.airnowapi.org/aq/forecast/latLong/?format=application/json&",
    "latitude=", lat, "&longitude=", lon, "&distance=25&",
    "API_KEY=", api_key
  )
  
  tryCatch({
    # Make the web request
    response <- httr::GET(api_url)
    
    # Check for a successful response
    if (httr::status_code(response) == 200) {
      # Parse the JSON content
      aqi_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      return(aqi_data)
    } else {
      print(paste("AirNow API request failed with status:", httr::status_code(response)))
      return(NULL)
    }
    
  }, error = function(e) {
    print("An error occurred while fetching Air Quality data.")
    print(e)
    return(NULL)
  })
}

# NEW FUNCTION: Get CURRENT Air Quality from AirNow API
get_current_air_quality <- function(lat, lon, api_key) {
  if (is.null(api_key) || api_key == "") {
    print("AirNow API key is missing.")
    return(NULL)
  }
  
  # Note the different URL endpoint for current observations
  api_url <- paste0(
    "https://www.airnowapi.org/aq/observation/latLong/current/?format=application/json&",
    "latitude=", lat, "&longitude=", lon, "&distance=25&",
    "API_KEY=", api_key
  )
  
  tryCatch({
    response <- httr::GET(api_url)
    if (httr::status_code(response) == 200) {
      jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    } else {
      print(paste("AirNow Current Observation API request failed with status:", httr::status_code(response)))
      return(NULL)
    }
  }, error = function(e) {
    print("An error occurred while fetching Current Air Quality data.")
    print(e)
    return(NULL)
  })
}

# NEW HELPER: Get color for AQI category
get_aqi_color <- function(category_name) {
  color <- case_when(
    category_name == "Good" ~ "#00e400",          # Green
    category_name == "Moderate" ~ "#ffff00",     # Yellow
    category_name == "Unhealthy for Sensitive Groups" ~ "#ff7e00", # Orange
    category_name == "Unhealthy" ~ "#ff0000",      # Red
    category_name == "Very Unhealthy" ~ "#8f3f97",  # Purple
    category_name == "Hazardous" ~ "#7e0023",     # Maroon
    TRUE ~ "#A9A9A9"                             # Default Gray
  )
  return(color)
}

#Get Air Quality Monitoring Sites for a State

get_active_monitors_by_state <- function(state_code, api_key) {
  if (is.null(api_key) || api_key == "" || is.null(state_code) || state_code == "") {
    print("AirNow API key or State Code is missing for site lookup.")
    return(NULL)
  }
  
  tryCatch({
    # --- MODIFIED: Added suppressMessages() to hide the "Retrieving data..." log message ---
    state_shape <- suppressMessages(tigris::states(cb = TRUE) %>% filter(STUSPS == state_code))
    
    bbox <- sf::st_bbox(state_shape)
    bbox_string <- paste(bbox['xmin'], bbox['ymin'], bbox['xmax'], bbox['ymax'], sep = ",")
    
    params_to_check <- c("OZONE", "PM2.5")
    
    current_utc_time <- with_tz(Sys.time(), "UTC")
    start_hour_str <- format(current_utc_time - hours(1), "%Y-%m-%dT%H")
    end_hour_str <- format(current_utc_time, "%Y-%m-%dT%H")
    
    all_sites_df <- purrr::map_dfr(params_to_check, function(param) {
      api_url <- paste0(
        "https://www.airnowapi.org/aq/data/?",
        "startDate=", start_hour_str, "&endDate=", end_hour_str,
        "&parameters=", param, "&BBOX=", bbox_string,
        "&dataType=A&format=application/json&verbose=0&API_KEY=", api_key
      )
      
      tryCatch({
        response <- httr::GET(api_url)
        if (httr::status_code(response) == 200) {
          suppressWarnings({ data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")) })
          return(data)
        }
        return(NULL)
      }, error = function(e) { return(NULL) })
    })
    
    if (is.null(all_sites_df) || nrow(all_sites_df) == 0) {
      print("No active monitoring sites were returned for the state's bounding box.")
      return(NULL)
    }
    
    final_df <- all_sites_df %>%
      mutate(
        CategoryName = case_when(
          Category == 1 ~ "Good", Category == 2 ~ "Moderate",
          Category == 3 ~ "Unhealthy for Sensitive Groups", Category == 4 ~ "Unhealthy",
          Category == 5 ~ "Very Unhealthy", Category == 6 ~ "Hazardous",
          TRUE ~ "Unknown"
        ) %>% factor(levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous", "Unknown"))
      ) %>%
      distinct(Latitude, Longitude, Parameter, .keep_all = TRUE)
    
    return(final_df)
    
  }, error = function(e) {
    print("An error occurred while fetching active monitor data.")
    print(e)
    return(NULL)
  })
}

# =============================================================================
# ADD THIS NEW FUNCTION TO SECTION 2
# =============================================================================

# Get ALL current monitors for a state using its bounding box
get_monitors_by_state_bbox <- function(state_code, api_key) {
  if (is.null(api_key) || api_key == "" || is.null(state_code) || state_code == "") {
    print("AirNow API key or State Code is missing for statewide lookup.")
    return(NULL)
  }
  
  tryCatch({
    state_shape <- suppressMessages(tigris::states(cb = TRUE) %>% filter(STUSPS == state_code))
    bbox <- sf::st_bbox(state_shape)
    bbox_string <- paste(bbox['xmin'], bbox['ymin'], bbox['xmax'], bbox['ymax'], sep = ",")
    
    # --- CRITICAL FIX 1: Use "PM25" for the API request ---
    params_to_check <- c("OZONE", "PM25") 
    
    # --- CRITICAL FIX 2: Use a rolling 24-hour window ---
    current_utc_time <- with_tz(Sys.time(), "UTC")
    # Go back a full 24 hours from the current time
    start_utc_time <- current_utc_time - hours(24) 
    
    # Format these start and end times for the API
    start_date_str <- format(start_utc_time, "%Y-%m-%dT%H")
    end_date_str <- format(current_utc_time, "%Y-%m-%dT%H")
    
    all_sites_df <- purrr::map_dfr(params_to_check, function(param) {
      api_url <- paste0(
        "https://www.airnowapi.org/aq/data/?",
        # Use the corrected 24-hour window
        "startDate=", start_date_str, "&endDate=", end_date_str,
        "&parameters=", param, "&BBOX=", bbox_string,
        "&dataType=B&format=application/json&verbose=0&API_KEY=", api_key
      )
      
      tryCatch({
        response <- httr::GET(api_url)
        if (httr::status_code(response) == 200) {
          content_text <- httr::content(response, "text", encoding = "UTF-8")
          if (nchar(content_text) > 2) {
            suppressWarnings({ data <- jsonlite::fromJSON(content_text) })
            return(data)
          }
        }
        return(NULL)
      }, error = function(e) { return(NULL) })
    })
    
    if (is.null(all_sites_df) || nrow(all_sites_df) == 0) {
      print("No active monitoring sites were returned for the state's bounding box.")
      return(NULL)
    }
    
    final_df <- all_sites_df %>%
      # Ensure we only use the most recent report for each monitor
      group_by(Latitude, Longitude, Parameter) %>%
      filter(UTC == max(UTC)) %>%
      ungroup() %>%
      mutate(
        CategoryName = case_when(
          Category == 1 ~ "Good", Category == 2 ~ "Moderate",
          Category == 3 ~ "Unhealthy for Sensitive Groups", Category == 4 ~ "Unhealthy",
          Category == 5 ~ "Very Unhealthy", Category == 6 ~ "Hazardous",
          TRUE ~ "Unknown"
        ) %>% factor(levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous", "Unknown"))
      ) %>%
      # The rest of your code correctly uses "Parameter" which is the name of the
      # column returned by the API, so this part is correct.
      distinct(Latitude, Longitude, Parameter, .keep_all = TRUE)
    
    return(final_df)
    
  }, error = function(e) {
    print("An error occurred while fetching statewide monitor data.")
    print(e)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# 3. ENHANCED DATA FETCHING FUNCTION (FINAL CORRECTED VERSION)
# -----------------------------------------------------------------------------
get_prescribed_burn_forecast <- function(lat, lon, days_since_rain_input, location_text) { 
  
  tryCatch({
    print(paste("Fetching forecast for lat:", lat, "lon:", lon))
    
    # --- Part 1: Get Metadata and the Grid Data URL ---
    location_metadata <- weathR::point_data(lat = lat, lon = lon)
    nws_office_code <- location_metadata$cwa
    grid_data_url <- location_metadata$forecast_grid_data
    local_timezone <- location_metadata$time_zone
    
    # --- Part 2: Fetch the Raw Grid Data ---
    raw_grid_response <- httr::GET(grid_data_url)
    if (status_code(raw_grid_response) != 200) {
      stop("Failed to fetch raw grid data from NWS.")
    }
    raw_grid_data <- fromJSON(content(raw_grid_response, "text", encoding = "UTF-8"))$properties
    
    # --- Part 3: Define a helper to safely extract and process each variable ---
    extract_grid_variable <- function(grid_data, var_name, new_col_name) {
      if (!is.null(grid_data[[var_name]]) && length(grid_data[[var_name]]$values) > 0) {
        values_df <- as_tibble(grid_data[[var_name]]$values)
        df <- values_df %>%
          mutate(
            time_utc = as.POSIXct(sub("/.*", "", validTime), format="%Y-%m-%dT%H:%M:%S+00:00", tz="UTC")
          ) %>%
          rename(!!new_col_name := value) %>%
          select(time_utc, !!new_col_name)
        return(df)
      }
      return(NULL)
    }
    
    # --- Part 4: Extract all desired variables from the raw grid data ---
    df_list <- list(
      temp_c = extract_grid_variable(raw_grid_data, "temperature", "temp_c"),
      humidity = extract_grid_variable(raw_grid_data, "relativeHumidity", "humidity"),
      # --- FIX 1: Rename to reflect correct units (km/h) ---
      wind_speed_kmh = extract_grid_variable(raw_grid_data, "windSpeed", "wind_speed_kmh"),
      wind_dir = extract_grid_variable(raw_grid_data, "windDirection", "wind_dir_deg"),
      wind_gust_kmh = extract_grid_variable(raw_grid_data, "windGust", "wind_gust_kmh"),
      sky_cover = extract_grid_variable(raw_grid_data, "skyCover", "sky_cover"),
      weather = extract_grid_variable(raw_grid_data, "weather", "weather_raw"),
      mixing_height = extract_grid_variable(raw_grid_data, "mixingHeight", "mixing_height_m"),
      transport_wind_kts = extract_grid_variable(raw_grid_data, "transportWindSpeed", "transport_wind_kts"),
      transport_wind_dir = extract_grid_variable(raw_grid_data, "transportWindDirection", "transport_wind_dir"),
      haines_index = extract_grid_variable(raw_grid_data, "hainesIndex", "haines_index")
    ) %>%
      purrr::compact()
    
    # --- Part 5: Combine all dataframes and process them ---
    if (length(df_list) < 2) return(NULL)
    
    master_df <- df_list %>%
      purrr::reduce(full_join, by = "time_utc") %>%
      arrange(time_utc) %>%
      zoo::na.locf(na.rm = FALSE)
    
    degrees_to_cardinal <- function(deg) {
      if(is.na(deg)) return(NA_character_)
      dirs <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")
      return(dirs[round(deg / 22.5) + 1])
    }
    
    # Find this block within the get_prescribed_burn_forecast function
    final_df <- master_df %>%
      mutate(
        temp = round((temp_c * 9/5) + 32, 0),
        wind_speed = round(wind_speed_kmh * 0.621371, 0),
        wind_gust = if ("wind_gust_kmh" %in% names(.)) round(wind_gust_kmh * 0.621371, 0) else NA_real_,
        wind_dir = sapply(wind_dir_deg, degrees_to_cardinal),
        # --- START CHANGE 1: ADD THIS LINE ---
        # Convert transport wind direction from degrees to cardinal letters (N, SW, etc.)
        transport_wind_dir_cardinal = sapply(transport_wind_dir, degrees_to_cardinal),
        # --- END CHANGE 1 ---
        mixing_height_ft = round(mixing_height_m * 3.28084, 0),
        transport_wind_mph = round(transport_wind_kts * 1.15078, 1),
        transport_wind_ms = round(transport_wind_mph * 0.44704, 0),
        weather_code = if ("weather_raw" %in% names(.)) map_chr(weather_raw, ~paste(.x$weather, collapse = " ") %||% NA_character_) else NA_character_,
        local_time = with_tz(time_utc, local_timezone),
        hour_of_day = hour(local_time),
        ventilation_index = mixing_height_ft * transport_wind_mph,
        kbdi_trend = calculate_kbdi_trend(temp, humidity),
        ffmc = calculate_ffmc(temp, humidity, wind_speed)
      ) %>%
      rowwise() %>%
      mutate(
        fuel_moisture_vals = list(calculate_fuel_moisture(temp, humidity, days_since_rain_input)),
        dispersion_vals = list(determine_dispersion_category(mixing_height_ft, transport_wind_mph, hour_of_day)),
        assessment_vals = list(assess_burn_window(temp, humidity, wind_speed, mixing_height_ft, ventilation_index))
      ) %>%
      ungroup() %>%
      unnest_wider(fuel_moisture_vals, names_sep = "_") %>%
      unnest_wider(dispersion_vals, names_sep = "_") %>%
      unnest_wider(assessment_vals, names_sep = "_") %>%
      rename(
        fuel_moisture_one_hr = fuel_moisture_vals_one_hr,
        fuel_moisture_ten_hr = fuel_moisture_vals_ten_hr,
        dispersion_category = dispersion_vals_category,
        dispersion_description = dispersion_vals_description,
        dispersion_adjusted_vi = dispersion_vals_adjusted_vi,
        burn_quality = assessment_vals_category,
        burn_score = assessment_vals_score
      ) %>%
      mutate(
        ignition_prob = calculate_ignition_probability(temp, humidity, fuel_moisture_one_hr),
        day_label = format(local_time, "%a %m/%d")
      ) %>%
      # --- START CHANGE 2: MODIFY THE 'select' STATEMENT ---
      # Add the new 'transport_wind_dir_cardinal' column to the final data frame
      select(
        time = local_time, temp, humidity, wind_speed, any_of("wind_gust"), wind_dir,
        any_of("sky_cover"), weather_code, mixing_height_m, mixing_height_ft,
        transport_wind_mph, transport_wind_ms, transport_wind_dir_cardinal, # <-- ADDED THIS
        ventilation_index, any_of("haines_index"), kbdi_trend, ffmc, fuel_moisture_one_hr,
        ignition_prob, dispersion_category, dispersion_description,
        dispersion_adjusted_vi, burn_quality, burn_score, hour_of_day, day_label
      ) %>%
      # --- END CHANGE 2 ---
      filter(!is.na(temp)) %>%
      head(72)
    
    fwf_url <- paste0("https://api.weather.gov/products/types/FWF/locations/", nws_office_code)
    fwf_list <- httr::GET(fwf_url, timeout(5))
    full_product_text <- if (status_code(fwf_list) == 200) {
      product_url <- fromJSON(content(fwf_list, "text", encoding = "UTF-8"))$`@graph`$`@id`[1]
      fwf_product <- httr::GET(product_url, timeout(5))
      fromJSON(content(fwf_product, "text", encoding = "UTF-8"))$productText
    } else { NA }
    
    discussion <- if (!is.na(full_product_text)) {
      extracted <- str_extract(full_product_text, "\\.DISCUSSION...[\\s\\S]*?(?=\\n\\n[A-Z]{2}Z[0-9]{3}|\\n\\n\\.|&&|\\$\\$)")
      if(!is.na(extracted)) trimws(extracted) else "Official discussion not found."
    } else { "Fire weather discussion product not available." }
    
    zone_forecast <- if (!is.na(full_product_text)) {
      get_fire_weather_zone_forecast_by_city(full_product_text, location_text)
    } else { "Zone forecast product not available." }
    
    narrative_forecast <- tibble(
      name = "Forecast Summary",
      detailedForecast = "The detailed hourly forecast table provides the most accurate, time-synced weather data. Please refer to it for specific trends in temperature, humidity, and wind."
    )
    
    return(list(
      forecast = final_df, 
      nws_office = nws_office_code, 
      narrative = narrative_forecast,
      fire_discussion = discussion,
      zone_forecast = zone_forecast
    ))
    
  }, error = function(e) {
    print("An error occurred in get_prescribed_burn_forecast. Detailed error below:")
    print(e)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# 4. ENHANCED SHINY UI
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Prescribed Burn Weather Dashboard v2.0", titleWidth = 400),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Detailed Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Forecast Discussion", tabName = "forecastdiscussion", icon = icon("map-marked-alt")),
      menuItem("Burn Windows", tabName = "windows", icon = icon("clock")),
      menuItem("Air Quality", tabName = "airquality", icon = icon("wind")),
      menuItem("HMS Fire & Smoke", tabName = "hms", icon = icon("fire")),
      menuItem("Drought Information", tabName = "drought", icon = icon("tint-slash")),
      menuItem("Safety Checklist", tabName = "safety", icon = icon("check-square")),
      menuItem("Resources", tabName = "resources", icon = icon("book"))
    ),
    
    hr(),
    
    h4("Location Settings", style = "padding-left: 10px;"),
    div(style = "padding: 10px;",
        textInput("location_input", "Enter Location:", value = "Jackson, MS"),
        actionButton("get_weather_btn", "Get Forecast", class = "btn-primary", width = "100%")
    ),
    
    hr(),
    
    h4("Developer Information", style = "padding-left: 10px;"),
    div(style = "padding: 10px; font-size: 0.9em;",
        p(strong("Developer:"), " Rodney Cuevas"),
        p("For questions, comments, or bug reports, please email:"),
        p(tags$a(href = "mailto:RCuevas@mdeq.ms.gov", "RCuevas@mdeq.ms.gov"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .burn-excellent { background-color: #28a745; color: white; }
        .burn-good { background-color: #5cb85c; color: white; }
        .burn-fair { background-color: #ffc107; color: black; }
        .burn-marginal { background-color: #fd7e14; color: white; }
        .burn-poor { background-color: #dc3545; color: white; }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                # NEW COLLAPSIBLE BOX FOR PRESCRIPTION PARAMETERS
                box(
                  title = "Burn Prescription Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE, # Makes the box hideable
                  fluidRow(
                    column(width = 4,
                           sliderInput("rh_slider", "Relative Humidity (%)", 
                                       min = 0, max = 100, value = c(30, 55))
                    ),
                    column(width = 4,
                           sliderInput("wind_slider", "Wind Speed (mph)", 
                                       min = 0, max = 30, value = c(4, 15))
                    ),
                    column(width = 4,
                           sliderInput("temp_slider", "Temperature (°F)", 
                                       min = 0, max = 110, value = c(40, 80)))
                  ),
                  fluidRow(
                    column(width = 6,
                           sliderInput("vi_slider", "Min. Ventilation Index", 
                                       min = 0, max = 100000, value = 20000, step = 5000)
                    ),
                    column(width = 6,
                           numericInput("days_since_rain", "Days Since Last Significant Rain:", 
                                        value = 3, min = 0, max = 90, step = 1)
                    )
                  )
                )
              ),
              fluidRow(
                uiOutput("nws_office_ui")
              ),
              fluidRow(
                box(
                  title = "Current Conditions Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("current_conditions_ui")
                )
              ),
              
              fluidRow(
                box(
                  title = "Smoke Dispersion Forecast",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("smoke_dispersion_ui")
                )
              ),
              
              fluidRow(
                box(
                  title = "Fire Weather Indices",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  uiOutput("fire_indices_ui")
                ),
                box(
                  title = "Next 24-Hour Burn Window Quality",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  withSpinner(plotlyOutput("burn_quality_plot", height = "300px"), type = 6, color = "#007bff")
                )
              ),
              
              fluidRow(
                box(
                  title = "48-Hour Weather Trend",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(plotlyOutput("weather_trend_plot", height = "400px"), type = 6, color = "#007bff")
                ) 
              ) 
      ), 
      
      # Detailed Forecast Tab
      tabItem(tabName = "forecast",
              fluidRow(
                # --- NEW: Box for narrative discussion ---
                box(
                  title = "Forecast Discussion",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("narrative_forecast_ui") # This will display the text
                )
              ),
              fluidRow(
                box(
                  title = "Detailed Hourly Forecast",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::DTOutput("forecast_table"), type = 6, color = "#007bff")
                )
              ),
              fluidRow(
                box(
                  title = "Download Forecast Data",
                  status = "info",
                  width = 12,
                  downloadButton("download_csv", "Download as CSV", class = "btn-success"),
                  br(), br(),
                  p("Export the forecast data for record-keeping or further analysis.")
                )
              )
      ),
      
      # Burn Windows Tab
      tabItem(tabName = "windows",
              fluidRow(
                box(
                  title = "Optimal Burn Windows (Next 72 Hours)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("burn_windows_ui")
                )
              ),
              
              fluidRow(
                box(
                  title = "Burn Window Heatmap",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(plotlyOutput("burn_heatmap", height = "500px"), type = 6, color = "#007bff")
                )
              )
      ),
      
      tabItem(tabName = "airquality",
              fluidRow(
                box(
                  title = "Current Air Quality Conditions",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("current_aqi_ui")
                )
              ),
              fluidRow(
                box(
                  title = "Air Quality Forecast",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  # We will use columns to show today and tomorrow side-by-side
                  fluidRow(
                    column(width = 6, uiOutput("today_aqi_ui")),
                    column(width = 6, uiOutput("tomorrow_aqi_ui"))
                  )
                )
              ),
              
              # --- START: ADD THIS NEW fluidRow AND box ---
              fluidRow(
                box(
                  title = "State Air Quality Monitor Map",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # UI for the pollutant selector dropdown
                  uiOutput("pollutant_selector_ui"),
                  hr(),
                  # The map itself
                  withSpinner(plotOutput("monitor_map_plot", height = "600px"), type = 6, color = "#007bff")
                )
              ),
              # --- END: ADD THIS NEW fluidRow AND box ---
              
              fluidRow(
                box(
                  title = "About Air Quality Data",
                  status = "success",
                  width = 12,
                  p("Air quality data is provided by the U.S. EPA AirNow program. The forecast shows the expected dominant pollutant and and overall Air Quality Index (AQI) for the day."),
                  p(strong("A higher AQI indicates worse air pollution and greater health concern.")),
                  p("This information should be used to assess background air quality levels before a prescribed burn to minimize cumulative impacts on the public.")
                )
              )
      ),
      
      # --- START: ADD THIS ENTIRE NEW tabItem ---
      tabItem(tabName = "hms",
              fluidRow(
                box(
                  title = "NOAA HMS Fire and Smoke Analysis",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  
                  # Date selector
                  dateInput("hms_date_selector", 
                            "Select Date:", 
                            value = Sys.Date(), # Defaults to the current day
                            max = Sys.Date()    # Users can't select a future date
                  ),
                  hr(),
                  
                  # The leaflet map output
                  withSpinner(leafletOutput("hms_map", height = "700px"), type = 6, color = "#dc3545"),
                  
                  hr(),
                  p("This map displays satellite-detected fire hot spots and smoke plumes from the NOAA Hazard Mapping System (HMS). Data is updated daily. This information provides situational awareness but should be field-verified. Polygons represent the approximate area covered by smoke, categorized by density.")
                )
              )
      ),
      # --- END: NEW tabItem ---
      
      tabItem(tabName = "drought",
              fluidRow(
                box(
                  title = "U.S. Drought Monitor Map",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  # Use plotOutput for ggplot
                  withSpinner(plotOutput("drought_map_plot", height = "650px"), type = 6, color = "#007bff"), 
                  hr(),
                  p("This map displays the latest drought conditions for the contiguous United States from the ",
                    tags$a(href="https://droughtmonitor.unl.edu/", "U.S. Drought Monitor", target="_blank"),
                    ". The map is updated weekly."
                  )
                )
              )
      ),
      
      # Safety Checklist Tab
      tabItem(tabName = "safety",
              fluidRow(
                box(
                  title = "Burn Restrictions & Advisories",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  uiOutput("burn_restrictions_ui")
                ),
                box(
                  title = "Pre-Burn Go/No-Go Checklist", # More professional title
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  
                  # Grouped Checkbox List
                  h4("Phase 1: Planning & Preparation"),
                  checkboxGroupInput("safety_checks_planning", NULL,
                                     choices = list(
                                       "Prescribed Burn Plan Written & Approved" = 1,
                                       "Valid Burn Permit Obtained" = 2,
                                       "Risk Assessment & Go/No-Go Parameters Defined" = 3,
                                       "Contingency Plan Developed (Escape Routes, Suppression Resources)" = 4,
                                       "Smoke Management Plan & Sensitive Receptors Identified" = 5
                                     )),
                  
                  hr(),
                  h4("Phase 2: On-Site Actions (Day of Burn)"),
                  checkboxGroupInput("safety_checks_onsite", NULL,
                                     choices = list(
                                       "On-Site Weather Verified (within prescription)" = 6,
                                       "Equipment & Personnel Inspected and Ready" = 7,
                                       "Firebreaks Confirmed Secure" = 8,
                                       "Crew Briefing Conducted (Objectives, Roles, Hazards)" = 9,
                                       "Test Fire Conducted & Evaluated" = 10
                                     )),
                  
                  hr(),
                  h4("Phase 3: Communication"),
                  checkboxGroupInput("safety_checks_comms", NULL,
                                     choices = list(
                                       "Required Notifications Made (Fire Dept, Forestry, etc.)" = 11,
                                       "Neighbors/Stakeholders Notified" = 12
                                     )),
                  
                  br(),
                  uiOutput("safety_status")
                ),
                
                box(
                  title = "Important Reminders",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  h4("Red Flag Warnings"),
                  p("Always check for Red Flag Warnings before burning. Do not burn during Red Flag conditions."),
                  hr(),
                  h4("Smoke Management"),
                  p("Consider smoke-sensitive areas: hospitals, schools, highways, airports."),
                  hr(),
                  h4("Escape Routes"),
                  p("Always maintain two escape routes and ensure all personnel know the evacuation plan."),
                  hr(),
                  h4("Weather Monitoring"),
                  p("Continuously monitor on-site weather conditions. Forecasts are predictions, not guarantees.")
                )
              )
      ),
      
      # Resources Tab
      tabItem(tabName = "resources",
              fluidRow(
                box(
                  title = "Prescribed Burning Resources",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("Understanding the Fire Weather Indices"),
                  tags$ul(
                    tags$li(strong("Ventilation Index (VI):"), " Product of mixing height and transport wind. Higher values mean better smoke dispersal. (Excellent: > 60,000, Good: 40,000-60,000, Fair: 20,000-40,000, Poor: < 20,000)."),
                    tags$li(strong("Keetch-Byram Drought Index (KBDI):"), " A measure of long-term drought on a scale of 0-800. (0-200: Low fire potential, 200-400: Moderate, 400-600: High, 600-800: Extreme). This app shows a simplified trend, not the absolute value."),
                    tags$li(strong("Fine Fuel Moisture Code (FFMC):"), " Estimates moisture in fine fuels on a scale of 0-100. (85-88: High ignition potential, 89-91: Very High, >92: Extreme)."),
                    tags$li(strong("Haines Index:"), " Atmospheric stability and dryness. (2-3: Very Low Potential, 4: Low, 5: Moderate, 6: High potential for erratic or large fire growth)."),
                    tags$li(strong("Mixing Height:"), " The height to which the lower atmosphere will be well-mixed. Higher values (>2000 ft) are better for smoke lift."),
                    tags$li(strong("Transport Wind:"), " Average wind speed within the mixing layer. This wind governs the direction smoke will travel.")
                  ),
                  hr(),
                  
                  h4("Common Prescribed Fire Terminology"),
                  tags$ul(
                    tags$li(strong("Ignition Pattern:"), " The method used to light the fire (e.g., backing fire, strip-heading fire) to control intensity."),
                    tags$li(strong("Mop-up:"), " The process of extinguishing or removing burning material near control lines after the fire has passed to prevent escapes."),
                    tags$li(strong("Patrol:"), " The period of monitoring the burn unit after mop-up is complete to ensure the fire is completely out."),
                    tags$li(strong("Spotting:"), " When burning embers (firebrands) are carried by the wind and start new fires beyond the main control lines."),
                    tags$li(strong("Firebreak:"), " A natural or man-made barrier used to stop or check fires, or to provide a control line from which to work.")
                  ),
                  hr(),
                  
                  h4("Post-Burn Responsibilities"),
                  tags$p("A prescribed burn is not complete until it is declared 'out'. This involves:"),
                  tags$ul(
                    tags$li(strong("1. Mop-up:"), "Securing the perimeter of the burn unit by extinguishing all smoldering fuels within a specified distance from the control line (e.g., 50-100 feet)."),
                    tags$li(strong("2. Patrol & Monitor:"), "Periodically checking the burn area for several days to ensure no smoke or heat remains and that the fire cannot reignite or escape.")
                  ),
                  hr(),
                  
                  h4("Useful Links"),
                  tags$ul(
                    tags$li(a("National Weather Service Fire Weather", href = "https://www.weather.gov/fire", target = "_blank")),
                    tags$li(a("NWCG Prescribed Fire Resources", href = "https://www.nwcg.gov/publications/pms484", target = "_blank")),
                    tags$li(a("Request a Spot Weather Forecast", href = "https://spot.weather.gov/new-request", target = "_blank"))
                  ),
                  hr(),
                  
                  p(strong("Disclaimer:"), "This tool provides weather information for planning purposes only. Always verify conditions on-site, obtain proper permits, and follow local regulations. The user assumes all responsibility for burn operations.")
                )
              )
      ),
      
      tabItem(tabName = "forecastdiscussion",
              fluidRow(
                box(
                  title = "Official NWS Fire Weather Discussion",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  uiOutput("zone_forecast_header"),
                  hr(),
                  uiOutput("zone_forecast_discussion"),
                  hr(),
                  uiOutput("zone_forecast_ui")
                )
              )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 5. ENHANCED SHINY SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to store forecast data
  rv_forecast_data <- reactiveVal(NULL)
  rv_nws_office <- reactiveVal(NULL)
  rv_state_code <- reactiveVal(NULL)
  
  # --- ADD THESE NEW REACTIVEVALS FOR REAL-TIME DATA ---
  rv_realtime_conditions <- reactiveVal(NULL)
  rv_realtime_station <- reactiveVal(NULL)
  
  # --- ADD THIS NEW REACTIVEVAL ---
  rv_narrative_forecast <- reactiveVal(NULL)
  rv_fire_discussion <- reactiveVal(NULL)
  rv_zone_forecast <- reactiveVal(NULL)
  
  rv_current_aq_data <- reactiveVal(NULL)
  rv_air_quality_data <- reactiveVal(NULL)
  rv_burn_restrictions <- reactiveVal(NULL)
  
  rv_statewide_monitors <- reactiveVal(NULL)
  rv_hms_data <- reactiveVal(NULL)
  
  # Fetch weather data when button clicked
  observeEvent(input$get_weather_btn, {
    
    location_text <- trimws(input$location_input)
    
    if (is.null(location_text) || location_text == "") {
      showNotification("Please enter a location.", type = "warning")
      return()
    }
    
    showNotification("Fetching weather data...", type = "message", id = "loading")
    
    # Geocode location
    geo_location <- tryCatch({
      tidygeocoder::geo(address = location_text, method = 'osm')
    }, error = function(e) {
      showNotification("Geocoding error. Please try a different location.", type = "error")
      return(NULL)
    })
    
    if (is.null(geo_location) || is.na(geo_location$lat)) {
      showNotification("Could not find location.", type = "error")
      removeNotification("loading")
      return()
    }
    
    # --- START: NEW REAL-TIME DATA FETCH ---
    # Fetch real-time conditions from the nearest airport station
    realtime_results <- get_realtime_conditions(geo_location$lat, geo_location$long)
    if (!is.null(realtime_results)) {
      rv_realtime_conditions(realtime_results$data)
      rv_realtime_station(realtime_results$station)
      showNotification("Real-time observation loaded.", type = "message", duration = 3)
    } else {
      showNotification("Could not load real-time observation.", type = "warning")
      rv_realtime_conditions(NULL) # Clear old data
      rv_realtime_station(NULL)
    }
    # --- END: NEW REAL-TIME DATA FETCH ---
    
    # Fetch forecast
    forecast_results <- get_prescribed_burn_forecast(
      geo_location$lat, 
      geo_location$long,
      days_since_rain_input = input$days_since_rain,
      location_text = location_text
    )
    
    # Fetch burn restrictions info
    restrictions_info <- check_burn_restrictions(geo_location$lat, geo_location$long)
    rv_burn_restrictions(restrictions_info) 
    
    # Check if the forecast part of the results is valid AND contains data
    if (is.data.frame(forecast_results$forecast) && nrow(forecast_results$forecast) > 0) {
      
      # This new line filters the forecast to only include hours from the current time forward.
      forecast_results$forecast <- forecast_results$forecast %>% filter(time >= Sys.time())
      
      # SUCCESS: Store all the new data
      rv_forecast_data(forecast_results$forecast)
      rv_nws_office(forecast_results$nws_office)
      rv_narrative_forecast(forecast_results$narrative)
      rv_fire_discussion(forecast_results$fire_discussion)
      rv_zone_forecast(forecast_results$zone_forecast)
      
      removeNotification("loading")
      showNotification("Data loaded. Generating plots and tables...", type = "message", duration = 4)
      
    } else {
      # FAILURE: Show a message AND clear all stale data
      showNotification("Could not fetch weather data for this specific location. It may be too rural or lack detailed grid data.", type = "error", duration = 10)
      removeNotification("loading")
      
      rv_forecast_data(NULL)
      rv_nws_office(NULL)
      rv_narrative_forecast(NULL)
      rv_fire_discussion(NULL)
      rv_zone_forecast(NULL)
    }
    
    # Retrieve the API key from the server environment
    airnow_key <- Sys.getenv("AIRNOW_API_KEY")
    
    if (airnow_key != "") {
      # 1. Get data for the SPECIFIC location (for AQI boxes and state code)
      current_aq_results <- get_current_air_quality(geo_location$lat, geo_location$long, airnow_key)
      rv_current_aq_data(current_aq_results)
      
      # 2. Get forecast data
      forecast_aq_results <- get_air_quality_data(geo_location$lat, geo_location$long, airnow_key)
      rv_air_quality_data(forecast_aq_results)
      
      # 3. Get statewide monitors. This is now de-coupled from the point-specific result.
      state_code <- NULL
      # First, try to get state code from the specific AQI result (most reliable).
      if (is.data.frame(current_aq_results) && nrow(current_aq_results) > 0) {
        state_code <- current_aq_results$StateCode[1]
      } else {
        # Fallback: If the point-specific call failed, parse the state from the user's input.
        location_parts <- strsplit(location_text, ",")[[1]]
        if (length(location_parts) == 2) {
          parsed_state <- trimws(location_parts[2])
          # Simple validation: check if it's a 2-letter abbreviation.
          if (nchar(parsed_state) == 2 && toupper(parsed_state) == parsed_state) {
            state_code <- parsed_state
          }
        }
      }
      
      # Now, if we successfully found a state code from ANY source, fetch the map data.
      if (!is.null(state_code)) {
        rv_state_code(state_code) # <-- ADD THIS LINE TO SAVE THE STATE CODE
        statewide_monitors <- get_monitors_by_state_bbox(state_code, airnow_key)
        rv_statewide_monitors(statewide_monitors)
        
        print("--- Statewide Monitor Data (from fallback if needed) ---")
        print(head(statewide_monitors))
        
      } else {
        # If we couldn't figure out the state, clear the monitor data and state code.
        rv_statewide_monitors(NULL)
        rv_state_code(NULL) # <-- ADD THIS LINE TO CLEAR THE STATE CODE
      }
      
    } else {
      # If no key, set all to NULL
      rv_current_aq_data(NULL)
      rv_air_quality_data(NULL)
      rv_statewide_monitors(NULL)
      print("NOTICE: AIRNOW_API_KEY is not set. Air Quality feature will be disabled.")
    }
    
  }, ignoreInit = TRUE)
  
  # Current Conditions UI
  output$current_conditions_ui <- renderUI({
    # --- MODIFIED: Use both real-time and forecast data ---
    realtime_df <- rv_realtime_conditions()
    forecast_df <- rv_forecast_data()
    station_info <- rv_realtime_station()
    
    # Require both data sources to be available before rendering
    req(realtime_df, forecast_df, station_info)
    
    if (nrow(realtime_df) == 0 || nrow(forecast_df) == 0) {
      return(p("Enter a location and click 'Get Forecast' to begin."))
    }
    
    # Get the single row of real-time data
    current_obs <- realtime_df[1, ]
    # Get the first hour of forecast data (for VI)
    current_fcst <- forecast_df[1, ]
    
    # Create status boxes
    create_info_box <- function(title, value, unit, min_val = NULL, max_val = NULL) {
      if(is.na(value)) {
        return(infoBox(title, "N/A", icon = icon("question-circle"), color = "blue"))
      }
      in_range <- if (!is.null(min_val) && !is.null(max_val)) { value >= min_val && value <= max_val } else { TRUE }
      color <- if (in_range) "green" else "red"
      icon_name <- if (in_range) "check" else "times"
      display_value <- if (value > 1000) format(value, big.mark = ",") else value
      infoBox(title = title, value = paste(display_value, unit), icon = icon(icon_name), color = color, width = 3)
    }
    
    # --- UI OUTPUT ---
    tagList(
      # New row to display which station is providing the real-time data
      fluidRow(
        column(12,
               div(style = "text-align: right; font-size: 0.9em; color: #6c757d; padding-right: 15px; margin-bottom: 10px;",
                   paste0("Real-time surface conditions from station: ", 
                          station_info$name, " (", station_info$id, ")")
               )
        )
      ),
      # Existing row for the info boxes
      fluidRow(
        # These three pull from the REAL-TIME observation
        create_info_box("Temperature", current_obs$temp, "°F", input$temp_slider[1], input$temp_slider[2]),
        create_info_box("Humidity", current_obs$humidity, "%", input$rh_slider[1], input$rh_slider[2]),
        create_info_box("Wind Speed", current_obs$wind_speed, "mph", input$wind_slider[1], input$wind_slider[2]),
        
        # This one still pulls from the NWS FORECAST, as it's not an observed parameter
        create_info_box("Ventilation Index", current_fcst$ventilation_index, "", input$vi_slider, Inf)
      )
    )
  })
  
  # UI to display the NWS office
  output$nws_office_ui <- renderUI({
    office_code <- rv_nws_office()
    if (is.null(office_code)) return(NULL)
    
    # You can search for NWS office codes to get full names, but for now
    # just showing the code is great information.
    # Example: "JAN" is Jackson, MS.
    div(
      style = "text-align: right; font-size: 0.9em; color: #6c757d; padding-right: 15px;",
      paste("Forecast data provided by NWS Office:", toupper(office_code))
    )
  })
  
  # Fire Indices UI
  output$fire_indices_ui <- renderUI({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    current <- df[1, ]
    
    tags$div(
      # Top section for primary atmospheric conditions
      tags$p(strong("Surface Wind (20ft):"), sprintf(" %s mph from the %s", current$wind_speed, current$wind_dir)),
      tags$p(strong("Transport Wind:"), sprintf(" %.1f mph from the %s", current$transport_wind_mph, current$transport_wind_dir_cardinal)),
      tags$p(strong("Mixing Height:"), sprintf(" %s ft", format(current$mixing_height_ft, big.mark = ","))),
      
      if ("haines_index" %in% names(current) && !is.na(current$haines_index)) {
        tags$p(strong("Haines Index:"), sprintf(" %d", current$haines_index),
               if (current$haines_index >= 5) tags$span(" (Unstable)", style = "color: orange;"))
      },
      
      # Separator
      hr(), 
      
      # Bottom section for calculated fire behavior indices
      tags$p(strong("KBDI Trend:"), sprintf(" %d", current$kbdi_trend),
             if (current$kbdi_trend > 150) tags$span(" (Very Dry)", style = "color: red;")),
      tags$p(strong("Fine Fuel Moisture:"), sprintf(" %.1f", current$ffmc),
             if (current$ffmc > 85) tags$span(" (High Fire Risk)", style = "color: red;")),
      tags$p(strong("1-Hr Fuel Moisture:"), sprintf(" %.1f%%", current$fuel_moisture_one_hr),
             if (current$fuel_moisture_one_hr < 6) tags$span(" (Critical)", style = "color: red;")),
      tags$p(strong("Ignition Probability:"), sprintf(" %d%%", current$ignition_prob),
             if (current$ignition_prob > 70) tags$span(" (High)", style = "color: orange;")),
      tags$p(strong("Smoke Dispersion:"), sprintf(" %s", current$dispersion_category),
             tags$span(sprintf(" (%s)", current$dispersion_description), style = "font-size: 0.9em; color: gray;"))
    )
  })
  
  
  # Burn Restrictions UI
  output$burn_restrictions_ui <- renderUI({
    restrictions <- rv_burn_restrictions()
    if (is.null(restrictions)) return(p("Get a forecast to check local advisories."))
    
    # Create a list of links
    link_items <- lapply(names(restrictions$links), function(name) {
      tags$li(a(name, href = restrictions$links[[name]], target = "_blank"))
    })
    
    tagList(
      h4(restrictions$status, style = "color: #007bff;"),
      p(restrictions$message),
      if (restrictions$permit_required) {
        p(strong("A burn permit is likely required."), style = "color: #dc3545;")
      },
      hr(),
      h5("Important Resources:"),
      tags$ul(link_items)
    )
  })
  
  # Smoke Dispersion UI
  output$smoke_dispersion_ui <- renderUI({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    current <- df[1, ]
    
    # Determine the color for the status box based on the category
    status_color_class <- case_when(
      current$dispersion_category == "Excellent" ~ "burn-excellent",
      current$dispersion_category == "Good" ~ "burn-good",
      current$dispersion_category == "Fair" ~ "burn-fair",
      current$dispersion_category == "Poor" ~ "burn-marginal",
      TRUE ~ "burn-poor"
    )
    
    div(
      class = paste("alert", status_color_class),
      style = "padding: 15px; text-align: center;",
      h4(style = "margin-top: 0;", "Current Dispersion Category"),
      h2(style = "margin-bottom: 5px; font-weight: bold;", current$dispersion_category),
      p(strong(current$dispersion_description)),
      p(paste("Adjusted Ventilation Index:", format(current$dispersion_adjusted_vi, big.mark = ",")))
    )
  })
  
  # Burn Quality Plot
  output$burn_quality_plot <- renderPlotly({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df_24hr <- df %>% 
      head(24) %>%
      mutate(color = case_when(
        burn_quality == "Excellent" ~ "#28a745",
        burn_quality == "Good" ~ "#5cb85c",
        burn_quality == "Fair" ~ "#ffc107",
        burn_quality == "Marginal" ~ "#fd7e14",
        TRUE ~ "#dc3545"
      ))
    
    p <- plot_ly(df_24hr, x = ~time, y = ~burn_score, type = 'bar',
                 marker = list(color = ~color),
                 text = ~paste("Time:", format(time, "%a %I:%M %p"),
                               "<br>Quality:", burn_quality,
                               "<br>Score:", burn_score),
                 hoverinfo = "text") %>%
      layout(title = "Burn Window Quality Score",
             xaxis = list(title = ""),
             yaxis = list(title = "Quality Score", range = c(0, 100)),
             showlegend = FALSE)
    
    p
  })
  
  # Weather Trend Plot (ENHANCED VERSION)
  output$weather_trend_plot <- renderPlotly({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df_48hr <- df %>% head(48)
    
    # NEW: Get the local timezone abbreviation (e.g., "CDT") from the time column
    local_tz_abbr <- format(df_48hr$time[1], "%Z")
    
    p <- plot_ly(df_48hr, x = ~time) %>%
      add_trace(
        y = ~temp, 
        name = "Temperature", 
        type = 'scatter', 
        mode = 'lines+markers',
        line = list(color = '#FF6B6B', shape = "spline", width = 3), # NEW: Smoother line, thicker
        marker = list(size = 5, symbol = 'circle-open'),
        yaxis = "y",
        hoverinfo = "text", # NEW: Custom hover text
        text = ~paste(format(time, "%a %I:%M %p"), "<br>Temp:", temp, "°F")
      ) %>%
      add_trace(
        y = ~humidity, 
        name = "Humidity", 
        type = 'scatter', 
        mode = 'lines+markers',
        line = list(color = '#4ECDC4', shape = "spline", width = 3), # NEW: Smoother line, thicker
        marker = list(size = 5, symbol = 'circle-open'),
        yaxis = "y2",
        hoverinfo = "text", # NEW: Custom hover text
        text = ~paste("RH:", humidity, "%")
      ) %>%
      add_trace(
        y = ~wind_speed, 
        name = "Wind Speed", 
        type = 'scatter', 
        mode = 'lines+markers',
        line = list(color = '#45B7D1', shape = "spline", width = 2, dash = 'dot'), # NEW: Smoother line, different style
        marker = list(size = 5, symbol = 'cross'),
        yaxis = "y3",
        hoverinfo = "text", # NEW: Custom hover text
        text = ~paste("Wind:", wind_speed, "mph")
      ) %>%
      layout(
        title = "48-Hour Weather Forecast",
        # NEW: Dynamic timezone label on the x-axis
        xaxis = list(title = paste("Time (", local_tz_abbr, ")")), 
        yaxis = list(
          title = "Temperature (°F)", 
          side = "left", 
          titlefont = list(color = '#FF6B6B'),
          tickfont = list(color = '#FF6B6B')
        ),
        yaxis2 = list(
          title = "Humidity (%)", 
          overlaying = "y", 
          side = "right",
          range = c(min(df_48hr$humidity)-5, 100), # Give humidity some space
          titlefont = list(color = '#4ECDC4'),
          tickfont = list(color = '#4ECDC4')
        ),
        yaxis3 = list(
          title = "Wind (mph)", 
          overlaying = "y", 
          side = "right",
          position = 0.92, # Adjust position
          range = c(0, max(df_48hr$wind_speed) + 5), # Give wind some space
          titlefont = list(color = '#45B7D1'),
          tickfont = list(color = '#45B7D1')
        ),
        hovermode = 'x unified', # This is great, keep it
        # NEW: Move legend to the top-center
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.1) 
      )
    
    p
  })
  
  # Narrative Forecast UI
  output$narrative_forecast_ui <- renderUI({
    narrative_df <- rv_narrative_forecast()
    if (is.null(narrative_df) || nrow(narrative_df) == 0) {
      return(p("Forecast discussion will appear here after fetching data."))
    }
    
    # Create a UI element for each forecast period
    lapply(1:nrow(narrative_df), function(i) {
      tagList(
        h4(strong(narrative_df$name[i])),
        p(narrative_df$detailedForecast[i]),
        if(i < nrow(narrative_df)) hr() else NULL
      )
    })
  })
  
  # Forecast Table
  # --- CORRECTED to use meters directly ---
  output$forecast_table <- DT::renderDT({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df %>%
      # --- START: MODIFIED 'select' STATEMENT ---
      select(
        Time = time, `Temp (°F)` = temp, `Humidity (%)` = humidity, `Wind (mph)` = wind_speed,
        `Gust (mph)` = any_of("wind_gust"), `Wind Dir` = wind_dir, `Sky Cover (%)` = any_of("sky_cover"),
        `Weather` = weather_code, `Mix Hgt (m)` = mixing_height_m, 
        `Trans Wind (mph)` = transport_wind_mph, 
        `Trans Wind Dir` = transport_wind_dir_cardinal, # <-- ADDED THIS COLUMN
        `Burn Quality` = burn_quality
      ) %>%
      # --- END: MODIFIED 'select' STATEMENT ---
      DT::datatable(options = list(
        pageLength = 12, scrollX = TRUE,
        rowCallback = JS(
          "function(row, data) {",
          # This line below needs to be updated to account for the new column
          "  var quality = data[data.length - 1];", # JavaScript index is 0-based
          "  if(quality == 'Excellent') $(row).css('background-color', '#d4edda');",
          "  else if(quality == 'Good') $(row).css('background-color', '#e7f5e7');",
          "  else if(quality == 'Fair') $(row).css('background-color', '#fff3cd');",
          "  else if(quality == 'Marginal') $(row).css('background-color', '#ffeaa7');",
          "  else if(quality == 'Poor') $(row).css('background-color', '#f8d7da');",
          "}"
        )
      ))
  })
  
  # Burn Windows UI
  output$burn_windows_ui <- renderUI({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df_filtered <- df %>%
      mutate(
        temp_ok = temp >= input$temp_slider[1] & temp <= input$temp_slider[2],
        rh_ok = humidity >= input$rh_slider[1] & humidity <= input$rh_slider[2],
        wind_ok = wind_speed >= input$wind_slider[1] & wind_speed <= input$wind_slider[2],
        vi_ok = ventilation_index >= input$vi_slider,
        all_params_ok = temp_ok & rh_ok & wind_ok & vi_ok
      ) %>%
      filter(all_params_ok,
             hour_of_day >= 10 & hour_of_day <= 16)
    
    if (nrow(df_filtered) > 0) {
      df_optimal <- df_filtered %>%
        group_by(day_label) %>%
        # --- START: MODIFIED 'summarise' BLOCK ---
        summarise(
          start_time = min(time),
          end_time = max(time),
          avg_temp = round(mean(temp), 0),
          avg_humidity = round(mean(humidity), 0),
          avg_wind = round(mean(wind_speed), 1),
          # This helper function finds the most common wind direction in the window
          prevailing_wind_dir = names(which.max(table(wind_dir))),
          prevailing_trans_wind_dir = names(which.max(table(transport_wind_dir_cardinal))),
          avg_vi = round(mean(ventilation_index), 0),
          avg_dispersion = first(dispersion_category),
          hours = n(),
          .groups = 'drop'
        ) %>%
        # --- END: MODIFIED 'summarise' BLOCK ---
        filter(hours >= 2) 
    } else {
      df_optimal <- tibble()
    }
    
    if (is.null(df_optimal) || nrow(df_optimal) == 0) {
      return(
        div(
          class = "alert alert-warning",
          h4("No 'Optimal' Burn Windows Found"),
          p("This means no time periods met 100% of the criteria you set in the 'Burn Prescription Parameters' on the Dashboard tab."),
          p("The heatmap below shows the general burn quality, which uses a more flexible scoring system. You may see 'Good' or 'Fair' hours there even if they don't meet your strict prescription."),
          strong("Action:"),
          tags$ul(
            tags$li("Review the heatmap to see which parameters might be slightly out of range during otherwise good periods."),
            tags$li("Consider adjusting the prescription sliders on the Dashboard if your burn plan allows for flexibility.")
          )
        )
      )
    }
    
    # --- START: MODIFIED UI OUTPUT BLOCK ---
    window_boxes <- lapply(1:nrow(df_optimal), function(i) {
      window <- df_optimal[i, ]
      div(
        class = "alert alert-success",
        h4(window$day_label),
        p(strong("Time Window: "), 
          format(window$start_time, "%I:%M %p"), " - ", 
          format(window$end_time, "%I:%M %p"),
          " (", window$hours, " hours)"),
        p(strong("Avg. Conditions: "),
          "Temp: ", window$avg_temp, "°F | ",
          "RH: ", window$avg_humidity, "% | ",
          "VI: ", format(window$avg_vi, big.mark = ",")),
        # This new paragraph displays the prevailing winds
        p(strong("Prevailing Winds: "),
          "Surface: ", window$avg_wind, " mph from ", window$prevailing_wind_dir, " | ",
          "Transport from ", window$prevailing_trans_wind_dir
        ),
        p(strong("Smoke Dispersion: "), window$avg_dispersion)
      )
    })
    # --- END: MODIFIED UI OUTPUT BLOCK ---
    
    do.call(tagList, window_boxes)
  })
  
  # Reusable function to create the AQI display box (ENHANCED with dynamic titles)
  create_aqi_box <- function(data, title_prefix, reporting_area, state_code) {
    if (is.null(data) || nrow(data) == 0) {
      # Use a more informative message when no data is available for a specific day
      return(p(paste(title_prefix, ": Forecast data not available.", sep="")))
    }
    
    # Create the main title for the entire day's forecast section
    full_title <- h4(
      paste(title_prefix, " for ", reporting_area, ", ", state_code, sep=""), 
      style="text-align: center;"
    )
    
    # Use lapply to create a UI element for EACH row (pollutant)
    pollutant_boxes <- lapply(1:nrow(data), function(i) {
      pollutant_data <- data[i, ]
      category_name <- pollutant_data$Category$Name
      aqi_value <- pollutant_data$AQI
      pollutant_name <- pollutant_data$ParameterName
      
      box_color <- get_aqi_color(category_name)
      text_color <- if (category_name == "Moderate") "black" else "white"
      
      if (aqi_value == -1) {
        aqi_display_element <- h2(style="margin:0; font-weight:bold;", category_name)
      } else {
        aqi_display_element <- tagList(
          h2(style="margin:0; font-weight:bold;", aqi_value),
          h4(style="margin:0;", category_name)
        )
      }
      
      column(
        width = 6,
        div(
          style = paste0(
            "background-color:", box_color, "; color:", text_color, ";",
            "padding: 20px; text-align: center; border-radius: 10px; margin-bottom: 10px;"
          ),
          aqi_display_element
        ),
        p(style="text-align: center;",
          strong("Pollutant:"), " ", pollutant_name
        )
      )
    })
    
    tagList(
      full_title, # Use the new dynamic title
      fluidRow(do.call(tagList, pollutant_boxes))
    )
  }
  
  # Corrected UI block for AQI - Shows "Today's Forecast"
  # This block is now DEDICATED to showing CURRENT OBSERVATIONS
  output$current_aqi_ui <- renderUI({
    req(input$get_weather_btn) 
    current_aq_data <- rv_current_aq_data()
    
    # Check if we have valid current observation data
    if (is.null(current_aq_data) || !is.data.frame(current_aq_data) || nrow(current_aq_data) == 0) {
      return(
        div(class = "alert alert-info",
            h4("No Current Air Quality Monitors Found"),
            p("Real-time air quality monitoring data is not available for this specific location.")
        )
      )
    }
    
    # Get metadata for the title from the first row of data
    reporting_area <- current_aq_data$ReportingArea[1]
    state_code <- current_aq_data$StateCode[1]
    hour_observed <- current_aq_data$HourObserved[1]
    
    # Create the title
    full_title <- h4(
      paste("Current Observations for ", reporting_area, ", ", state_code, " (as of ", hour_observed, ":00)", sep=""),
      style="text-align: center;"
    )
    
    # Use lapply to create a display box for each observed pollutant
    pollutant_boxes <- lapply(1:nrow(current_aq_data), function(i) {
      pollutant_data <- current_aq_data[i, ]
      category_name <- pollutant_data$Category$Name
      aqi_value <- pollutant_data$AQI
      pollutant_name <- pollutant_data$ParameterName
      
      box_color <- get_aqi_color(category_name)
      text_color <- if (category_name == "Moderate") "black" else "white"
      
      aqi_display_element <- tagList(
        h2(style="margin:0; font-weight:bold;", aqi_value),
        h4(style="margin:0;", category_name)
      )
      
      column(
        width = 6,
        div(
          style = paste0("background-color:", box_color, "; color:", text_color, "; padding: 20px; text-align: center; border-radius: 10px; margin-bottom: 10px;"),
          aqi_display_element
        ),
        p(style="text-align: center;", strong("Pollutant:"), " ", pollutant_name)
      )
    })
    
    tagList(full_title, fluidRow(do.call(tagList, pollutant_boxes)))
  })
  
  # This block now handles BOTH the forecast display AND the "no forecast available" message
  output$today_aqi_ui <- renderUI({
    aq_data <- rv_air_quality_data()
    
    # Check if forecast data is invalid or empty
    if (is.null(aq_data) || !is.data.frame(aq_data) || nrow(aq_data) == 0) {
      return(
        div(class = "alert alert-warning",
            h4("No Air Quality Forecast Available"),
            p("A multi-day air quality forecast has not been issued for this specific location.")
        )
      )
    }
    
    # If data IS available, proceed to show Tomorrow's forecast
    tomorrow_forecast <- aq_data %>%
      mutate(DateForecast = as.Date(DateForecast)) %>%
      filter(DateForecast == (Sys.Date() + 1))
    
    create_aqi_box(tomorrow_forecast, "Tomorrow's Forecast", aq_data$ReportingArea[1], aq_data$StateCode[1])
  })
  
  # This block now only renders if forecast data is available
  output$tomorrow_aqi_ui <- renderUI({
    aq_data <- rv_air_quality_data()
    # If data is invalid, return NULL so nothing is displayed here (the message is in the block above)
    if (is.null(aq_data) || !is.data.frame(aq_data) || nrow(aq_data) == 0) return(NULL)
    
    # If data IS available, proceed to show the Day After Tomorrow's forecast
    day_after_forecast <- aq_data %>%
      mutate(DateForecast = as.Date(DateForecast)) %>%
      filter(DateForecast == (Sys.Date() + 2))
    
    create_aqi_box(day_after_forecast, "Day After Tomorrow", aq_data$ReportingArea[1], aq_data$StateCode[1])
  })
  
  # Burn Heatmap
  output$burn_heatmap <- renderPlotly({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Create a new data frame for the heatmap
    # Ensure the 'day' column is an ordered factor. Using rev() puts the
    # most recent day at the top of the plot, which is more intuitive.
    df_for_heat <- df %>%
      mutate(
        day = factor(day_label, levels = rev(unique(df$day_label)))
      )
    
    p <- plot_ly(
      data = df_for_heat,
      x = ~hour_of_day,
      y = ~day,
      z = ~burn_score,
      type = "heatmap",
      colorscale = list(
        c(0, "#dc3545"),      # Poor
        c(0.3, "#fd7e14"),    # Marginal
        c(0.5, "#ffc107"),    # Fair
        c(0.7, "#5cb85c"),    # Good
        c(1, "#28a745")       # Excellent
      ),
      hoverinfo = 'text',
      text = ~paste(
        "Day:", day, "<br>",
        "Hour:", hour_of_day, ":00<br>",
        "Score:", round(burn_score, 1)
      )
    ) %>%
      layout(
        title = "Burn Quality by Day and Hour",
        xaxis = list(title = "Hour of Day"),
        yaxis = list(title = "", categoryorder = "array", categoryarray = ~day) # Ensure order
      )
    
    p
  })
  
  # Renders the dropdown UI for selecting a pollutant
  output$pollutant_selector_ui <- renderUI({
    sites <- rv_statewide_monitors()
    req(sites)
    
    # --- CORRECTED: Use the 'Parameter' column for choices ---
    pollutant_choices <- unique(sites$Parameter)
    
    selectInput("pollutant_select", 
                "Select Pollutant to Display:",
                choices = pollutant_choices,
                selected = pollutant_choices[1],
                width = "300px")
  })
  
  # Renders the state map with monitor locations and their AQI data
  output$monitor_map_plot <- renderPlot({
    sites <- rv_statewide_monitors()
    state_code <- rv_state_code() # <-- GET THE SAVED STATE CODE
    
    # Require the statewide data, the state code, and the pollutant selection.
    req(sites, state_code, input$pollutant_select)
    
    # Filter using the 'Parameter' column
    filtered_sites <- sites %>%
      filter(Parameter == input$pollutant_select)
    
    # Fetch state and county boundaries using tigris
    state_shape <- suppressMessages(tigris::states(cb = TRUE) %>%
                                      filter(STUSPS == state_code))
    
    # <-- ADD THIS LINE: Fetch the county boundaries for the selected state
    counties_shape <- suppressMessages(tigris::counties(state = state_code, cb = TRUE))
    
    aqi_colors <- c( "Good" = "#00e400", "Moderate" = "#ffff00", "Unhealthy for Sensitive Groups" = "#ff7e00",
                     "Unhealthy" = "#ff0000", "Very Unhealthy" = "#8f3f97", "Hazardous" = "#7e0023", "Unknown" = "#A9A9A9")
    
    plot_subtitle <- if (nrow(filtered_sites) > 0) {
      local_tz_string <- tz(rv_forecast_data()$time[1])
      # Use the first valid UTC timestamp from the filtered data
      timestamp_utc <- as.POSIXct(filtered_sites$UTC[1], format = "%Y-%m-%dT%H:%M", tz = "UTC")
      timestamp_local <- with_tz(timestamp_utc, local_tz_string)
      display_timestamp <- format(timestamp_local, "Data from %I:%M %p %Z on %b %d, %Y")
      paste(display_timestamp, "|", nrow(filtered_sites), "locations found")
    } else {
      "No locations found with recent data for the selected pollutant"
    }
    
    ggplot() +
      # Layer 1: The state background
      geom_sf(data = state_shape, fill = "gray95", color = "black") +
      
      # <-- ADD THIS LINE: Draw the county outlines on top of the state
      geom_sf(data = counties_shape, fill = NA, color = "gray70", linewidth = 0.25) +
      
      # Layer 3: The data points, drawn on top of everything else
      geom_point(data = filtered_sites, aes(x = Longitude, y = Latitude, color = CategoryName), size = 5, alpha = 0.9) +
      geom_text(data = filtered_sites, aes(x = Longitude, y = Latitude, label = AQI), color = "black", fontface = "bold", nudge_y = 0.08) +
      
      # Continue with the rest of your ggplot code...
      scale_color_manual(name = "AQI Category", values = aqi_colors, drop = FALSE) +
      labs(
        title = paste("Active", input$pollutant_select, "Monitors in", state_code),
        subtitle = plot_subtitle,
        caption = "Data Source: U.S. EPA AirNow API"
      ) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom"
      )
  })
  
  # Drought Map
  output$drought_map_plot <- renderPlot({
    withProgress(message = 'Generating Drought Map', value = 0, {
      
      # --- 1. Define Map Aesthetics ---
      incProgress(0.1, detail = "Setting up colors")
      drought_colors <- c("0"="#FFFF00", "1"="#FCD37F", "2"="#FFAA00", "3"="#E60000", "4"="#730000")
      drought_labels <- c("0"="D0 (Abnormally Dry)", "1"="D1 (Moderate Drought)", "2"="D2 (Severe Drought)", "3"="D3 (Extreme Drought)", "4"="D4 (Exceptional Drought)")
      
      # --- 2. Fetch and Process Drought Data ---
      incProgress(0.3, detail = "Downloading shapefile...")
      drought_url <- "https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip"
      temp_zip <- tempfile(fileext = ".zip")
      temp_dir <- tempdir()
      
      tryCatch({
        download.file(drought_url, temp_zip, mode = "wb", quiet = TRUE)
        unzip(temp_zip, exdir = temp_dir)
      }, error = function(e) { stop("Failed to download or unzip drought data.") })
      
      incProgress(0.5, detail = "Reading shapefile...")
      
      # --- START: CORRECTED FILE SELECTION ---
      # This specifically finds the correct shapefile instead of grabbing the first one.
      shp_file <- list.files(temp_dir, pattern = "USDM_.*\\.shp$", full.names = TRUE)
      if (length(shp_file) == 0) {
        stop("Could not find the USDM shapefile in the downloaded zip archive.")
      }
      drought_data <- st_read(shp_file[1], quiet = TRUE)
      # --- END: CORRECTED FILE SELECTION ---
      
      # --- 3. Fetch State Boundaries ---
      incProgress(0.7, detail = "Fetching state boundaries...")
      contiguous_states <- suppressMessages(tigris::states(cb = TRUE, resolution = '5m') %>%
                                              filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")))
      
      # --- 4. Get Map Metadata ---
      release_date_str <- str_extract(basename(shp_file), "[0-9]{8}")
      release_date <- as.Date(release_date_str, format = "%Y%m%d")
      map_subtitle <- paste("Map released", format(release_date, "%B %d, %Y"))
      map_caption <- "Data Source: The U.S. Drought Monitor (droughtmonitor.unl.edu)"
      
      # --- 5. Build and Return the Map ---
      incProgress(0.9, detail = "Building map...")
      
      ggplot() +
        # Reverted column name to DM, which is correct for this file
        geom_sf(data = drought_data, aes(fill = as.factor(DM)), color = NA) +
        geom_sf(data = contiguous_states, fill = NA, color = "black", size = 0.25) +
        coord_sf(
          xlim = st_bbox(contiguous_states)[c("xmin", "xmax")],
          ylim = st_bbox(contiguous_states)[c("ymin", "ymax")],
          expand = FALSE
        ) +
        scale_fill_manual(
          name = "Intensity",
          values = drought_colors,
          labels = drought_labels[names(drought_colors) %in% unique(drought_data$DM)]
        ) +
        labs(
          title = "Current U.S. Drought Monitor (Contiguous U.S.)",
          subtitle = map_subtitle,
          caption = map_caption
        ) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          plot.caption = element_text(hjust = 0.5, size = 8),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(face = "bold")
        )
    })
  })
  
  
  # Safety Status
  output$safety_status <- renderUI({
    # Combine the checked items from all three groups
    checked <- c(input$safety_checks_planning, 
                 input$safety_checks_onsite, 
                 input$safety_checks_comms)
    
    num_checked <- length(checked)
    total <- 12 # The new total number of items
    
    if (num_checked == total) {
      div(class = "alert alert-success",
          icon("check-circle"), " All safety items checked! Ready to proceed with caution.")
    } else {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"), sprintf(" %d of %d items checked", num_checked, total))
    }
  })
  
  # =============================================================================
  # START: SPOT FORECAST SERVER LOGIC
  # =============================================================================
  
  # Zone Forecast Header (was spot_forecast_header)
  output$zone_forecast_header <- renderUI({
    df <- rv_forecast_data()
    if (is.null(df) || nrow(df) == 0) return(p("Enter a location and click 'Get Forecast' to generate the zone forecast."))
    
    tagList(
      h4(paste("Zone Forecast for", input$location_input)),
      p(paste("National Weather Service", toupper(rv_nws_office()))),
      p(format(Sys.time(), "%I:%M %p %Z %a %b %d %Y")),
      br(),
      p(paste("Forecast is based on ignition time of", format(df$time[1], "%I:%M %p %Z on %B %d."))),
      p("If conditions become unrepresentative, contact the National Weather Service.")
    )
  })
  
  # General Fire Weather Discussion (was spot_forecast_discussion)
  output$zone_forecast_discussion <- renderUI({
    discussion_text <- rv_fire_discussion()
    if (is.null(discussion_text)) return(NULL)
    
    pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;", discussion_text)
  })
  
  # Zone-Specific Trend Forecast
  output$zone_forecast_ui <- renderUI({
    zone_text <- rv_zone_forecast()
    if (is.null(zone_text)) return(NULL)
    
    tagList(
      h4(".ZONE-SPECIFIC FORECAST TRENDS..."),
      pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;", zone_text)
    )
  })
  
  # Observer to fetch HMS data when the date changes
  observeEvent(input$hms_date_selector, {
    selected_date <- input$hms_date_selector
    
    # Show a notification to the user
    showNotification(paste("Fetching HMS data for", selected_date, "..."), 
                     id = "hms_loading", type = "message", duration = NULL)
    
    # Fetch the data using the helper function
    hms_results <- get_hms_data(selected_date)
    
    # Store the results in the reactive value
    rv_hms_data(hms_results)
    
    # Remove the loading notification
    removeNotification("hms_loading")
    
    # Inform the user of the result
    if (is.null(hms_results) || (is.null(hms_results$fire) && is.null(hms_results$smoke))) {
      showNotification(paste("No HMS fire or smoke data was found for", selected_date), type = "warning")
    } else {
      showNotification("HMS data loaded successfully.", type = "message", duration = 4)
    }
  }, ignoreInit = FALSE) # ignoreInit=FALSE ensures it runs on startup with the default date
  
  # Render the interactive Leaflet map
  output$hms_map <- renderLeaflet({
    hms_data <- rv_hms_data()
    
    smoke_palette <- colorFactor(
      palette = c("Light" = "#D3D3D3", 
                  "Medium" = "#808080", 
                  "Heavy" = "#2B2B2B", 
                  "Unknown" = "#BEBEBE"),
      domain = c("Light", "Medium", "Heavy", "Unknown"),
      ordered = TRUE
    )
    
    # Start building the base map (no changes)
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
    
    # Add smoke polygons if they exist (no changes)
    if (!is.null(hms_data$smoke) && nrow(hms_data$smoke) > 0) {
      map <- map %>%
        addPolygons(
          data = hms_data$smoke,
          fillColor = ~smoke_palette(DensityCategory),
          fillOpacity = 0.5,
          weight = 1,
          color = "black",
          popup = ~paste("<b>Smoke Density:</b>", DensityCategory),
          group = "Smoke Plumes"
        )
    }
    
    # Add fire points if they exist
    if (!is.null(hms_data$fire) && nrow(hms_data$fire) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = hms_data$fire,
          
          # --- MODIFICATION 1: DYNAMIC RADIUS ---
          # Use a logarithmic scale to handle the wide range of FRP values.
          # We add 1 to avoid log(0) and multiply by 2 for better visual scaling.
          radius = ~log(FRP + 1) * 2,
          
          color = "red",
          stroke = FALSE,
          fillOpacity = 0.7, # Slightly reduced opacity
          popup = ~paste0(
            "<b>Fire Detection Details</b><br>",
            "<b>Time:</b> ", substr(Time, 1, 2), ":", substr(Time, 3, 4), " UTC<br>",
            "<b>Source:</b> ", Satellite, " (", Method, ")<br>",
            "<b>Intensity (FRP):</b> ", round(FRP, 2), " MW"
          ),
          group = "Fire Detections"
        )
    }
    
    # Add layer controls (no changes)
    map <- map %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite"),
        overlayGroups = c("Smoke Plumes", "Fire Detections"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Add smoke legend if there's data (no changes)
    if (!is.null(hms_data$smoke) && nrow(hms_data$smoke) > 0) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          pal = smoke_palette,
          values = hms_data$smoke$DensityCategory,
          title = "Smoke Density",
          opacity = 0.7
        )
    }
    
    # --- MODIFICATION 2: ADD CUSTOM LEGEND FOR FIRE SIZE ---
    # Create HTML for a custom legend control
    legend_html <- "
    <div style='background-color: rgba(255, 255, 255, 0.8);
                padding: 10px;
                border-radius: 5px;
                border: 1px solid #ccc;'>
      <h4 style='margin-top:0;'>Fire Intensity (FRP)</h4>
      <div style='display: flex; align-items: center; margin-bottom: 5px;'>
        <div style='background:red; width:%1$spx; height:%1$spx; border-radius:50%%; margin-right:8px;'></div><span>10 MW</span>
      </div>
      <div style='display: flex; align-items: center; margin-bottom: 5px;'>
        <div style='background:red; width:%2$spx; height:%2$spx; border-radius:50%%; margin-right:8px;'></div><span>100 MW</span>
      </div>
      <div style='display: flex; align-items: center;'>
        <div style='background:red; width:%3$spx; height:%3$spx; border-radius:50%%; margin-right:8px;'></div><span>1000 MW</span>
      </div>
    </div>
  "
    
    # Calculate the pixel diameter for the legend examples using the same formula as the map
    # Diameter is 2 * radius
    size_10mw <- 2 * (log(10 + 1) * 2)
    size_100mw <- 2 * (log(100 + 1) * 2)
    size_1000mw <- 2 * (log(1000 + 1) * 2)
    
    # Add the custom control to the map if there is fire data
    if (!is.null(hms_data$fire) && nrow(hms_data$fire) > 0) {
      map <- map %>%
        addControl(
          html = sprintf(legend_html, size_10mw, size_100mw, size_1000mw),
          position = "bottomleft"
        )
    }
    
    map # Return the final map object
  })
  
  # Download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("burn_forecast_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- rv_forecast_data()
      if (!is.null(df)) {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

# =============================================================================
# 6. RUN THE APP
# =============================================================================
shinyApp(ui = ui, server = server)
