
ADVANCED PRESCRIBED BURN WEATHER DASHBOARD
======================================


OVERVIEW
--------
A comprehensive R Shiny application designed to consolidate critical weather, smoke dispersion, and air quality data to aid land managers, ecologists, and fire practitioners in planning and executing prescribed burns safely and effectively.

A screenshot of the application ('screenshot.png') is included in the project folder for a visual preview.


ABOUT THE PROJECT
-----------------
Prescribed burning is a critical tool for ecosystem management, fuel reduction, and habitat restoration. However, its success and safety are heavily dependent on precise weather and atmospheric conditions. This dashboard was created to bridge the gap between raw weather data and actionable intelligence for burn planning.

It fetches data directly from the National Weather Service (NWS) and U.S. EPA AirNow APIs, presenting it through an intuitive, interactive interface. Users can assess everything from hourly weather trends and burn window quality to smoke dispersion potential and background air quality in a single, unified tool.


KEY FEATURES
------------
- Location-Based Forecasts: Enter any U.S. location (e.g., "Jackson, MS", "Tallahassee, FL") to get a precise forecast for your burn unit.

- Burn Prescription Dashboard: Interactively set your desired temperature, humidity, and wind speed parameters to instantly see how current and future conditions align with your prescription.

- Advanced Smoke Management: Utilizes NWS grid data to calculate and display Mixing Height, Transport Winds, Ventilation Index, and an overall Smoke Dispersion Category.

- Interactive Data Visualizations:
    - 48-hour multi-axis plot for temperature, humidity, and wind speed.
    - 24-hour bar chart showing the calculated "Burn Window Quality Score".
    - 72-hour heatmap to easily identify optimal burn periods during the day.

- Official NWS Products: Retrieves and displays the official NWS Fire Weather Planning Discussion and zone-specific trend forecasts for your area.

- Air Quality Integration (Optional):
    - Displays current and forecast Air Quality Index (AQI) for the selected location.
    - Generates a statewide map of active Ozone and PM2.5 monitors to assess regional air quality.

- Drought Monitoring: Integrates and displays the latest map from the U.S. Drought Monitor.

- Interactive Safety Checklist: A pre-burn checklist to ensure all safety protocols are considered before ignition.

- Data Export: Download the detailed 72-hour hourly forecast as a CSV file for record-keeping.


INSTALLATION AND SETUP
----------------------

Follow these steps to run the dashboard on your local machine.

PREREQUISITES:
- R: Make sure you have a recent version of R installed. You can download it at (https://cran.r-project.org/).
- RStudio: RStudio Desktop is the recommended IDE for running Shiny apps. You can download it at (https://posit.co/download/rstudio-desktop/).

STEP 1: CLONE THE REPOSITORY
Clone or download this repository to your local machine.
    
    git clone <your-repository-url>

STEP 2: INSTALL REQUIRED R PACKAGES
Open the 'app.R' file in RStudio. Run the following command in the R console to install all the necessary packages:

    install.packages(c(
      "shiny", "shinydashboard", "dplyr", "jsonlite", "weathR", "ggplot2", 
      "plotly", "tidyr", "lubridate", "tidygeocoder", "DT", "sf", "purrr", 
      "httr", "stringr", "tigris", "shinycssloaders"
    ))

STEP 3: SET UP AIR QUALITY API KEY (Optional but Recommended)
To enable the "Air Quality" tab, you need a free API key from the U.S. EPA's AirNow service.

1. Obtain an API Key:
   - Visit the AirNow API request page: https://docs.airnowapi.org/account/request/
   - Fill out the form to request your free key, which will be sent to your email.

2. Create a '.Renviron' File:
   - In the root directory of this project (the same folder containing 'app.R'), create a new, blank text file.
   - Save this file with the exact name '.Renviron'. Be careful that your text editor doesn't add a '.txt' extension.

3. Add Your Key to the File:
   - Open '.Renviron' and add the following line, pasting your key in place of 'YOUR_API_KEY_HERE':

        AIRNOW_API_KEY="YOUR_API_KEY_HERE"

   - Save and close the file.


USAGE
-----
1. Open the 'app.R' file in RStudio.
2. Click the "Run App" button at the top of the script editor, or run the following command in the console:

    shiny::runApp('app.R')

The application will launch in a new window or in your web browser.


DISCLAIMER
----------
This tool provides weather information for PLANNING PURPOSES ONLY. Forecasts are predictions and are not a substitute for continuous, on-site weather monitoring. The user assumes all responsibility for burn operations. Always verify conditions on-site, obtain proper permits, and follow all local, state, and federal regulations.


AUTHOR
------
- Rodney Cuevas
- Contact: RCuevas@mdeq.ms.gov
