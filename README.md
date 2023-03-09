# australian_heartworm_dashboard
Dashboard used to show suitability for heartworm disease transmission in Australia

The RShiny dashboard as 3 primary components:
  - global: sets global parameters for the dashboard. Loads packages, loads files
  - ui: user interface. Determines how the dashboard looks
  - server: runs processes, before displaying them on the ui

Also has:
 - preprocessing: to run processes that are needed prior to global. In this case, this is where the weather data is sourced from
 - graph play.R: where I trial new figures etc
 - install.packages.R: script with all packages to reinstall if needed
 
 Files:
  - "*.nc" files: weather data
  - "*.RDS" files: large dataframes. Contain data for each postcode of Australia, for each date (within dates specified in file name)
  - "poa.list": a list of Australian postcodes, and their corresponding code in the Australian postcode shapefile
  
  