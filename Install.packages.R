#Script for all packages to reinstall if needed

packages <- c("dplyr", "readr", "purrr", "stringr", "tibble", "magick", "aws.s3",
              "ncdf4", "rgdal", "ggplot2", "raster", "rasterVis", "maptools", "maps", "tidync",
              "sf", "sp", "rgeos", "devtools", "viridis", "wesanderson", "devtools", "cropgrowdays", "PROJ", "shiny",
              "rasterVis", "shinycssloader", "rmarkdown", "tinytex", "knitr", "quarto", "shinyWidgets", "terra")


install.packages(packages, dependencies = TRUE)

install.packages("terra", dependencies = TRUE)

devtools::install_github("becarioprecario/spatialkernel", force=TRUE)


tinytex::install_tinytex()