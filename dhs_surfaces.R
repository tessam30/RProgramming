# Rwanda rasters to basic maps #

library(tidyverse)
library(raster)
library(rgdal)
library(sp)
library(dismo)
library(mapview)
library(maps)
library(rasterVis)

setwd("~/Rwanda/GIS/Rasters")

# Read in lay names for each raster
  rw_names <- read.csv("Rwanda_modelled_surfaces.csv", stringsAsFactors = FALSE)
  rw_names$Raster <- gsub('.{4}$', '', rnames$Raster) # remove last 4 digits in string
  
# Create a set of rasters
  rlist <- list.files(pattern="*.tif")
  rlist2 <- rlist[c(FALSE, TRUE)]
  
  # Apply the raster function to each element in list
  rwa <- lapply(rlist, raster)
  
  # Create a raster stack for quick min/max calcuations
  rwa_stack <- raster::stack(rlist)
  rwa_stack2 <- raster::stack(rlist2)
  minValue(rwa_stack)
  minV <- min(minValue(rwa_stack))
  maxV <- max(maxValue(rwa_stack))
  
  
  # print for a crosswalk
  cat(rlist,sep="\n")

# Set rasters to be easily mapped in ggplot
  ind_list <- lapply(rwa, rasterToPoints)

# Function to extract indicator of interest, convert to data.frame and plot
  map_surface <- function(indic) {
      
      # look up position of indicator in excel table
      indic_name = as.name(indic)
      
      # Look for the string entered, if there return the row value in the excel table
      y = match(indic, rw_names$Indicator)
      
      
      # To really be corred, the next step should search for the string
      # rast_name = paste(rw_names$Raster[y], "_v01", sep = "")
      # rast = as.name(rast_name)
      # print(rast)
      
      # convert corresponding point data to dataframe based on match return
      df <- data.frame(ind_list[[y]])
      
      df = setNames(df, c("lon", "lat", "z"))
      names(df)
      
       ggplot(df, aes(y = lat, x = lon)) +
          geom_raster(aes_(fill = quote(z))) +
          #scale_fill_gradient(low = "#ffffcc", high = "#800026")+
         scale_fill_distiller(palette = "YlOrRd", direction = 1) +
         theme_light() + labs(title = rw_names$Indicator.1[y])
    }
  
map_surface("stunted")

# To do -- map values to a consistent range; make surfaces comparable

# Using rasterVis to facet all plots -- requires a raster stack
  gplot(rwa_stack2 ) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    coord_equal()

  # documentation https://artax.karlin.mff.cuni.cz/r-help/library/rasterVis/html/gplot-methods.html
  
  
