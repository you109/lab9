library(sp)           # Spatial data handling
library(raster)       # Raster data processing
library(terra)        # Modern spatial data handling (alternative to raster)
library(gstat)        # Geostatistical modeling
library(animation)    # Animation for time series
library(RColorBrewer) # Color palettes
library(sf)           # Modern spatial vector handling
library(dplyr)        # Data manipulation

# Load the Digital Elevation Model (DEM) raster
DEM <- rast("Shale_Hills_DEM.tif")

# Load the watershed boundary shapefile
ws <- vect("./Watershed_Boundary/wshd_nad83_edit.shp")

# === Plot the DEM and watershed boundary ===
plot(DEM, main = "Shade Hill", xlab = "Easting (m)", ylab = "Northing (m)", col.axis = "darkgreen") 
plot(ws, add = TRUE)  # Overlay watershed boundary on the DEM

# === Load and process soil moisture data ===
sm <- read.csv("SM_10cm_2010.csv")  # Load CSV with soil moisture data
sm <- sm %>% dplyr::select(-SID)          # Remove the 'SID' column

# Convert data into a spatial object using X and Y coordinates
coordinates(sm) <- ~ X+Y 

# === Plot soil moisture stations on top of the DEM ===
plot(sm, add = TRUE, pch = 16)  # Overlay soil moisture stations as points

# Add a legend for soil moisture stations
par(xpd = TRUE)  # Allows drawing outside plot boundaries
legend('topright', legend = 'Soil Moisture Station', pch = 16)
par(xpd = FALSE) # Reset settings

# === Define a color scale using RColorBrewer ===
display.brewer.pal(3, 'GnBu')  # Display a 3-color palette
col.pal <- brewer.pal(3, 'GnBu') # Store the colors

# === Assign colors based on soil moisture values ===
intervals <- seq(min(sm@data[,1]), max(sm@data[,1]), length = 8)  # Divide moisture range into 8 bins
idx.col <- cut(sm@data[,1], intervals, include.lowest = TRUE)  # Assign each value to a bin

# === Re-plot the DEM with soil moisture stations colored by value ===
plot(DEM, col = brewer.pal(9, 'Greys'), 
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col = col.pal[idx.col], add = TRUE) # Color-coded soil moisture points
plot(ws, add = TRUE)  # Re-add watershed boundary

# === Add legend for soil moisture values ===
leg <- paste0(round(intervals[-8], digits = 2), '-', round(intervals[-1], digits = 2))  # Legend text
legend('bottomright', legend = leg, pch = 16, col = col.pal , title = 'Soil Moisture [%]')

# === Alternative: Use Quantiles for Color Binning ===
intervals <- quantile(sm@data[,1], seq(0,1,length=8), na.rm = TRUE)  # Compute quantile-based bins

# === Animation Setup ===
intervals <- quantile(sm@data[,2], seq(0,1,length=8), na.rm = TRUE)  # Compute quantiles for second column
idx.col <- cut(sm@data[,2], intervals, include.lowest = TRUE)  # Assign color bins
col.pal[idx.col]  # Retrieve colors

# === Re-plot with updated quantile binning ===
plot(DEM, col = brewer.pal(9, 'Greys'), 
     xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
plot(sm, pch = 16, col=col.pal[idx.col], add = TRUE)  # Plot moisture points with new colors
plot(ws, add = TRUE)  # Re-add watershed boundary

# === Add updated legend ===
leg <- paste0(round(intervals[-8], digits = 2), '-', round(intervals[-1], digits = 2))  
par(xpd = TRUE)  
legend('bottomright', legend = leg, pch = 16, col = col.pal, title = 'Soil Moisture [%]')

# === Animate Soil Moisture Data Over Time ===
ani.options(interval = 0.3)  # Set animation frame interval

barplot(height = ppt$Total_Precip_mm, names = ppt$TmStamp)


# Read the daily total precipitation data from CSV file into a data frame
ppt <- read.csv("SHCZO_ppt_2010.csv")

# Set up the plotting area to have 2 rows and 1 column of plots, with custom margins (bottom, left, top, right)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))


for(i in 1:ncol(sm@data)){  # Loop through each time step
      
      intervals <- quantile(sm@data[,i], seq(0,1,length=8), na.rm = TRUE)  # Compute quantiles
      idx.col <- cut(sm@data[,i], intervals, include.lowest = TRUE)  # Assign colors
      
      # === Update plot at each timestep ===
      plot(DEM, col = brewer.pal(9, 'Greys'), 
           xlab = 'Easting [m]', ylab = 'Northing [m]', main = 'Shale Hills Elevation [m]')
      plot(sm, pch = 16, col=col.pal[idx.col], add = TRUE)  # Color-coded points
      plot(ws, add = TRUE)  # Watershed boundary
      
      # === Update legend dynamically ===
      leg <- paste0(round(intervals[-8], digits = 2), '-', round(intervals[-1], digits = 2))  
      legend('bottomright', legend = leg, pch = 16, col = col.pal, title = 'Soil Moisture [%]')
      
      ####################################
      #
      # Create a barplot of daily total precipitation (in mm)
      # 'height' specifies the values for the bars (daily precipitation)
      # 'names' sets the x-axis labels using timestamps
      barplot(height = ppt$Total_Precip_mm, names = ppt$TmStamp)
      
      # Find the index in the precipitation data that matches the current timestamp from the animation
      # 'gsub' removes the 'X' prefix from the column name (used in spatial data frame sm@data)
      # 'which' finds the position of the matching timestamp in the ppt data
      # idx.v <- 

      # Add a vertical red line to the barplot at the matched index to indicate the current time step in the animation
      # abline()
      ####################################
      
      ani.pause()  # Pause for animation effect
}


