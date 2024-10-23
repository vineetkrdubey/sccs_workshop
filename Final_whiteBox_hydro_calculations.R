rm(list = ls())

install.packages("whitebox")
if (!require("remotes")) install.packages('remotes')
remotes::install_github("opengeos/whiteboxR", build = FALSE)
install.packages("terra")


####Calculating Flow direction, Flow Accumulation, Stream Order and Hydrologically conditioned dem 

library(whitebox)
whitebox::install_whitebox()
library(terra)
setwd("./Inputdem")
# Set the path to the DEM and output files
input_dem <- "./input_dem.tif"
output_flow_dir <- "flow_direction.tif"
output_flow_accum <- "flow_accumulation.tif"

originaldem <- rast("input_dem.tif")
plot(originaldem)

# Run the breach depressions tool
wbt_breach_depressions(dem = input_dem, output = "breached_dem.tif")

breached_dem <- rast("breached_dem.tif")
plot(breached_dem)
# calculate flow direction with the breached DEM
wbt_d8_pointer(dem = "breached_dem.tif", output = output_flow_dir)

flow_direction <- rast("flow_direction.tif")
plot(flow_direction)

# calculate flow accumulation
wbt_d8_flow_accumulation(input = "breached_dem.tif", output = output_flow_accum)
flow_accumulation <- rast("flow_accumulation.tif")
plot(flow_accumulation)


# Step 1: Re-extract streams with a lower threshold (e.g., 100 instead of 500)
wbt_extract_streams(flow_accum = output_flow_accum, output = "streams.tif", threshold = 100)

# Step 2: Calculate Strahler stream order
wbt_strahler_stream_order(
  d8_pntr = output_flow_dir,      # Flow direction raster
  streams = "streams.tif",        # Extracted stream network
  output = "strahler_order.tif"   # Output file for Strahler stream order
)

strahler_order_raster <- rast("strahler_order.tif")
plot(strahler_order_raster)

