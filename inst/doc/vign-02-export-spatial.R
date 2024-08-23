## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5, 
  fig.align = "default"
)

## ----create-model-------------------------------------------------------------
library(raem)

# aquifer parameters
k = 10    # hydraulic conductivity, m/d
top = 30  # aquifer top elevation, m
base = 0  # aquifer bottom elevation, m
n = 0.2   # aquifer effective porosity, -

# create elements
w1 = well(x = -500, y = 100, Q = 1000)
w2 = well(x = 300, -200, Q = 1200)
as = areasink(x = 0, y = 0, R = 1500, N = 0.3 / 365)
uf = uniformflow(TR = k * (top - base), gradient = 0.002, angle = -135) # SW direction
rf = constant(x = 1000, y = 1000, h = 25)

# create and solve model
m = aem(k, top, base, n, w1, w2, as, uf, rf)

# output grid
xg = seq(-1200, 1000, length = 100)
yg = seq(-700, 500, length = 100)

# plot head contours
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20, labcex = 0.8,
         xlab = 'x (m)', ylab = 'y (m)')


## ----export-vector, message = FALSE-------------------------------------------
library(sf)
library(isoband)

# create a 10 by 10 m contouring grid and get the heads as a grid
xg = seq(-1200, 1000, by = 10)
yg = seq(-700, 500, by = 10)

h = heads(m, xg, yg, as.grid = TRUE)

# optionally, set the x and y origin corresponding to (0, 0) 
# in the requested coordinate system
xorigin = 195600
yorigin = 203500
epsg = 31370

# create the isolines with the specified levels
# the y-coordinates need to be reversed here for isolines()
lvls = seq(13.5, 24.5, by = 0.5)
isolines = isolines(xg + xorigin, 
                    rev(yg) + yorigin,
                    h,
                    levels = lvls)

# convert to sfg object, create sfc column for sf object
isolines_sf = st_sf(level = as.numeric(names(isolines)),
                    geometry = st_sfc(iso_to_sfg(isolines)),
                    crs = epsg)

plot(isolines_sf)


## ----export-shapefile, eval = FALSE-------------------------------------------
#  # export as shapefile
#  write_sf(isolines_sf, 'isolines.shp')

## ----terra, message = FALSE---------------------------------------------------
library(terra)

# set extent and create raster
extent = c(range(xg) + xorigin, range(yg) + yorigin)
r = rast(h, crs = paste('epsg', epsg, sep = ':'), extent = ext(extent))

# plot
plot(r)

## ----write-raster, eval = FALSE-----------------------------------------------
#  writeRaster(r, 'heads.tiff', overwrite = TRUE)

