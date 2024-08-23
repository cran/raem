## ----set-up, include = FALSE--------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5, 
  fig.align = "default"
)

## ----aq-params----------------------------------------------------------------
library(raem)

k = 10     # hydraulic conductivity, m/d
top = 10   # aquifer top elevation, m
base = -15 # aquifer bottom elevation, m
n = 0.2    # aquifer effective porosity, -


## ----well---------------------------------------------------------------------
# create well elements
w1 = well(x = 200, y = 0, Q = 300)   
w2 = well(x = -200, y = 0, Q = 1000)

# create the model. This automatically solves the system of equations.
m = aem(k = k, top = top, base = base, n = n, w1, w2)

# set up the contouring grid 
xg = seq(-600, 600, length = 100)
yg = seq(-300, 300, length = 100)

# plot head contours and streamlines
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20, drawlabels = FALSE)
contours(m, xg, yg, variable = 'streamfunction', col = 'orange',
         nlevels = 20, drawlabels = TRUE, add = TRUE)


## ----headwell-----------------------------------------------------------------
# create head-specified wells
hw1 = headwell(xw = 300, yw = 100, hc = 6)
hw2 = headwell(xw = -200, yw = -100, xc = 0, yc = 0, hc = 7)

# create reference point element
rf = constant(x = -1000, y = 0, h = 8)

# create and solve model
m = aem(k, top, base, n, hw1, hw2, rf) 

# plot head contours
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)

## ----headwell-Q---------------------------------------------------------------
# computed discharge of hw1
m$elements$hw1$parameter

# computed discharge of hw2
element_discharge(m, name = 'hw2')

## ----line-sink----------------------------------------------------------------
# create line-sink
ls = linesink(x0 = -200, y0 = 200, x1 = 200, y1 = -200, sigma = 5)

# create and solve the model
m = aem(k, top, base, n, ls)

# plot head contours and line-sink geometry
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20, drawlabels = FALSE)
plot(m, add = TRUE)

## ----headlinesink-------------------------------------------------------------
el = list(rf = rf) # list of elements
nls = 10           # number of line-sinks
xls = seq(-700, 700, length = nls + 1) # x-coordinates of line-sinks
yls = c(rep(c(-25, 25), nls), -25)     # y-coordinates of line-sinks
hc = 7             # stream level, m
res = 2            # hydraulic resistance of streambed, days
width = 3          # stream width, m

# create head-specified line-sinks
for(i in seq(nls)) {
  hl = headlinesink(x0 = xls[i], 
                    x1 = xls[i + 1], 
                    y0 = yls[i],
                    y1 = yls[i + 1],
                    hc = hc,
                    resistance = res,
                    width = width)
  el[[paste0('hls_', i)]] = hl
}

# create and solve model
m = aem(k, top, base, n, el, verbose = TRUE)

# plot head contours
contours(m, xg, yg, col = 'dodgerblue', nlevels = 10)
plot(m, add = TRUE)


## ----area-sink----------------------------------------------------------------
# add area-sink to list of elements
el$as = areasink(x = 0, y = 0, N = 0.001, R = 1000) 

# create and solve the model
m = aem(k, top, base, n, el)

# plot head contours
contours(m, xg, yg, col = 'dodgerblue', nlevels = 10)
plot(m, add = TRUE)

## ----headareasink-------------------------------------------------------------
# create head-specified area-sink
has = headareasink(x = 0, 
                   y = 200, 
                   h = 5, 
                   resistance = 1, 
                   R = 100)

# create and solve model
m = aem(k, top, base, n, w1, w2, rf, has)

# plot head contours and area-sink geometry
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)
plot(has, add = TRUE, col = adjustcolor('grey50', alpha = 0.5))

## ----uniformflow--------------------------------------------------------------
# create uniform flow element
uf = uniformflow(TR = k * (top - base), gradient = 0.001, angle = -30)

# create model
m = aem(k, top, base, n, uf, rf)

# plot head contours
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)

# add stream lines
contours(m, xg, yg, variable = 'streamfunction', 
         col = 'orange', nlevels = 20, add = TRUE)

## ----example-model, fig.show="hold"-------------------------------------------
# aquifer parameters ----
k = 15          # hydraulic conductivity, m/d
top = 20        # aquifer top elevation, m
base = -10      # aquifer bottom elevation, m
n = 0.2         # aquifer effective porosity, -

N = 0.2 / 365   # areal recharge rate, m/d
res = 2         # streambed resistance, d
width = 5       # stream width, m
hr = 17.5       # stream level at head of stream, m
hrg = 0.0005    # gradient of stream level, -
href = 18.5     # head at reference point, m

# stream coordinates and water level
yriv = c(seq(-1000, -300, by = 200),
         seq(-200, 200, by = 20),
         seq(300, 1000, by = 200))
hriv = hr - (yriv - yriv[1]) * hrg
nls = length(yriv)

# create elements ----
wA = well(x = -300, y = 0, Q = 550)
wB = well(x = -500, y = -300, Q = 450)
as = areasink(x = -50, y = 0, N = N, R = 2000)
rf = constant(x = 1000, y = -1000, h = 18.5)

# create model ----
m = aem(k, top, base, n) |> # first, create model with no elements
  add_element(wA) |>        # add elements
  add_element(wB) |>
  add_element(as) |> 
  add_element(rf)

# add head-specified line-sinks in a loop
for(i in seq(nls - 1)) {
  hls = headlinesink(x0 = 0, 
                     x1 = 0, 
                     y0 = yriv[i],
                     y1 = yriv[i + 1],
                     h = hriv[i],
                     resistance = res,
                     width = width
  )
  m = add_element(m, hls, name = paste('stream', i, sep = '_'))
}

# solve
m = solve(m)

# view head contours ----
xg = seq(-800, 300, length = 100)
yg = seq(-600, 300, length = 100)

contours(m, xg, yg, col = 'dodgerblue', levels = seq(16, 18.5, 0.1), labcex = 0.8,
         xlab = 'x (m)', ylab = 'y (m)')
plot(m, add = TRUE)
polygon(x = c(-500, 50, 50, -500), y = c(-200, -200, 150, 150), border = 'forestgreen')
grid() # add gridlines to plot


## ----fig-inset-well-----------------------------------------------------------
# view inset near well A
xg = seq(-500, 50, length = 100)
yg = seq(-200, 150, length = 100)

contours(m, xg, yg, col = 'dodgerblue', levels = seq(16, 18, 0.05), labcex = 0.8,
         xlab = 'x (m)', ylab = 'y (m)')
grid()

# plot control points of line-sinks
for(i in m$elements) {
  if(inherits(i, 'linesink')) {
    plot(i, add = TRUE, use.widths = FALSE)
    points(i$xc, i$yc, pch = 16, cex = 0.8) 
  }
}


## ----heads--------------------------------------------------------------------
heads(m, x = c(-350, -200), y = -100)

# as a grid
heads(m, x = seq(-500, -100, length = 8), y = seq(-200, 100, 60), as.grid = TRUE)

## ----cross-section-plot, fig.height=3, out.width='75%', fig.align='center'----
xprofile = seq(-800, 400, length = 1000)
hprofile = heads(m, x = xprofile, y = -100)

plot(xprofile, hprofile, type = 'l', xlab = 'x (m)', ylab = 'head (m)')

## ----discharge, warning=TRUE--------------------------------------------------
discharge(m, x = c(-350, -200), y = -100, z = 15)

# NA's for z-component
discharge(m, x = c(-350, -200), y = -100, z = top) 

# as.grid
str(discharge(m, 
              x = seq(-350, -200, length = 5), 
              y = seq(-200, -100, length = 4),
              z = c(10, 15, length = 3),
              as.grid = TRUE))

# magnitude
discharge(m, x = c(-350, -200), y = -100, z = 15, magnitude = TRUE)


## ----tracelines---------------------------------------------------------------
# calculate particle traces
paths = tracelines(m, 
                   x0 = -600, 
                   y0 = seq(-200, 200, 50),
                   z0 = top, 
                   times = seq(0, 5 * 365, 365 / 10)) # 10 steps per year for 5 years

# plot head contours and element geometries around well A
xg = seq(-600, 100, length = 100)
yg = seq(-200, 200, length = 100)

contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)
plot(m, add = TRUE)

# add tracelines to plot
plot(paths, add = TRUE, col = 'orange')

# compute and plot endpoints
endp = endpoints(paths)
points(endp[,c('x', 'y')])

## ----backward-tracking--------------------------------------------------------
# compute backward particle tracking with retardation
backward = tracelines(m, 
                      x0 = -250, 
                      y0 = -50, 
                      z = 10, 
                      forward = FALSE, 
                      R = 1.5, 
                      times = seq(0, 5 * 365, 365 / 10))

# plot the head contours and element geometries around well A
contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)
plot(m, add = TRUE)

# plot backward particle trace with a marker every 1.5 years
plot(backward, col = 'forestgreen', add = TRUE, marker = 1.5 * 365)

## ----capzone------------------------------------------------------------------
# 5-year capture zone of well A
cpA_5 = capzone(m, wA, time = 5 * 365, npar = 10) 

# plot head contours and element geometries
xg = seq(-800, 300, length = 100)
yg = seq(-600, 300, length = 100)

contours(m, xg, yg, col = 'dodgerblue', nlevels = 20)
plot(m, add = TRUE)

# plot capture zone output
plot(cpA_5, add = TRUE)

