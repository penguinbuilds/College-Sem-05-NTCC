library(raster)
library(RNetCDF)
library(sf)
library(tsbox)

nc <- open.nc("../data/cmip6/tasmax_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc") # nolint

tasmax.dates <- as.Date(var.get.nc(nc, "time"), origin = "1850-01-01 00:00:00") # nolint

tasmax.scenes <- sapply(1:length(tasmax.dates), function(z) { # nolint
    grid <- var.get.nc(nc, "tasmax", start = c(NA, NA, z), count = c(NA, NA, 1))
    x <- raster(grid,
        xmn = -90, xmx = 90, ymn = 0, ymx = 360,
        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )
    x <- rotate(flip(t(x), 2))
    x <- (x - 273.15)
    return(x)
})

weather_station <- st_sfc(st_point(c(28.5447704, 77.3267306)), crs = 4326)

y <- sapply(tasmax.scenes, function(scene) extract(scene, as_Spatial(weather_station))) # nolint

x <- 1970 + (as.numeric(tasmax.dates) / 365.25)

tasmax.series <- ts(y, start = floor(min(x)), end = floor(max(x)), deltat = 1 / 12) # nolint

plot(tasmax.series,
    col = "darkred", ylab = "Monthly Mean High Temperatures (C)",
    type = "l", lwd = 3, bty = "n", las = 1, fg = NA
)

grid(nx = NA, ny = NULL, lty = 1)

close.nc(nc)
