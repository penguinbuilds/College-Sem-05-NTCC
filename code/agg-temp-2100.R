library(raster)
library(RNetCDF)

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

indices <- which((tasmax.dates >= as.Date(paste0("2100-01-01"))) &
    (tasmax.dates <= as.Date(paste0("2100-12-31"))))

tasmax.2100 <- tasmax.scenes[[indices[1]]] # nolint

for (scene in tasmax.scenes[indices[2:length(indices)]]) {
    values(tasmax.2100) <- pmax(values(tasmax.2100), values(scene)) # nolint
}

plot(tasmax.2100, main = "2100", col = colorRampPalette(c("navy", "lightgray", "red"))(32)) # nolint

close.nc(nc)
