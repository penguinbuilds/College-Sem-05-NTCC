library(raster)
library(RNetCDF)

nc <- open.nc("../data/cmip6/pr_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012.nc") # nolint

pr.dates <- as.Date(var.get.nc(nc, "time"), origin = "1850-01-01 00:00:00") # nolint

pr.scenes <- sapply(1:length(pr.dates), function(z) { # nolint
    grid <- var.get.nc(nc, "pr", start = c(NA, NA, z), count = c(NA, NA, 1))
    x <- raster(grid,
        xmn = -90, xmx = 90, ymn = 0, ymx = 360,
        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )
    x <- rotate(flip(t(x), 2))
    x <- x * 3401.575 * 30
    return(x)
})

indices <- which((pr.dates >= as.Date(paste0("2100-01-01"))) &
    (pr.dates <= as.Date(paste0("2100-12-31"))))

pr.2100 <- pr.scenes[[indices[1]]] # nolint

for (scene in pr.scenes[indices[2:length(indices)]]) {
    pr.2100 <- pr.2100 + scene # nolint
}

plot(log(pr.2100), main = "2100", col = colorRampPalette(c("tan2", "lightgray", "darkgreen"))(32)) # nolint

close.nc(nc)
