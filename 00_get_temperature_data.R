library(geoidep)
library(rgee)
library(sf)
library(terra)
library(tidyverse)
ee_Initialize()
sf_use_s2(use_s2 = FALSE)
# 1. Loading spatial data -------------------------------------------------
dist <- get_districts()
prov <- get_provinces()
dep <-  get_departaments()
box <- dep |> 
  summarise() |> 
  st_bbox() |> 
  st_as_sfc() |> 
  sf_as_ee()
# 2. MODIS data -----------------------------------------------------------
ee_year <- seq(from = 2001, to = 2024, by = 1) |> ee$List()
ic <- ee$ImageCollection("MODIS/061/MOD11A2") |>
  ee$ImageCollection$filter(ee$Filter$calendarRange(1,12,"month"))

# Quality filter
filter.clear.sky <- function(image) {
  clearSky <-  image$select("Clear_sky_days")
  clearDaysCount <- clearSky$bitwiseAnd(255)$toUint8()$bitCount()
  mask <-  clearDaysCount$gte(3)
  image <- image$select("LST_Day_1km")$updateMask(mask)
  return(image) 
}

modis.db <- ee$ImageCollection$fromImages(
  ee_year$map(
    ee_utils_pyfunc(
      function(x){
        ic$filter(ee$Filter$calendarRange(x,x,"year"))$
          map(filter.clear.sky)$
          mean()$
          multiply(0.02)$
          subtract(273.15)
        }
      )
    )
  ) |> 
  ee$ImageCollection$toBands() |> 
  ee$Image$clip(box)

# 3. Download raster data -------------------------------------------------
if(!dir.create("output")){dir.create("output")}
if(!dir.create("output/raster")){dir.create("output/raster")}
lst <-  ee_as_rast(
  image = modis.db,
  region = box,
  dsn = 'output/raster/modis_lst.tif',
  scale = 1000,
  crs = "epsg:32718")

r_filled <- focal(
  lst,
  w = matrix(1, 5, 5),
  fun = mean,
  na.policy = "only",
  na.rm = TRUE
  )

# 4. Zonal statistic by administration limits  ----------------------------
anios <- 2001:2024
names(lst) <- gsub(
  ".*(lst).*",
  "\\1",
  str_to_lower(names(lst))) |> 
  paste0("_mean_",anios)

dist |> 
  st_transform(32718) |> 
  vect() |>
  terra::extract(
    x = r_filled,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  write_csv(file = "output/lst_mean_yearly_peru_district_2001-2024.csv")

prov |> 
  st_transform(32718) |> 
  vect() |> 
  terra::extract(
    x = r_filled,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  write_csv(file = "output/lst_mean_yearly_peru_province_2001-2024.csv")

dep |> 
  st_transform(32718) |> 
  vect() |> 
  terra::extract(
    x = r_filled,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |> 
  st_as_sf() |> 
  st_drop_geometry() |> 
  write_csv(file = "output/lst_mean_yearly_peru_departament_2001-2024.csv")