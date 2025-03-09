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
ic <- ee$ImageCollection("UCSB-CHG/CHIRPS/PENTAD") |>
  ee$ImageCollection$filter(ee$Filter$calendarRange(1,12,"month"))

chirps.db <- ee$ImageCollection$fromImages(
  ee_year$map(
    ee_utils_pyfunc(
      function(x){
        ic$filter(ee$Filter$calendarRange(x,x,"year"))$
          mean()
      }
    )
  )
) |>
  ee$ImageCollection$toBands() |>
  ee$Image$clip(box)

# 3. Download raster data -------------------------------------------------
if(!dir.create("output")){dir.create("output")}
if(!dir.create("output/raster")){dir.create("output/raster")}
pp <-  ee_as_rast(
  image = chirps.db,
  region = box,
  dsn = 'output/raster/chirps_pp.tif',
  scale = 5566,
  crs = "epsg:32718")

# 4. Zonal statistic by administration limits  ----------------------------
anios <- 2001:2024
names(pp) <- gsub(
  ".*(precipitation).*",
  "\\1",
  str_to_lower(names(pp))) |>
  paste0("_mean_",anios)

dist |>
  select(ubigeo) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pp,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pp_mean_yearly_peru_district_2001-2024.csv")

prov |>
  mutate(ubigeo = paste0(ccdd,ccpp))|>
  select(ubigeo)|>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pp,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pp_mean_yearly_peru_province_2001-2024.csv")

dist |>
  group_by(ccdd,nombdep) |>
  summarise() |>
  select(ccdd) |>
  rename(ubigeo = ccdd) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pp,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pp_mean_yearly_peru_departament_2001-2024.csv")
