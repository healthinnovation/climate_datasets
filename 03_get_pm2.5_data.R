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

# 2. GHAP data ------------------------------------------------------------
ghap.db <- ee$ImageCollection("projects/sat-io/open-datasets/GLOBAL-SATELLITE-PM25/ANNUAL") |>
  ee$ImageCollection$filter(ee$Filter$calendarRange(2001,2022,"year")) |>
  ee$ImageCollection$toBands() |>
  ee$Image$clip(box)

# 3. Download raster data -------------------------------------------------
if(!dir.create("output")){dir.create("output")}
if(!dir.create("output/raster")){dir.create("output/raster")}
pm2.5 <-  ee_as_rast(
  image = ghap.db,
  region = box,
  dsn = 'output/raster/ghap_pm25.tif',
  scale = 1000,
  crs = "epsg:32718")

# 4. Zonal statistic by administration limits  ----------------------------
anios <- 2001:2022
names(pm2.5) <- gsub(
  ".*(V6GL02).*",
  "\\1",
  names(pm2.5)) |>
  tolower() |>
  paste0("_mean_",anios)

dist |>
  select(ubigeo) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pm2.5,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pm2.5_mean_yearly_peru_district_2001-2022.csv")

prov |>
  mutate(ubigeo = paste0(ccdd,ccpp))|>
  select(ubigeo)|>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pm2.5,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pm2.5_mean_yearly_peru_province_2001-2022.csv")

dist |>
  group_by(ccdd,nombdep) |>
  summarise() |>
  select(ccdd) |>
  rename(ubigeo = ccdd) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = pm2.5,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/pm2.5_mean_yearly_peru_departament_2001-2022.csv")
