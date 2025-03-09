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

# 2. SENTINEL5P data ------------------------------------------------------
ee_year <- seq(from = 2019, to = 2024, by = 1) |> ee$List()
ic <- ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2") |>
  ee$ImageCollection$filter(ee$Filter$calendarRange(1,12,"month")) |>
  ee$ImageCollection$select("NO2_column_number_density")

sen5p.db <- ee$ImageCollection$fromImages(
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
sen5p.raster <-  ee_as_rast(
  image = sen5p.db,
  region = box,
  dsn = 'output/raster/sen5p_no2.tif',
  scale = 1113.2 ,
  crs = "epsg:32718")

# 4. Zonal statistic by administration limits  ----------------------------
anios <- 2019:2024
names(sen5p.raster) <- gsub(
  ".*(NO2).*",
  "\\1",
  names(sen5p.raster)) |>
  tolower() |>
  paste0("_mean_",anios)

dist |>
  select(ubigeo) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = sen5p.raster,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/no2_mean_yearly_peru_district_2019-2024.csv")

prov |>
  mutate(ubigeo = paste0(ccdd,ccpp))|>
  select(ubigeo)|>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = sen5p.raster,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/no2_mean_yearly_peru_province_2019-2024.csv")

dist |>
  group_by(ccdd,nombdep) |>
  summarise() |>
  select(ccdd) |>
  rename(ubigeo = ccdd) |>
  st_transform(32718) |>
  vect() |>
  terra::extract(
    x = sen5p.raster,
    fun = "mean",
    bind = TRUE,
    na.rm=TRUE) |>
  st_as_sf()  |>
  st_drop_geometry() |>
  write_csv(file = "output/no2_mean_yearly_peru_departament_2019-2024.csv")
