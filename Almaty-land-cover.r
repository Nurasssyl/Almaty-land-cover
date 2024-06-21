pacman::p_load(
  rstac,
  sf,
  terra,
  arcgislayers,
  tidyverse,
  elevatr,
  tidyterra
)

url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"

city_data <- arcgislayers::arc_open(
  url
)

city_sf <-
  arcgislayers::arc_select(
    city_data,
    fields = "NAME",
    where = "NAME = 'Alma-ata'",
    crs = 4326
  )

plot(
  sf::st_geometry(
    city_sf
  )
)

city_bbox <- sf::st_bbox(city_sf)

main_dir <- getwd()

ms_query <-
  rstac::stac(
    "https://planetarycomputer.microsoft.com/api/stac/v1"
  )

ms_collections <-
  ms_query |>
  rstac::collections() |>
  rstac::get_request()

print(ms_collections, n = 123)

collections <- "esa-worldcover"

ms_esa_query <-
  rstac::stac_search(
    q = ms_query,
    collections = collections,
    datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
    bbox = city_bbox,
    limit = 100
  ) |>
  rstac::get_request()

ms_query_signin <-
  rstac::items_sign(
    ms_esa_query,
    rstac::sign_planetary_computer()
  )

ms_query_signin

rstac::assets_download(
  items = ms_query_signin,
  asset_names = "map",
  output_dir = main_dir,
  overwrite = TRUE
)

version <- "v200"
year <- "2021"
asset_name <- "map"

data_dir <- paste0(
  main_dir, "/",
  collections, "/",
  version, "/",
  year, "/",
  asset_name
)

raster_file <- list.files(
  data_dir,
  full.names = TRUE
)

land_cover_raster <- terra::rast(
  raster_file
)

city_land_cover <-
  terra::crop(
    land_cover_raster,
    terra::vect(city_sf),
    snap = "in"
  )

terra::plot(city_land_cover)

dem <- elevatr::get_elev_raster(
  locations = city_sf,
  z = 11,
  clip = "bbox"
) |>
  terra::rast()

city_land_cover_resampled <- terra::resample(
  x = city_land_cover,
  y = dem,
  method = "near"
)

terra::plot(city_land_cover_resampled)

actual_values <- sort(unique(terra::values(city_land_cover_resampled)))

values <- seq(
  from = 10,
  to = 100,
  by = 10
)

values <- append(
  values,
  95,
  after = 9
)

labels <- c(
  "Деревья",
  "Кустарник",
  "Пастбище",
  "Пахотные земли",
  "Застройки",
  "Малая растительность",
  "Снег и лед",
  "водные объекты",
  "Водно-болотные угодья",
  "Мангровые заросли",
  "Мхи и лишайники"
)

codebook <- data.frame(cbind(values, labels))
actual_labels <- subset(codebook, values %in% actual_values)


map <- ggplot() +
  tidyterra::geom_spatraster(
    data = as.factor(city_land_cover_resampled),
    use_coltab = TRUE,
    maxcell = Inf
  ) +
  tidyterra::scale_fill_coltab(
    data = as.factor(city_land_cover_resampled),
    name = "Легенды:",
    labels = actual_labels$labels
  ) +
  geom_sf(
    data = city_sf,
    fill = "transparent",
    color = "white",
    linewidth = .5
  ) +
  theme_void()



map <- map +
  labs(title = "Анализ земельного покрова Алматы")

ggsave("C:/Users/User/Desktop/R/cover-city/Almaty-land-cover-test.jpeg", plot = map, width = 10, height = 8, units = "in", dpi = 300)


