library(tidyverse)
library(sf)
library(units) # us survey foot to regular foot conversion
library(httr2)
library(jsonlite)


# school addresses downloaded from data.cityofrochester.gov
# school_addresses <- st_read(paste0(Sys.getenv("downloads_loc"), "Rochester_City_School_District_(RCSD)_Schools_and_Programs_2024_2025.geojson")) %>%
    # # transform to move from degrees to us survey foot
    # st_transform(2262)
parcels_2022 <- read_sf("https://maps.cityofrochester.gov/server2/rest/services/Open_Data/TaxParcel2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    st_transform(2262)
# city_boundary <- read_sf(paste0(Sys.getenv("downloads_loc"), "City_of_Rochester_Border.geojson")) %>%
city_boundary <- read_sf("https://services2.arcgis.com/yoz1ZtATTCokO9nU/arcgis/rest/services/City_of_Rochester_Border/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    st_transform(2262)

# but this would just be rcsd
# school_parcels <- parcels_2022 %>% st_filter(school_addresses, st_within)
# want private schools too

schools <- parcels_2022 %>% filter(CLASSDSCRP %in% c("School", "Special Schools"))

# how big a buffer?
# nysdot says 1320 feet: https://www.dot.ny.gov/about-nysdot/faq/posting-speed-limit-within-a-school-zone

zone_distance <- 1320
units(zone_distance) <- as_units("feet")
zone_distance <- set_units(zone_distance, "us_survey_foot")

school_zones <- st_buffer(schools, zone_distance)
all_zones <- st_union(school_zones) %>%
  # don't count area outside the city
  st_intersection(city_boundary)

covered <- st_area(all_zones)
units(covered) <- as_units("US_survey_foot^2")
covered <- set_units(covered, "miles^2")
# what % of city is covered?
covered / (st_area(city_boundary) %>% set_units("US_survey_foot^2") %>% set_units("miles^2"))

p <- schools %>%
    ggplot() +
    geom_sf() +
    # geom_sf(data = school_zones, fill = NA, color = 'red') +
    geom_sf(data = all_zones, fill = NA, color = 'red') +
    geom_sf(data = city_boundary, fill = NA, color = 'black') +
    theme_bw()

# ggsave(p, filename = "school_zones.png")

# get streets

bbox_rochester <- "-77.68,43.10,-77.52,43.3"

# Construct query URL for GeoJSON format
query_url_feature <- paste0(
  "https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/arcgis/rest/services/NYS_Streets/FeatureServer/0/query?",
  "where=1%3D1&",
  "geometry=", bbox_rochester, "&",
  "geometryType=esriGeometryEnvelope&",
  "spatialRel=esriSpatialRelIntersects&",
  "inSR=4326&",
  "outSR=4326&",
  "outFields=*&",
  "f=geojson"
)

rochester_streets <- st_read(query_url_feature) %>%
  st_transform(2262) %>%
  filter(LeftCityTownName == "Rochester" | RightCityTownName == "Rochester") %>%
  mutate(
    street_name = coalesce(Label, CompleteStreetName, StreetName),
    street_name_norm = toupper(str_squish(str_replace_all(street_name, "\\s+", " ")))
  )

# arterial classification code table on page 16 of data dictionary
# https://gis.ny.gov/system/files/documents/2023/07/streets_data_dictionary.pdf

arterials <- rochester_streets %>% 
  filter(ACC %in% 1:4 & ! FCC %in% c('A15', 'A17')) %>%
  filter( ! grepl(x = street_name, pattern = "Ramp|Service Road")) %>%
  filter( ! grepl(x = street_name, pattern = "390|490|590"))

# length of arterials within school zones

art_l_z <- arterials %>% st_intersection(all_zones) %>% st_length() %>% sum()
art_l <- arterials %>% st_length() %>% sum()
art_l_z / art_l


p2 <- ggplot() +
  geom_sf(data = city_boundary, fill = NA, color = 'black') +
  # arterials
  geom_sf(data = arterials, fill = NA, color = 'black', size = 1) +
  geom_sf(data = all_zones, fill = 'red', alpha = .5) +
  #geom_sf(data = ped_hij_streets, color = 'blue', size = 1) +
  #geom_sf(data = bike_hij_streets, color = 'green', size = 1) +
  # geom_sf(data = hij, color = 'red', size = 1) +
  theme_bw()

ggsave(p2, filename = "arterials.png")


city_bbox <- city_boundary %>% st_bbox()
city_mid <- mean(c(city_bbox["xmin"], city_bbox["xmax"]))
pad <- 5000

hij <- st_read("Rochester_ATP_Top10_Ped_Bike_Crash_Corridors.geojson") %>%
    st_transform(2262) %>%
    mutate(
        # get xys for labels
        xys = st_geometry(.) %>% st_point_on_surface() %>% st_coordinates(),
        x = xys[,1],
        y = xys[,2],
        side = if_else(x < city_mid, "left", "right"),
        target = if_else(side == "left", city_bbox["xmin"] - pad, city_bbox["xmax"] + pad),
        hjust_val = if_else(side == "left", 1, 0),
        nudge_x_val = target - x)

ped <- hij %>% filter(mode == "Pedestrian")
bike <- hij %>% filter(mode == "Bicycle")

ped_prop <- (ped %>% st_intersection(all_zones) %>% st_length() %>% sum()) / (ped %>% st_length() %>% sum())
bike_prop <- (bike %>% st_intersection(all_zones) %>% st_length() %>% sum()) / (bike %>% st_length() %>% sum())

p_ped <- ped %>%
    ggplot(data = ) +
    geom_sf(data = city_boundary, fill = NA, color = 'black') +
    # arterials
    geom_sf(data = arterials, fill = NA, color = 'grey', size = 1) +
    geom_sf(data = all_zones, fill = 'grey', alpha = .5) +
    geom_sf(data = ped, color = 'red', size = 1) +
    geom_label_repel(
        data = ped, aes(x = x, y = y, label = street, hjust = hjust_val),
        direction = "y", nudge_x = ped$nudge_x_val,
        label.padding = .1, alpha = .75, segment.size = 1, segment.color = "black",
        min.segment.length = 0, box.padding = .5, force = 10,
        fill = "black", color = "white"
        ) +
    theme_void() +
    # theme(axis.title = element_blank()) +
    ggtitle("Pedestrian High Injury Network + 1/4 mile schools buffer")

ggsave(p_ped, filename = "ped_high_inj_network.png", bg = "white")

(p_ped + 
    coord_sf(xlim = st_bbox(ped)[c("xmin", "xmax")], ylim = st_bbox(ped)[c("ymin", "ymax")]) +
    ggtitle("Pedestrian High Injury Network + 1/4 mile schools buffer", subtitle = "zoomed in")
    ) %>% 
    ggsave(., filename = "ped_high_inj_network_zoom.png", bg = "white")

p_bike <- ggplot() +
    geom_sf(data = city_boundary, fill = NA, color = 'black') +
    # arterials
    # geom_sf(data = arterials, fill = NA, color = 'black', size = 1) +
    geom_sf(data = rochester_streets, fill = NA, col = 'black', alpha = .1) +
    geom_sf(data = all_zones, fill = 'grey', alpha = .5) +
    geom_sf(data = bike, color = 'red', size = 1) +
    geom_label_repel(
        data = bike, aes(x = x, y = y, label = street, hjust = hjust_val),
        direction = "y", 
        nudge_x = bike$nudge_x_val, 
        label.padding = .1, alpha = .75, segment.size = 1, segment.color = "black",
        min.segment.length = 0, box.padding = .5, force = 10,
        fill = "black", color = "white"
        ) +
    theme_void() +
    # zoom in:
    ggtitle("Cyclist High Injury Network + 1/4 mile schools buffer")

ggsave(p_bike, filename = "bike_high_inj_network.png", bg = "white")

(p_bike + 
    coord_sf(
        xlim = st_bbox(bike)[c("xmin", "xmax")], 
        ylim = st_bbox(bike)[c("ymin", "ymax")]) +
    ggtitle(
        "Cyclist High Injury Network + 1/4 mile schools buffer", 
        subtitle = "zoomed in")
    ) %>%
    ggsave(filename = "bike_high_inj_network_zoom.png", bg = "white")