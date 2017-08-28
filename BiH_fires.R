# Script to animate fire data for BiH
# Mirza Cengic | mirzaceng@gmail.com | 27.08.17.

# Get packages
pacman::p_load(sf, raster, mapview, ggplot2, gganimate, ggmap,
               dplyr, spatstat, maptools, gganimate)

## Get fire data for Europe in the last 7 days (local file)
# Source: earthdata.nasa.gov - VIIRS 
europe_fires <- st_read("E:/Projects/Fires/bih_fires/Data/VIIRS/VNP14IMGTDL_NRT_Europe_7d.shp")
europe_fires_new <- st_read("E:/Projects/Fires/bih_fires/Data/VIIRS/VNP14IMGTDL_NRT_Europe_48h.shp")

# Coordinate system empty, put back wgs84
st_crs(europe_fires_new) <- st_crs(4326)
st_crs(europe_fires) <- st_crs(4326)

# Merge old and new data
europe_fires <- rbind(europe_fires, europe_fires_new)

# Get BiH boundary
bih_boundary <- st_as_sf(raster::getData(name = "GADM", country = "BIH", level = 0))

# Crop fires to BiH
bih_fires <- st_intersection(europe_fires, bih_boundary)

# bih_fires %>% 
#   select(LATITUDE, LONGITUDE, ACQ_DATE, BRIGHT_TI4) %>% 
#   st_write("Y:/Mirza_Cengic/Projects/Other/Fires/BiH_fires_aug17.shp")



# Plot
mapview(bih_fires, zcol = "ACQ_DATE")

## If gganimate magick doesn't work; set environment PATH
Sys.setenv(PATH = paste("C:/Program Files/ImageMagick/",
                        Sys.getenv("PATH"), sep = ";"))
#

## Clean up fire data
bih_fires_clean <- bih_fires %>% 
  select(c(ACQ_DATE, CONFIDENCE, BRIGHT_TI4)) %>% 
  transmute(Date = ACQ_DATE %>% format("%d %b"),
            x = st_coordinates(.)[, 1],
            y = st_coordinates(.)[, 2]) %>% 
  as.data.frame() %>% 
  select(-geometry)

# Get background map
bih_map <- get_map(location = c(17.70, 43.45), zoom = 9, maptype = "hybrid")

p_bihmap <- ggmap(bih_map) +
  theme(axis.text = element_blank()) + xlab("") + ylab("")

# Make single plot
p_bih_fires <-
  p_bihmap + 
    geom_point(data = bih_fires_clean, aes(x, y),
               color = "firebrick4", alpha = 0.04, size = 2) +
  geom_point(data = bih_fires_clean, aes(x, y, frame = Date),
             color = "firebrick2", size = 1.6) + 
    # borders("world", size = 2, color = "red") + 
  scale_colour_continuous(guide = FALSE) + 
  annotate("text", x = 18.448, y = 44.0675, label = "Autor: Mirza Cengic",
           colour = "white", alpha = 0.7, size = 3.1) +
    ggtitle("Aktivni požari u proteklih 10 dana\n")

p_bih_fires


# Animate
animation::ani.options(antialias = "cleartype", ani.width = 480 * 1.5, ani.height = 480 * 1.4)

gganimate(p_bih_fires, interval = 0.7, filename = "E:/Projects/Fires/bih_fires/BiH_active_fires_aug.gif")

