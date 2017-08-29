# Extract burnt landcover
pacman::p_load(raster, sf, stringr, ggplot2, broom, rgeos, maptools)

# bih_boundary <- raster::getData(name = "GADM", country = "BIH", level = 0)

bih_lc <- raster("Y:/Mirza_Cengic/Projects/Other/Fires/BiH_land_cover_2015.tif")

bih_fires <- shapefile("Y:/Mirza_Cengic/Projects/Other/Fires/BiH_fires_aug17.shp")

bih_lc_area <- raster::area(bih_lc)
bih_lc_area_df <- as.data.frame(values(bih_lc_area), col.names = "Area")


bih_fires_unique <- remove.duplicates(bih_fires)
# Extract full data per day
# bih_fires_landcover <- extract(bih_lc, bih_fires, sp = TRUE)
# Extract for unique points
bih_fires_landcover <- extract(stack(bih_lc, bih_lc_area), bih_fires_unique, sp = TRUE)
bih_fires_landcover_merged <- sp::merge(bih_fires_landcover, fires_values, by.x = "BiH_land_cover_2015", by.y = "Value")

bih_fires_landcover_merged@data$Short <- as.character(bih_fires_landcover_merged@data$Short)

bih_fires_landcover_df <- bih_fires_landcover_merged@data

head(bih_fires_landcover_df)
# fires_date <- bih_fires_landcover_df$ACQ_DATE
# fires_date <- fires_date %>% 
#   str_replace("2017/08/", "")

names(fires_landcover) <- c("Value", "Area")

fires_values <- read.csv("Y:/Mirza_Cengic/Projects/Other/Fires/BiH_LC_values_table.csv")
# fires_values_df <- merge(fires_landcover, fires_values, by = "Value")
head(fires_values_df)
#
identical(fires_values_df$Value, bih_fires_landcover@data$BiH_land_cover_2015)

fires_frequency <- bih_fires_landcover_df %>% 
  select(Short) %>% 
  table() %>% 
  tidy()

# Not sure how to pass . in order to rename variable with pipe (. passes the piped object as first argument)
names(fires_frequency) <- c("Landcover", "Frequency")

fires_frequency <- fires_frequency[order(fires_frequency$Frequency), ]


fires_frequency$Landcover <- factor(fires_frequency$Landcover, levels = fires_frequency$Landcover[order(-fires_frequency$Frequency)])


ggplot(fires_frequency, aes(x = Landcover, y = Frequency)) + geom_bar(stat = "identity")

#####
## Get fire polygon for plotting
# Create raster from SpatialPointsDataFrame
fire_raster <- raster()
extent(fire_raster) <- extent(bih_fires_landcover_merged) 

# Get raster values from burnt points
fire_raster_values <- rasterize(bih_fires_landcover_merged, fire_raster, bih_fires_landcover_merged$BiH_land_cover_2015, fun=mean) 

# Convert burnt location raster to polygon
fire_polygon <- rasterToPolygons(fire_raster_values)

# Dissolve polygon
region <- gUnaryUnion(fire_polygon)

mapview(region, alpha.regions = 0.1, col.regions = "grey80")
#####


table(fires_values_df$Landcover)
ggplot(fires_values_df, aes())

sort(unique(fires_landcover))

# Get total burnt area

fires_values_df %>% 
  as.data.frame() %>%
  transmute(Landcover = Landcover,
            Area = Area,
            Short = Short) %>%  
  group_by(Short) %>% 
  summarise(Frequency = sum(Area)) %>% 
  arrange(desc(Frequency)) %>% 
  summarise(Total = sum(Frequency))

# Get burnt forest area

fires_values_df %>% 
  as.data.frame() %>%
  transmute(Landcover = Landcover,
            Area = Area,
            Short = Short %>% as.character()) %>%  
  group_by(Short) %>% 
  summarise(Frequency = sum(Area)) %>% 
  arrange(desc(Frequency)) %>% 
  filter(str_detect(Short, "Forest")) %>% 
  summarise(Total = sum(Frequency))

# Burnt forest area is 63 square kilometers, about the size of San Marino

hist(bih_fires_landcover$ACQ_DATE, bih_fires_landcover$BiH_land_cover_2015)
