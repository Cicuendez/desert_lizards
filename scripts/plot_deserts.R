
setwd("~/Dropbox/HECTOR/Dan/")

# Required packages
libs <- c("rgdal", "maptools", "gridExtra", "rgeos", "raster", "devtools", "treeio",
          "tidytree", "geiger", "doParallel", "sf", "dplyr", "tidyverse", "lwgeom",
          "cartography", "ggplot2", "cowplot", "googleway", "ggrepel", "ggspatial",
          "rnaturalearth", "rnaturalearthdata", "RColorBrewer", "scico")
# libwgeom is not installed
lapply(libs, require, character.only = TRUE)

# Import NATURAL EARTH raster
NatEarth <- raster("layers/NE1_LR_LC_SR/NE1_LR_LC_SR.tif")
plot(NatEarth)

# Import LAND layer
land_layer0 <- readRDS("objects/land_layer.rds")
land_layer50 <- st_read("layers/Land/ne_50m_land/ne_50m_land.shp")


# Import DESERTS layer
world_deserts <- readRDS("objects/world_deserts.rds")



NatEarth_df <- as.data.frame(NatEarth, xy = TRUE)
summary(NatEarth_df)

NatEarth_df2 <- NatEarth_df %>%
  mutate(ne=na_if(NE1_LR_LC_SR, 252))
NatEarth_df2 <- NatEarth_df2 %>%
  mutate(ne=na_if(ne, 253))
length(which(is.na(NatEarth_df2$ne)))

scico_palette_show()
pdf("temp/00naturalEarth.pdf")
ggplot() +
  geom_raster(data=NatEarth_df2, aes(x=x, y=y)) +
#  scale_fill_scico(palette="oleron", direction=1, begin=0, end=1, alpha=1, na.value="aliceblue") +
#  labs(fill="net primary productivity") +
#  ggtitle("Net primary productivity") +
  theme_classic()
#  coord_equal()
dev.off()


# PLOT DESERT MAP
desert_colors <- c("red", "gold", "green", "purple", "brown", "lightblue", "darkblue", "orange")
desert_names <- c("Atacama", "Australian", "Central Asian", "Gobi", "Kalahari", "North American",
                  "Persian", "Saharoarab")
names(desert_colors) <- desert_names

# Alternative palette
green <- '#6A8C1F'
yellow <- '#F2DD72'
orange1 <- '#F29E38'
orange2 <- '#F25116'
brown <- '#400C07'
blue <- '#03A688'
red <- '#A62103'
purple <- '#b65cff'
blue2 <- '#7a97ff'
desert_colors <- c(green, yellow, orange1, orange2, brown, blue, red, blue2)
desert_names <- c("Persian", "Gobi", "Saharoarab", "Australian", "Kalahari", "North American", 
                  "Atacama", "Central Asian")
names(desert_colors) <- desert_names
plot(1:8, 1:8, cex= 10, pch=16, col = desert_colors)


deserts_ggplot <- ggplot() +
  geom_sf(data=land_layer50, fill="gray92", color="transparent") +
  geom_sf(data=world_deserts, aes(fill=desert), color="transparent") +
  scale_fill_manual(values=desert_colors) +
  theme_classic()
#  ggtitle("Deserts of the world")

pdf("plots/world_deserts_plot/deserts_ggplot.pdf", paper="a4r")
deserts_ggplot
dev.off()


