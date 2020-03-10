## -----------------------------------------------------------------------------------
library(sf)
belgium_shape_sf <- st_read('./shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))


## -----------------------------------------------------------------------------------
class(belgium_shape_sf)
belgium_shape_sf %>% as_tibble() %>% slice(1:3) 

## -----------------------------------------------------------------------------------
ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

## -----------------------------------------------------------------------------------
library(tmap)
# qtm(belgium_shape_sf) # does not work
# shapefile slightly corrupted!

# slightly smooth the shapefile
simple_shp <- st_simplify(belgium_shape_sf, 
                          dTolerance = 0.00001)

# and plot
qtm(simple_shp)

## -----------------------------------------------------------------------------------
tm_shape(simple_shp) +
  tm_borders(col = KULbg, lwd = 0.5) +
  tm_layout(main.title = 'Welcome to Belgium!', legend.outside = TRUE, frame = FALSE) 


## -----------------------------------------------------------------------------------
post_expo <- mtpl %>% group_by(pc) %>% summarize(num = n(), total_expo = sum(expo)) 

post_expo %>% slice(1:5) 

## -----------------------------------------------------------------------------------
## Your Turn!-------------------------------------------------------------------------

belgium_shape_sf <- left_join(___, 
                              ___, 
                              by = c("___" = "___"))


## -----------------------------------------------------------------------------------
belgium_shape_sf$freq <- 
  belgium_shape_sf$___/belgium_shape_sf$___


## -----------------------------------------------------------------------------------
belgium_shape_sf$freq_class <- cut(___, 
                                   breaks = quantile(___, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, 
                                   labels = c("low", "average", "high"))

ggplot(___) +
  geom_sf(aes(fill = ___), colour = "black", size = 0.1) +
  ggtitle("MTPL claim frequency data") + labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()



## ----Your Turn ends here------------------------------------------------------------
## -----------------------------------------------------------------------------------

library(tmap)

# slightly smooth the shapefile
belgium_shape_sf <- st_simplify(belgium_shape_sf, 
                                dTolerance = 0.00001)

# and plot
tm_shape(belgium_shape_sf) + 
  tm_borders(col = "black") + 
  tm_fill(col = "freq_class", style = "cont", palette = "Blues", colorNA = "white")

## -----------------------------------------------------------------------------------
tmap_leaflet(last_map())





