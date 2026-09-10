
# MORAN'S I --------------------------------------------------------------------
# ------------------------------------------------------------------------------
# https://mgimond.github.io/simple_moransI_example/

# DATA LOADING ------------------------------------------------------------------
# forest attributes
ff_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_attri_sf.csv") %>% 
  st_as_sf(wkt = "geometry", crs = 6372) %>% dplyr::select(-X) %>% 
  mutate(forest_cover = as.numeric(forest_cover),
         forest_late_ss = as.numeric(forest_late_ss),
         forest_connectivity = as.numeric(forest_connectivity))
ff_df <- read_sf("MSc-Thesis/Analysis/data/forest/df/11_forest_attri_sf.csv")%>% 
  st_as_sf(wkt = "geometry", crs = 6372) %>% dplyr::select(-field_1) %>% 
  mutate(forest_cover = as.numeric(forest_cover),
         forest_late_ss = as.numeric(forest_late_ss),
         forest_connectivity = as.numeric(forest_connectivity))

ff_all <- rbind(ff_df, ff_wf) %>% 
  filter(!(ID == "MX_DR_FC6")) 


geom <- ff_all %>% dplyr::select(ID, geometry)
ff_all <- left_join(geom, all_attri, by = "ID") 

ff_all_wet <- ff_all %>% filter(forest_type == "wet")

coords <- st_coordinates(ff_all)
knea <- knearneigh(coords, k = 4)
Coord <- knn2nb(knea)
lw <- nb2listw(Coord, style = "W")
lm.morantest(lm1, lw)

