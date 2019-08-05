library(brazilmaps)

library(dplyr)
library(ggplot2)


plot_map_mun <- function (dados, fill_var) {
  muni_map <- get_brmap("City") %>% 
    left_join(dados, c("City" = "Cod_Municipio"))
  
  uf_map <- get_brmap("State", class = "SpatialPolygonsDataFrame")
  
  muni_map %>% 
    ggplot() +
    geom_sf(aes(fill = get(fill_var)), colour = NA) +
    geom_path(
      data = uf_map,
      size = .005,
      aes(long, lat, group = group)
    ) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
    
}




