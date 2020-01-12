library(geobr)
library(brazilmaps)
library(ggplot2)

load_maps <- function() {
  # geobr
  mapa_municipios <<- read_municipality(code_muni = "all", year = 2018)
  
  # roots
  shpMun <<- rgdal::readOGR('./Shapes/br_municipios', 'BRMUE250GC_SIR', encoding = "UTF-8")
  mapaMun <<- fortify(shpMun, region = 'CD_GEOCMU')
  shpUFs <<- rgdal::readOGR('./Shapes/br_unidades_da_federacao', 'BRUFE250GC_SIR')
  
}


# geobr
plot_mun_brasil <- function (dados, fill_var, mp) {
  
  mun <- mapa_municipios %>% left_join(dados, by= c("code_muni" = "Cod_Municipio"))
  ggplot(mun) + 
    geom_sf(aes(fill=get(fill_var)),  size = 0.005) + 
    scale_size_identity() + 
    scale_fill_gradient(
      low = 'lightyellow',
      high = 'darkred'
    ) 
}



# roots
plot_mun_brasil_alt <- function (dados, fill_var, descr = "") {
  ggplot(dados) +
    geom_map(
      map = mapaMun,
      color = NA,
      aes(map_id = Cod_Municipio, fill = get(fill_var))
    ) +
    labs(
      fill = descr
    ) + 
    expand_limits(
      x = mapaMun$long,
      y = mapaMun$lat
    ) +
    scale_fill_gradient(
      low = 'lightyellow',
      high = 'darkred'
    ) +
    geom_path(
      data = shpUFs,
      size = .2,
      aes(long, lat, group = group)
    ) +
    coord_map() +
    theme_void() 
    
}




# brazil_maps
plot_mun_brasil_brazilmaps <- function (dados, fill_var, mp) {
  
  get_brmap("City") %>% 
    left_join(dados, c("City" = "Cod_Municipio")) %>% 
    ggplot() +
    geom_sf(aes(fill = get(fill_var)), colour = "transparent") +
    scale_fill_gradient(
      low = 'lightyellow',
      high = 'darkred'
    )  +
    geom_path(
      data = shpUFs,
      size = .2,
      aes(long, lat, group = group)
    ) 
}


