library(geobr)
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
plot_mun_brasil_alt <- function (dados, fill_var) {
  ggplot(dados) +
    geom_map(
      map = mapaMun,
      color = 'gray60', size = 0,
      aes(map_id = Cod_Municipio, fill = get(fill_var))
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
    theme_void() +
    theme(legend.position = c(0.2,0.3))
    
}

