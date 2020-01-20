library(tidyverse)
library(hrbrthemes)
library(viridis)


fun_reset <- function() {
  rm(list = ls(all.names = TRUE)) 
  source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
  source("./Scripts/02_Transformacoes_Necessarias.R", encoding = "UTF-8")
  source("./Scripts/Function_Mapas.R", encoding = "UTF-8")
  load_maps()
}

teste_comparacao_premio_area <-  
  Proagro_soja_mun_geral_safra  %>% 
  ungroup() %>%   
  select(Cod_Municipio,SAFRA, num_seguros, VL_TOTAL_Medio_Area_Defl) %>% 
  rename(VL_PROAGRO = VL_TOTAL_Medio_Area_Defl) %>% 
full_join(   
  PSR_soja_mun_safra %>% 
  ungroup() %>% 
  select(Cod_Municipio, num_seguros, SAFRA, VL_LIMITE_GARANTIA_Area_Defl) %>% 
  rename(VL_PSR = VL_LIMITE_GARANTIA_Area_Defl),
  by = c("Cod_Municipio", "SAFRA")
)


teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PROAGRO),]
teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PSR),]

teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$num_seguros.x > qtd_min_proagro,]
teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$num_seguros.y > qtd_min_PSR,]

teste_comparacao_premio_area$dif <- (teste_comparacao_premio_area$VL_PROAGRO - teste_comparacao_premio_area$VL_PSR)/teste_comparacao_premio_area$VL_PSR
teste_comparacao_premio_area$dif_abs <- (teste_comparacao_premio_area$VL_PROAGRO - teste_comparacao_premio_area$VL_PSR)
summarytools::descr(teste_comparacao_premio_area$VL_PROAGRO)
summarytools::descr(teste_comparacao_premio_area$VL_PSR)

teste_comparacao_premio_area <- teste_comparacao_premio_area %>% select(Cod_Municipio,SAFRA,dif) %>% spread(SAFRA,dif)

# teste_comparacao_premio_area <- 
#   teste_comparacao_premio_area[
#     !is.na(teste_comparacao_premio_area$`2013/2014`) &
#     !is.na(teste_comparacao_premio_area$`2014/2015`) &
#     !is.na(teste_comparacao_premio_area$`2015/2016`) &
#     !is.na(teste_comparacao_premio_area$`2016/2017`) &
#     !is.na(teste_comparacao_premio_area$`2017/2018`) &
#     !is.na(teste_comparacao_premio_area$`2018/2019`) 
#   ,]

teste_comparacao_premio_area$media <- apply(teste_comparacao_premio_area[,2:7],1, FUN = mean, na.rm = T)
# 
# teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$Cod_Municipio != 4102703,]
# openxlsx::write.xlsx(teste_comparacao_premio_area, "teste_comparacao.xlsx")

val_meio <- 0.1
val_maior <- 0.25
val_menor <- -0.25

teste_comparacao_premio_area_meio <- teste_comparacao_premio_area[abs(teste_comparacao_premio_area$media) <= val_meio,]
teste_comparacao_premio_area_maior <- teste_comparacao_premio_area[teste_comparacao_premio_area$media > val_maior,]
teste_comparacao_premio_area_menor <- teste_comparacao_premio_area[teste_comparacao_premio_area$media < val_menor,]


#esquisse::esquisser()
#plot_mun_brasil_brazilmaps(teste_comparacao_premio_area, "media")  



dados_comparacao_municipio_meio <- teste_comparacao_premio_area_meio %>% 
  left_join(data_Censo_Geral, by = c("Cod_Municipio" = "Cód.")) %>% 
  left_join(data_Censo_Rural %>% filter (Produto_Padronizado == "SOJA"), by = c("Cod_Municipio" = "Cód."))
dados_comparacao_municipio_meio$tipo <- "PSR ~ Proagro"

dados_comparacao_municipio_maior <- teste_comparacao_premio_area_maior %>% 
  left_join(data_Censo_Geral, by = c("Cod_Municipio" = "Cód.")) %>% 
  left_join(data_Censo_Rural %>% filter (Produto_Padronizado == "SOJA"), by = c("Cod_Municipio" = "Cód."))
dados_comparacao_municipio_maior$tipo <- "Proagro mais caro"


dados_comparacao_municipio_menor <- teste_comparacao_premio_area_menor %>% 
  left_join(data_Censo_Geral, by = c("Cod_Municipio" = "Cód.")) %>% 
  left_join(data_Censo_Rural %>% filter (Produto_Padronizado == "SOJA"), by = c("Cod_Municipio" = "Cód."))
dados_comparacao_municipio_menor$tipo <- "Proagro mais barato"



dados_comparacao_todos_municipios_soja <- data_Censo_Geral %>% 
      filter (Cód. %in% Proagro_soja_mun_geral_safra$Cod_Municipio | Cód. %in% PSR_soja_mun_safra$Cod_Municipio) %>% 
      left_join(data_Censo_Rural %>% filter (Produto_Padronizado == "SOJA"), by = c("Cód.")) %>% 
      rename("Cod_Municipio" = "Cód.")
dados_comparacao_todos_municipios_soja$tipo <- "Todos Municipios Produtores de SOJA"


dados_union <- dados_comparacao_municipio_meio %>% 
          union_all(dados_comparacao_municipio_maior) %>% 
          union_all(dados_comparacao_municipio_menor) %>% 
          union_all(dados_comparacao_todos_municipios_soja)


j <- 0
for(i in dados_union) {
  j <- j + 1

  print(
    dados_union %>%
    ggplot( aes(x=tipo, y=get(names(dados_union)[j]), fill = tipo)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("A boxplot with jitter") +
      xlab("") + ylab(names(dados_union)[j])
  )

  ggsave(paste("./jpg/", j,".jpg"))
}









 
# ###################### Cluster ############################
# 
# library(cluster)
# library(factoextra)
# library(fpc)
# library(NbClust)
# 
# library(Hmisc)
# library(ClusterR)
# library(FactoMineR)
# 
# 
# teste_comparacao_premio_area$media <- NULL
# test_drivers <- teste_comparacao_premio_area %>% left_join(data_Censo_Geral, by = c("Cod_Municipio" = "Cód.")) 
# #test_drivers <- test_drivers [ ,-c(1)]
# # 
# # #removendo colunas que contenham algum NA.
# # cok <- apply(test_drivers,2,function(x)!any(is.na(x)))
# # test_drivers <- test_drivers[,cok]  
# 
# 
# fviz_nbclust(teste_comparacao_premio_area[,-c(1)], clara, method = "silhouette")+
#   theme_classic()
# 
# # Compute CLARA
# clara.res <- clara(teste_comparacao_premio_area[,-c(1)], 4, stand = T, samples = 5000, pamLike = TRUE)
# # Print components of clara.res
# print(clara.res)
# 
# fviz_cluster(clara.res,
#              ellipse.type = "t", # Concentration ellipse
#              geom = "point", pointsize = 1,
#              ggtheme = theme_classic()
#              ,choose.vars = c("2013/2014","2014/2015")
# )
# 
# fviz_cluster(clara.res,
#              ellipse.type = "t", # Concentration ellipse
#              geom = "point", pointsize = 1,
#              ggtheme = theme_classic()
#              ,choose.vars = c("2014/2015","2015/2016")
# )
# 
# 
# fviz_cluster(clara.res,
#              ellipse.type = "t", # Concentration ellipse
#              geom = "point", pointsize = 1,
#              ggtheme = theme_classic()
#              ,choose.vars = c("2015/2016","2016/2017")
# )
# 
# fviz_cluster(clara.res,
#              ellipse.type = "t", # Concentration ellipse
#              geom = "point", pointsize = 1,
#              ggtheme = theme_classic()
#              ,choose.vars = c("2016/2017","2017/2018")
# )
# 
# 
# 
# 
# 
# fviz_nbclust(test_drivers, clara, method = "silhouette")+
#   theme_classic()
# 
# # Compute CLARA
# clara.res <- clara(test_drivers[,-c(1)], 2, stand = T, samples = 5000, pamLike = TRUE)
# # Print components of clara.res
# print(clara.res)
# 
# test_drivers$cluster <- 
# 
# 
# fviz_cluster(clara.res,
#              ellipse.type = "t", # Concentration ellipse
#              geom = "point", pointsize = 1,
#              ggtheme = theme_classic()
# )
# 

