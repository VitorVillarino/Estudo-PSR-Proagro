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

###################### Teste ############################

library(cluster)
library(factoextra)
library(fpc)
library(NbClust)
library(tidyverse)
library(Hmisc)
library(ClusterR)
library(FactoMineR)
library(stringr)  
library(hrbrthemes)
library(viridis)


premio_area_tidy <-  
  Proagro_soja_mun_geral_safra  %>% 
  ungroup() %>%   
  select(Cod_Municipio,SAFRA, num_seguros, Premio_Area_Defl) %>% 
  rename(VL_PROAGRO = Premio_Area_Defl) %>% 
  full_join(   
    PSR_soja_mun_safra %>% 
      ungroup() %>% 
      select(Cod_Municipio, num_seguros, SAFRA, VL_PREMIO_PAGO_Area_Defl) %>% 
      rename(VL_PSR = VL_PREMIO_PAGO_Area_Defl),
    by = c("Cod_Municipio", "SAFRA")
  )

premio_area_tidy <- premio_area_tidy[!is.na(premio_area_tidy$VL_PROAGRO),]
premio_area_tidy <- premio_area_tidy[!is.na(premio_area_tidy$VL_PSR),]

premio_area_tidy <- premio_area_tidy[premio_area_tidy$num_seguros.x > 0,]
premio_area_tidy <- premio_area_tidy[premio_area_tidy$num_seguros.y > 0,]


premio_area_tidy$dif <- (premio_area_tidy$VL_PROAGRO - premio_area_tidy$VL_PSR)/premio_area_tidy$VL_PSR
premio_area_tidy$dif_abs <- (premio_area_tidy$VL_PROAGRO - premio_area_tidy$VL_PSR)
summarytools::descr(premio_area_tidy$VL_PROAGRO)
summarytools::descr(premio_area_tidy$VL_PSR)


teste_na  <- premio_area_tidy %>% select(Cod_Municipio,SAFRA,dif) %>% spread(SAFRA,dif) %>% 
  group_by(Cod_Municipio) %>% 
  summarise(
    num_na = is.na(`2013/2014`) +
      is.na(`2014/2015`) +
      is.na(`2015/2016`) +
      is.na(`2016/2017`) +
      is.na(`2017/2018`) +
      is.na(`2018/2019`)
  )

teste_na <- teste_na[teste_na$num_na < 3,]

premio_area_tidy <- premio_area_tidy[premio_area_tidy$Cod_Municipio %in% teste_na$Cod_Municipio,]
 
val_max_intervalo <- 0.20
max_min_premio_area_tidy <- premio_area_tidy %>% 
                            group_by(Cod_Municipio) %>% 
                            summarize(
                              range = max(dif) - min(dif),
                              range_abs = max(dif_abs) - min(dif_abs)
                            )
  
max_min_premio_area_tidy <- max_min_premio_area_tidy[max_min_premio_area_tidy$range <= val_max_intervalo,]
premio_area_tidy <- premio_area_tidy[premio_area_tidy$Cod_Municipio %in% max_min_premio_area_tidy$Cod_Municipio,]



premio_area_tidy$SAFRA_num <-  as.numeric(substr(premio_area_tidy$SAFRA,0,4))
premio_area_tidy$Cod_factor <- as.factor(premio_area_tidy$Cod_Municipio)

premio_area_spread <- premio_area_tidy %>% select(Cod_Municipio,SAFRA,dif) %>% spread(SAFRA,dif)


premio_area_spread$media <- apply(premio_area_spread[,2:7],1, FUN = mean, na.rm = T)
esquisse::esquisser()

# 
# teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$Cod_Municipio != 4102703,]
# openxlsx::write.xlsx(teste_comparacao_premio_area, "teste_comparacao.xlsx")

val_meio <- 0.1
val_maior <- 0.30
val_menor <- -0.30

teste_comparacao_premio_area_meio <- premio_area_spread[abs(premio_area_spread$media) <= val_meio,]
teste_comparacao_premio_area_maior <- premio_area_spread[premio_area_spread$media > val_maior,]
teste_comparacao_premio_area_menor <- premio_area_spread[premio_area_spread$media < val_menor,]


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
              union_all(dados_comparacao_municipio_menor) 
#%>%   union_all(dados_comparacao_todos_municipios_soja)


j <- 0
for(i in dados_union) {
  j <- j + 1
  
  print(str_wrap(names(dados_union)[j],50))
  
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
      xlab("") + ylab(str_wrap(names(dados_union)[j],60))
  )
  
  ggsave(paste("./jpg/", j,".jpg"),width = 12, height = 7)
}



###################### Cluster ############################

library(cluster)
library(factoextra)
library(fpc)
library(NbClust)
library(tidyverse)
library(Hmisc)
library(ClusterR)
library(FactoMineR)



teste_comparacao_premio_area <-  
  Proagro_soja_mun_geral_safra  %>% 
  ungroup() %>%   
  select(Cod_Municipio,SAFRA, num_seguros, Premio_Area_Defl) %>% 
  rename(VL_PROAGRO = Premio_Area_Defl) %>% 
  full_join(   
    PSR_soja_mun_safra %>% 
      ungroup() %>% 
      select(Cod_Municipio, num_seguros, SAFRA, VL_PREMIO_PAGO_Area_Defl) %>% 
      rename(VL_PSR = VL_PREMIO_PAGO_Area_Defl),
    by = c("Cod_Municipio", "SAFRA")
  )



teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PROAGRO),]
teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PSR),]

teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$num_seguros.x > 0]
teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$num_seguros.y > 0,]

teste_comparacao_premio_area$dif <- (teste_comparacao_premio_area$VL_PROAGRO - teste_comparacao_premio_area$VL_PSR)/teste_comparacao_premio_area$VL_PSR
teste_comparacao_premio_area$dif_abs <- (teste_comparacao_premio_area$VL_PROAGRO - teste_comparacao_premio_area$VL_PSR)
summarytools::descr(teste_comparacao_premio_area$VL_PROAGRO)
summarytools::descr(teste_comparacao_premio_area$VL_PSR)




teste_comparacao_premio_area <- teste_comparacao_premio_area %>% select(Cod_Municipio,SAFRA,dif) %>% spread(SAFRA,dif)

teste_comparacao_premio_area <-
  teste_comparacao_premio_area[
    !is.na(teste_comparacao_premio_area$`2013/2014`) &
    !is.na(teste_comparacao_premio_area$`2014/2015`) &
    !is.na(teste_comparacao_premio_area$`2015/2016`) &
    !is.na(teste_comparacao_premio_area$`2016/2017`) &
    !is.na(teste_comparacao_premio_area$`2017/2018`) &
    !is.na(teste_comparacao_premio_area$`2018/2019`)
  ,]


val_max_intervalo <- 0.20

teste_comparacao_premio_area <-
  teste_comparacao_premio_area[
    abs(teste_comparacao_premio_area$`2013/2014` - teste_comparacao_premio_area$`2014/2015`) <= val_max_intervalo &
      abs(teste_comparacao_premio_area$`2014/2015` - teste_comparacao_premio_area$`2015/2016`) <= val_max_intervalo &
      abs(teste_comparacao_premio_area$`2015/2016` - teste_comparacao_premio_area$`2016/2017`) <= val_max_intervalo &
      abs(teste_comparacao_premio_area$`2016/2017` - teste_comparacao_premio_area$`2017/2018`) <= val_max_intervalo &
      abs(teste_comparacao_premio_area$`2017/2018` - teste_comparacao_premio_area$`2018/2019`) <= val_max_intervalo 
    ,]



teste_comparacao_premio_area$media <- apply(teste_comparacao_premio_area[,2:7],1, FUN = mean, na.rm = T)
# teste_comparacao_premio_area$SAFRA_num <-  as.numeric(substr(teste_comparacao_premio_area$SAFRA,0,4))
# teste_comparacao_premio_area$Cod_factor <- as.factor(teste_comparacao_premio_area$Cod_Municipio)
# esquisse::esquisser()

plot_mun_brasil_brazilmaps(teste_comparacao_premio_area,"media", "media")


test_drivers <- teste_comparacao_premio_area[ ,-c(1)]
test_drivers$media <- NULL





# #removendo colunas que contenham algum NA.
# cok <- apply(test_drivers,2,function(x)!any(is.na(x)))
# test_drivers <- test_drivers[,cok]


fviz_nbclust(test_drivers, clara, method = "silhouette",correct.d=FALSE)+
  theme_classic()

# Compute CLARA
clara.res <- clara(test_drivers, 2, stand = T, samples = 5000, pamLike = TRUE, correct.d = F)
# Print components of clara.res
print(clara.res)

fviz_cluster(clara.res,
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             ,choose.vars = c("2013/2014","2014/2015")
)

fviz_cluster(clara.res,
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             ,choose.vars = c("2014/2015","2015/2016")
)


fviz_cluster(clara.res,
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             ,choose.vars = c("2015/2016","2016/2017")
)

fviz_cluster(clara.res,
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
             ,choose.vars = c("2016/2017","2017/2018")
)


