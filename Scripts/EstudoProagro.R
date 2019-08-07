source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
source("./Scripts/02_Transformação.R", encoding = "UTF-8")
library(ggplot2)
library(dplyr)
library(summarytools)

# library(cluster)
# library(factoextra)
# library(fpc)
# library(NbClust)
# library(esquisse)
# library(forcats)

# library(lubridate)




#Histograma
ggplot(PSR_soja_mun) +
  aes(x = num_seguros) +
  geom_histogram(bins = 200L, fill = "#0c4c8a") +
  theme_minimal()


PSR_Proagro_Union_Mun <- union_all(
        x = Proagro_soja_mun %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
        y = PSR_soja_mun %>%  select(-c("Produtividade_Seg_Media", "PE_NIVEL_COBERTURA", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "Cobertura_Media", "NR_PRODUTIVIDADE_ESTIMADA"))
)



PSR_Proagro_Union_Mun_Programa <- union_all(
  x = Proagro_soja_mun_programa %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
  y = PSR_soja_mun_programa %>%  select(-c("Produtividade_Seg_Media", "PE_NIVEL_COBERTURA", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "Cobertura_Media", "NR_PRODUTIVIDADE_ESTIMADA"))
)


Test_Disp <- PSR_Proagro_Union_Mun 
ggplot(Test_Disp) +
  aes(x = Area_Media, y = Premio_Liquido_Medio, colour = Tipo) +
  geom_point(size = 1L) +
  scale_color_hue() +
  theme_minimal()



Test_Disp <- PSR_Proagro_Union_Mun_Programa %>%
  filter(Area_Media >= 2L & Area_Media <= 250L)

ggplot(Test_Disp) +
  aes(x = Area_Media, y = Premio_Liquido_Medio, colour = Tipo) +
  geom_point(size = 1L) +
  scale_color_hue() +
  theme_minimal()




# Histogramas
PSR_Proagro_Union_Mun %>%
ggplot() +
  aes(x = Premio_Area, fill = Tipo) +
  geom_histogram(alpha = 0.5, bins = 50L, position = 'identity') +
  scale_fill_hue() +
  theme_minimal()

PSR_Proagro_Union_Mun_Programa %>%
  ggplot() +
  aes(x = Premio_Area, fill = Tipo) +
  geom_histogram(alpha = 0.5, bins = 50L, position = 'identity') +
  scale_fill_hue() +
  theme_minimal()



Proagro_soja_mun_programa %>%
  ggplot() +
  aes(x = Premio_Area, fill = Tipo) +
  geom_histogram(alpha = 0.5, bins = 50L, position = 'identity') +
  scale_fill_hue() +
  theme_minimal()




# Diferenca entre
PSR_Proagro_lado_lado_mun <-  left_join(
                  x = PSR_soja_mun,
                  y = Proagro_soja_mun,
                  by =c("Cod_Municipio" = "Cod_Municipio")
)
PSR_Proagro_lado_lado_mun <- PSR_Proagro_lado_lado_mun[!is.na(PSR_Proagro_lado_lado_mun$NR_AREA_TOTAL.y),]

PSR_Proagro_lado_lado_mun$Diferenca_Premio <- PSR_Proagro_lado_lado_mun$Premio_Area.x - PSR_Proagro_lado_lado_mun$Premio_Area.y
PSR_Proagro_lado_lado_mun$Diferenca_Premio_Perc <- PSR_Proagro_lado_lado_mun$Diferenca_Premio/PSR_Proagro_lado_lado_mun$Premio_Area.x 



PSR_Proagro_lado_lado_mun <- left_join(
                                x = PSR_Proagro_lado_lado_mun,
                                y = municipios,
                                by = c("Cod_Municipio" = "Cod_Municipio" )
)

stby(data = PSR_Proagro_lado_lado_mun[,c("Diferenca_Premio","Sigla_UF")], 
     INDICES = PSR_Proagro_lado_lado_mun$Sigla_UF, 
     FUN = descr, stats = c("mean", "sd", "min", "med", "max"), 
     transpose = TRUE)


test_plot <- PSR_Proagro_lado_lado_mun %>% select(Cod_Municipio,Diferenca_Premio)

test_plot$Plot <- test_plot$Diferenca_Premio
plot_map_mun(dados = test_plot, "Plot")


ggplot(PSR_Proagro_lado_lado_mun) +
 aes(x = Diferenca_Premio) +
 geom_histogram(bins = 30L, fill = "#0c4c8a") +
 theme_minimal()

ggplot(PSR_Proagro_lado_lado_mun) +
  aes(x = Diferenca_Premio_Perc) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()


openxlsx::write.xlsx(PSR_Proagro_lado_lado_mun, "df.xlsx")
shell.exec("df.xlsx")








PSR_Proagro_Union_regiao <- union_all(
  x = Proagro_soja_regiao %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
  y = PSR_soja_regiao %>%  select(-c("Produtividade_Seg_Media", "PE_NIVEL_COBERTURA", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "Cobertura_Media", "NR_PRODUTIVIDADE_ESTIMADA"))
)


cor(PSR_Proagro_Union_regiao[,8:12])





# Histogramas
PSR_Proagro_Union_regiao %>%
  ggplot() +
  aes(x = Premio_Area, fill = Tipo) +
  geom_histogram(alpha = 0.5, bins = 30L, position = 'identity') +
  scale_fill_hue() +
  theme_minimal()


PSR_Proagro_lado_lado_regiao <-  left_join(
  x = PSR_soja_regiao,
  y = Proagro_soja_regiao,
  by =c("Cod_Microrregiao" = "Cod_Microrregiao")
)
PSR_Proagro_lado_lado_regiao <- PSR_Proagro_lado_lado_regiao[!is.na(PSR_Proagro_lado_lado_regiao$NR_AREA_TOTAL.y),]

PSR_Proagro_lado_lado_regiao$Diferenca_Premio <- (PSR_Proagro_lado_lado_regiao$Premio_Area.x - PSR_Proagro_lado_lado_regiao$Premio_Area.y)

ggplot(PSR_Proagro_lado_lado_regiao) +
  aes(x = Diferenca_Premio) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

