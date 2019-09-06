source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
source("./Scripts/02_Transformacoes_Necessarias.R", encoding = "UTF-8")
library(ggplot2)
library(summarytools)


#Teste Valor Deflacionado
Valores_Deflacionados <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA) %>%
  summarise(
    num_seguros = sum(QT_ENQ),
    VL_TOTAL = sum(VL_TOT),
    VL_TOTAL_deflacionado = sum(VL_TOTAL_deflacionado))
Valores_Deflacionados$VL_TOTAL_Medio             = Valores_Deflacionados$VL_TOTAL/Valores_Deflacionados$num_seguros
Valores_Deflacionados$VL_TOTAL_Medio_Defl        = Valores_Deflacionados$VL_TOTAL_deflacionado/Valores_Deflacionados$num_seguros


stby(data = Valores_Deflacionados$VL_TOTAL_Medio_Defl, 
     INDICES = Valores_Deflacionados$SAFRA, 
     FUN = descr, stats = c("mean", "sd", "min", "med", "max"))






Proagro_soja_mun <- Proagro_soja_mun[Proagro_soja_mun$Pelo_Menos_30,]


descr(Proagro_soja_mun$INDICE_SINISTRO_VALOR)

stby(data = Proagro_soja_mun$INDICE_SINISTRO_VALOR_Defl, 
     INDICES = Proagro_soja_mun$PROGRAMA, 
     FUN = descr, stats = c("mean", "sd", "min", "med", "max"), 
     transpose = TRUE)



Proagro_soja_mun <- Proagro_soja_mun[Proagro_soja_mun$VL_SINISTROS > 0,]
descr(Proagro_soja_mun$INDICE_SINISTRO_VALOR)





#Censo
data_censo_soja <- data_Censo_6615 %>% filter(Produtos=="Soja em grão (Toneladas)")

