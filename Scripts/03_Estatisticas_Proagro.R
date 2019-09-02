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


#Verificando quais os municípios com menos de 30 seguros em alguma safra
Cont_Seg_Safra <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA", SAFRA != "2019/2020") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA) %>%
  summarise(
    num_seguros = sum(QT_ENQ))
menos_30_em_alguma_safra <- Cont_Seg_Safra[Cont_Seg_Safra$num_seguros<30,]
length(unique(Cont_Seg_Safra$Cod_Municipio))
Cont_Seg_Safra <- Cont_Seg_Safra[!(Cont_Seg_Safra$Cod_Municipio %in% menos_30_em_alguma_safra$Cod_Municipio),]
length(unique(Cont_Seg_Safra$Cod_Municipio))



#Proagro - Agrupando Municipio Produto
Proagro_soja_mun <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, PROGRAMA, TP_SEGURO) %>% 
  summarise(
    num_seguros = sum(QT_ENQ),
    NR_AREA_TOTAL = sum(AREA),
    VL_FIN = sum(VL_FIN),                                          #valor financiado
    VL_RECP = sum(VL_RECP),                                        #recursos próprios
    VL_INV = sum(VL_INV),                                          #parcela do investimento
    VL_GRM = sum(VL_GRM),                                          #reda mínima
    VL_TOTAL = sum(VL_TOT),                                        #Valor total
    VL_TOTAL_deflacionado = sum(VL_TOTAL_deflacionado),            #Valor total deflacionado 
    VL_Premio_Deflacionado = sum(VL_Premio_Deflacionado),          #Valor do adicional deflacionado  
    VL_Premio = sum(VL_ADIC),
    VL_SUBVENCAO_FEDERAL = 0,
    RECEITA_ESTIMADA = sum(RBE),
    QT_SINISTROS = sum(QT_COB_DEF),
    VL_SINISTROS = sum(VL_COB_DEF),
    VL_SINISTROS_Deflacionado = sum(VL_COB_DEF_Deflacionado)
  )
Proagro_soja_mun$Area_Media                 = Proagro_soja_mun$NR_AREA_TOTAL/Proagro_soja_mun$num_seguros
Proagro_soja_mun$VL_TOTAL_Medio             = Proagro_soja_mun$VL_TOTAL/Proagro_soja_mun$num_seguros
Proagro_soja_mun$VL_TOTAL_Medio_Defl        = Proagro_soja_mun$VL_TOTAL_deflacionado/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Subvencao_Media            = Proagro_soja_mun$VL_SUBVENCAO_FEDERAL/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Receita_Media              = Proagro_soja_mun$RECEITA_ESTIMADA/Proagro_soja_mun$num_seguros
Proagro_soja_mun$VL_TOTAL_Medio_Area        = Proagro_soja_mun$VL_TOTAL/Proagro_soja_mun$NR_AREA_TOTAL
Proagro_soja_mun$VL_TOTAL_Medio_Area_Defl   = Proagro_soja_mun$VL_TOTAL_deflacionado/Proagro_soja_mun$NR_AREA_TOTAL

Proagro_soja_mun$Premio_Liquido_Medio       = Proagro_soja_mun$VL_Premio/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Premio_Area                = Proagro_soja_mun$VL_Premio/Proagro_soja_mun$NR_AREA_TOTAL
Proagro_soja_mun$Premio_Liquido_Medio_Defl  = Proagro_soja_mun$VL_Premio_Deflacionado/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Premio_Area_Defl           = Proagro_soja_mun$VL_Premio_Deflacionado/Proagro_soja_mun$NR_AREA_TOTAL

Proagro_soja_mun$INDICE_SINISTRO_QTD        = Proagro_soja_mun$QT_SINISTROS/Proagro_soja_mun$num_seguros
Proagro_soja_mun$VL_SINISTRO_MEDIO          = Proagro_soja_mun$VL_SINISTROS/Proagro_soja_mun$QT_SINISTROS
Proagro_soja_mun$VL_SINISTRO_MEDIO_Defl     = Proagro_soja_mun$VL_SINISTROS_Deflacionado/Proagro_soja_mun$QT_SINISTROS

Proagro_soja_mun$SINISTRALIDADE_Defl        = Proagro_soja_mun$VL_SINISTROS_Deflacionado/Proagro_soja_mun$num_seguros

Proagro_soja_mun$INDICE_SINISTRO_VALOR      = Proagro_soja_mun$VL_SINISTROS/Proagro_soja_mun$VL_TOTAL
Proagro_soja_mun$INDICE_SINISTRO_VALOR_Defl = Proagro_soja_mun$VL_SINISTROS_Deflacionado/Proagro_soja_mun$VL_TOTAL_deflacionado





Proagro_soja_mun$Tipo                     = "Proagro"
Proagro_soja_mun$Pelo_Menos_30 <- Proagro_soja_mun$Cod_Municipio %in% menos_30_em_alguma_safra$Cod_Municipio


#Estatísticas para saber
stat_Maior_30 <- Proagro_soja_mun %>% filter(Pelo_Menos_30 == T)
ctable(stat_Maior_30$PROGRAMA, stat_Maior_30$TP_SEGURO, prop = "r")
stat_Menor_30 <- Proagro_soja_mun %>% filter(Pelo_Menos_30 == F)
ctable(stat_Menor_30$PROGRAMA, stat_Menor_30$TP_SEGURO, prop = "r")


Proagro_soja_mun <- Proagro_soja_mun[
  (Proagro_soja_mun$TP_SEGURO == "PROAGRO MAIS"  & Proagro_soja_mun$PROGRAMA == "PRONAF") |
    (Proagro_soja_mun$TP_SEGURO == "PROAGRO TRADICIONAL"  & Proagro_soja_mun$PROGRAMA == "PRONAMP") |
    (Proagro_soja_mun$TP_SEGURO == "PROAGRO TRADICIONAL"  & Proagro_soja_mun$PROGRAMA == "SEM PROGRAMA"),]

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

