#PSR - Agrupando por Municipio/SAFRA
PSR_soja_mun <- data_PSR %>% 
  filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA) %>% 
  summarise(
    num_seguros = n(),
    NR_AREA_TOTAL = sum(NR_AREA_TOTAL),
    VL_LIMITE_GARANTIA = sum(VL_LIMITE_GARANTIA),
    VL_PREMIO_LIQUIDO = sum(VL_PREMIO_LIQUIDO),
    VL_SUBVENCAO_FEDERAL = sum(VL_SUBVENCAO_FEDERAL),
    VL_PREMIO_PAGO = sum(VL_PREMIO_PAGO),
    NR_PRODUTIVIDADE_ESTIMADA = sum(NR_PRODUTIVIDADE_ESTIMADA),
    NR_PRODUTIVIDADE_SEGURADA = sum(NR_PRODUTIVIDADE_SEGURADA),
    QT_SINISTROS = sum(!is.na(VALOR_INDENIZAÇÃO)),
    VL_SINISTROS = sum(VALOR_INDENIZAÇÃO, na.rm = T)
  )

PSR_soja_mun$VL_PREMIO_PAGO           = PSR_soja_mun$VL_PREMIO_LIQUIDO - PSR_soja_mun$VL_SUBVENCAO_FEDERAL
PSR_soja_mun$VL_PREMIO_PAGO_Medio     = PSR_soja_mun$VL_PREMIO_PAGO/PSR_soja_mun$num_seguros
PSR_soja_mun$Area_Media               = PSR_soja_mun$NR_AREA_TOTAL/PSR_soja_mun$num_seguros
PSR_soja_mun$Limite_Medio             = PSR_soja_mun$VL_LIMITE_GARANTIA/PSR_soja_mun$num_seguros
PSR_soja_mun$Premio_Liquido_Medio     = PSR_soja_mun$VL_PREMIO_LIQUIDO/PSR_soja_mun$num_seguros
PSR_soja_mun$Subvencao_Media          = PSR_soja_mun$VL_SUBVENCAO_FEDERAL/PSR_soja_mun$num_seguros
PSR_soja_mun$Produtividade_Media      = PSR_soja_mun$NR_PRODUTIVIDADE_ESTIMADA/PSR_soja_mun$num_seguros
PSR_soja_mun$Produtividade_Seg_Media  = PSR_soja_mun$NR_PRODUTIVIDADE_SEGURADA/PSR_soja_mun$num_seguros
PSR_soja_mun$Premio_Area              = PSR_soja_mun$VL_PREMIO_LIQUIDO/PSR_soja_mun$NR_AREA_TOTAL
PSR_soja_mun$INDICE_SINISTRO          = PSR_soja_mun$QT_SINISTROS/PSR_soja_mun$num_seguros
PSR_soja_mun$VL_SINISTO_MEDIO         = PSR_soja_mun$VL_SINISTROS/PSR_soja_mun$QT_SINISTROS
PSR_soja_mun$Tipo                     = "PSR"

PSR_soja_mun_programa <- PSR_soja_mun
PSR_soja_mun_programa$PROGRAMA = 'PSR'




#Proagro - Agrupando Municipio Produto

Proagro_soja_mun <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA) %>% 
  summarise(
    num_seguros = sum(QT_ENQ),
    NR_AREA_TOTAL = sum(AREA),
    VL_FIN = sum(VL_FIN),           #valor financiado
    VL_RECP = sum(VL_RECP),         #recursos próprios
    VL_INV = sum(VL_INV),           #parcela do investimento
    VL_GRM = sum(VL_GRM),           #reda mínima
    VL_TOT = sum(VL_TOT),           #Valor total
    VL_Premio = sum(VL_ADIC),
    VL_SUBVENCAO_FEDERAL = 0,
    RECEITA_ESTIMADA = sum(RBE),
    QT_SINISTROS = sum(QT_COB_DEF),
    VL_SINISTROS = sum(VL_COB_DEF)
  )
Proagro_soja_mun$Area_Media               = Proagro_soja_mun$NR_AREA_TOTAL/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Valor_Total_Medio        = Proagro_soja_mun$VL_TOT/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Premio_Liquido_Medio     = Proagro_soja_mun$VL_Premio/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Subvencao_Media          = Proagro_soja_mun$VL_SUBVENCAO_FEDERAL/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Receita_Media            = Proagro_soja_mun$RECEITA_ESTIMADA/Proagro_soja_mun$num_seguros
Proagro_soja_mun$Premio_Area              = Proagro_soja_mun$VL_Premio/Proagro_soja_mun$NR_AREA_TOTAL
Proagro_soja_mun$INDICE_SINISTRO          = Proagro_soja_mun$QT_SINISTROS/Proagro_soja_mun$num_seguros
Proagro_soja_mun$VL_SINISTO_MEDIO         = Proagro_soja_mun$VL_SINISTROS/Proagro_soja_mun$QT_SINISTROS
Proagro_soja_mun$Tipo                     = "Proagro"



Proagro_soja_mun_programa <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA, TP_SEGURO) %>% 
  summarise(
    num_seguros = sum(QT_ENQ),
    NR_AREA_TOTAL = sum(AREA),
    VL_FIN = sum(VL_FIN),           #valor financiado
    VL_RECP = sum(VL_RECP),         #recursos próprios
    VL_INV = sum(VL_INV),           #parcela do investimento
    VL_GRM = sum(VL_GRM),           #reda mínima
    VL_TOT = sum(VL_TOT),           #Valor total
    VL_Premio = sum(VL_ADIC),
    VL_SUBVENCAO_FEDERAL = 0,
    RECEITA_ESTIMADA = sum(RBE),
    QT_SINISTROS = sum(QT_COB_DEF),
    VL_SINISTROS = sum(VL_COB_DEF)
  )


Proagro_soja_mun_programa$Area_Media              = Proagro_soja_mun_programa$NR_AREA_TOTAL/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$Valor_Total_Medio       = Proagro_soja_mun_programa$VL_TOT/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$Premio_Liquido_Medio    = Proagro_soja_mun_programa$VL_Premio/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$Subvencao_Media         = Proagro_soja_mun_programa$VL_SUBVENCAO_FEDERAL/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$Receita_Media           = Proagro_soja_mun_programa$RECEITA_ESTIMADA/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$Premio_Area             = Proagro_soja_mun_programa$VL_Premio/Proagro_soja_mun_programa$NR_AREA_TOTAL
Proagro_soja_mun_programa$INDICE_SINISTRO         = Proagro_soja_mun_programa$QT_SINISTROS/Proagro_soja_mun_programa$num_seguros
Proagro_soja_mun_programa$VL_SINISTO_MEDIO        = Proagro_soja_mun_programa$VL_SINISTROS/Proagro_soja_mun_programa$QT_SINISTROS


PSR_Proagro_Union_Mun <- union_all(
  x = Proagro_soja_mun %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
  y = PSR_soja_mun %>%  select(-c("Produtividade_Seg_Media", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "NR_PRODUTIVIDADE_ESTIMADA"))
)



PSR_Proagro_Union_Mun_Programa <- union_all(
  x = Proagro_soja_mun_programa %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
  y = PSR_soja_mun_programa %>%  select(-c("Produtividade_Seg_Media", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "NR_PRODUTIVIDADE_ESTIMADA"))
)



PSR_Proagro_lado_lado_mun <-  left_join(
  x = PSR_soja_mun,
  y = Proagro_soja_mun,
  by =c("Cod_Municipio" = "Cod_Municipio")
)













#################### Microrregião - A ser visto #################################

#PSR
PSR_soja_regiao <- data_PSR %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Microrregiao, Produto_Padronizado) %>% 
  summarise(
    num_seguros = n(),
    NR_AREA_TOTAL = sum(NR_AREA_TOTAL),
    VL_LIMITE_GARANTIA = sum(VL_LIMITE_GARANTIA),
    VL_PREMIO_LIQUIDO = sum(VL_PREMIO_LIQUIDO),
    VL_SUBVENCAO_FEDERAL = sum(VL_SUBVENCAO_FEDERAL),
    NR_PRODUTIVIDADE_ESTIMADA = sum(NR_PRODUTIVIDADE_ESTIMADA),
    NR_PRODUTIVIDADE_SEGURADA = sum(NR_PRODUTIVIDADE_SEGURADA),
    PE_NIVEL_COBERTURA = sum(PE_NIVEL_COBERTURA)
  )
PSR_soja_regiao$Area_Media               = PSR_soja_regiao$NR_AREA_TOTAL/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Limite_Medio             = PSR_soja_regiao$VL_LIMITE_GARANTIA/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Premio_Liquido_Medio     = PSR_soja_regiao$VL_PREMIO_LIQUIDO/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Subvencao_Media          = PSR_soja_regiao$VL_SUBVENCAO_FEDERAL/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Produtividade_Media      = PSR_soja_regiao$NR_PRODUTIVIDADE_ESTIMADA/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Produtividade_Seg_Media  = PSR_soja_regiao$NR_PRODUTIVIDADE_SEGURADA/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Cobertura_Media          = PSR_soja_regiao$PE_NIVEL_COBERTURA/PSR_soja_regiao$num_seguros
PSR_soja_regiao$Premio_Area              = PSR_soja_regiao$VL_PREMIO_LIQUIDO/PSR_soja_regiao$NR_AREA_TOTAL
PSR_soja_regiao$Tipo                     = "PSR"





#Proagro
#Ver com Iran valor de subvenção federal
Proagro_soja_regiao <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Microrregiao, Produto_Padronizado) %>% 
  summarise(
    num_seguros = sum(QUANTIDADE),
    NR_AREA_TOTAL = sum(AREA),
    VL_LIMITE_GARANTIA = sum(VALOR_TOTAL),
    VL_PREMIO_LIQUIDO = sum(VALOR_ADICIONAL),
    VL_SUBVENCAO_FEDERAL = 0,
    RECEITA_ESTIMADA = sum(RBE)
  )
Proagro_soja_regiao$Area_Media               = Proagro_soja_regiao$NR_AREA_TOTAL/Proagro_soja_regiao$num_seguros
Proagro_soja_regiao$Limite_Medio             = Proagro_soja_regiao$VL_LIMITE_GARANTIA/Proagro_soja_regiao$num_seguros
Proagro_soja_regiao$Premio_Liquido_Medio     = Proagro_soja_regiao$VL_PREMIO_LIQUIDO/Proagro_soja_regiao$num_seguros
Proagro_soja_regiao$Subvencao_Media          = Proagro_soja_regiao$VL_SUBVENCAO_FEDERAL/Proagro_soja_regiao$num_seguros
Proagro_soja_regiao$Receita_Media            = Proagro_soja_regiao$RECEITA_ESTIMADA/Proagro_soja_regiao$num_seguros
Proagro_soja_regiao$Premio_Area              = Proagro_soja_regiao$VL_PREMIO_LIQUIDO/Proagro_soja_regiao$NR_AREA_TOTAL
Proagro_soja_regiao$Tipo                     = "Proagro"



PSR_Proagro_Union_regiao <- union_all(
  x = Proagro_soja_regiao %>%  select ( -c("Receita_Media", "RECEITA_ESTIMADA")),
  y = PSR_soja_regiao %>%  select(-c("Produtividade_Seg_Media", "PE_NIVEL_COBERTURA", "Produtividade_Media", "NR_PRODUTIVIDADE_SEGURADA", "Cobertura_Media", "NR_PRODUTIVIDADE_ESTIMADA"))
)


PSR_Proagro_lado_lado_regiao <-  left_join(
  x = PSR_soja_regiao,
  y = Proagro_soja_regiao,
  by =c("Cod_Microrregiao" = "Cod_Microrregiao")
)
PSR_Proagro_lado_lado_regiao <- PSR_Proagro_lado_lado_regiao[!is.na(PSR_Proagro_lado_lado_regiao$NR_AREA_TOTAL.y),]


