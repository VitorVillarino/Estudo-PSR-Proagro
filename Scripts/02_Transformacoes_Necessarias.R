################################# Inflação #################################

data_Proagro <- data_Proagro %>% 
  mutate(VL_TOTAL_deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_TOT / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_TOT / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_TOT / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_TOT / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_TOT / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_TOT / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_TOT
           )
  )

data_Proagro <- data_Proagro %>% 
  mutate(VL_Premio_Deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_ADIC / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_ADIC / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_ADIC / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_ADIC / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_ADIC / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_ADIC / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_ADIC
           )
  )

data_Proagro <- data_Proagro %>% 
  mutate(VL_COB_DEF_Deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_COB_DEF / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_COB_DEF / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_COB_DEF / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_COB_DEF / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_COB_DEF / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_COB_DEF / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_COB_DEF
           )
  )






data_PSR <- data_PSR %>% 
  mutate(VL_LIMITE_GARANTIA_deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_LIMITE_GARANTIA / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_LIMITE_GARANTIA / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_LIMITE_GARANTIA / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_LIMITE_GARANTIA / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_LIMITE_GARANTIA / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_LIMITE_GARANTIA / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_LIMITE_GARANTIA
           )
  )



data_PSR <- data_PSR %>% 
  mutate(VL_PREMIO_LIQUIDO_deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_PREMIO_LIQUIDO / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_PREMIO_LIQUIDO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_PREMIO_LIQUIDO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_PREMIO_LIQUIDO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_PREMIO_LIQUIDO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_PREMIO_LIQUIDO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_PREMIO_LIQUIDO
           )
  )


data_PSR <- data_PSR %>% 
  mutate(VL_PREMIO_PAGO_deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_PREMIO_PAGO / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_PREMIO_PAGO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_PREMIO_PAGO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_PREMIO_PAGO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_PREMIO_PAGO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_PREMIO_PAGO / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_PREMIO_PAGO
           )
  )



data_PSR <- data_PSR %>% 
  mutate(VL_SUBVENCAO_FEDERAL_deflacionado = 
           case_when(
             SAFRA  == "2014/2015" ~ VL_SUBVENCAO_FEDERAL / (1 + inflacao$jul2014/100),
             SAFRA  == "2015/2016" ~ VL_SUBVENCAO_FEDERAL / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)),
             SAFRA  == "2016/2017" ~ VL_SUBVENCAO_FEDERAL / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)),
             SAFRA  == "2017/2018" ~ VL_SUBVENCAO_FEDERAL / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)),
             SAFRA  == "2018/2019" ~ VL_SUBVENCAO_FEDERAL / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)),
             SAFRA  == "2019/2020" ~ VL_SUBVENCAO_FEDERAL / ((1 + inflacao$jul2014/100)*(1 + inflacao$jul2015/100)*(1 + inflacao$jul2016/100)*(1 + inflacao$jul2017/100)*(1 + inflacao$jul2018/100)*(1 + inflacao$jul2019/100)),
             TRUE ~ VL_SUBVENCAO_FEDERAL
           )
  )







################################# Proagro #################################



#Verificando quais os municípios com menos de 30 seguros em alguma safra
Cont_Seg_Safra <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA", SAFRA != "2019/2020") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, SAFRA) %>%
  summarise(
    num_seguros = sum(QT_ENQ))
menos_30_em_alguma_safra <- Cont_Seg_Safra[Cont_Seg_Safra$num_seguros<30,]
Cont_Seg_Safra <- Cont_Seg_Safra[!(Cont_Seg_Safra$Cod_Municipio %in% menos_30_em_alguma_safra$Cod_Municipio),]




#Proagro - Agrupando Municipio Produto - Geral
Proagro_soja_mun_geral <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA", SAFRA != '2019/2020') %>% 
  group_by(Cod_Municipio, Produto_Padronizado) %>% 
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
Proagro_soja_mun_geral$Area_Media                 = Proagro_soja_mun_geral$NR_AREA_TOTAL/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$VL_TOTAL_Medio             = Proagro_soja_mun_geral$VL_TOTAL/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$VL_TOTAL_Medio_Defl        = Proagro_soja_mun_geral$VL_TOTAL_deflacionado/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$Subvencao_Media            = Proagro_soja_mun_geral$VL_SUBVENCAO_FEDERAL/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$Receita_Media              = Proagro_soja_mun_geral$RECEITA_ESTIMADA/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$VL_TOTAL_Medio_Area        = Proagro_soja_mun_geral$VL_TOTAL/Proagro_soja_mun_geral$NR_AREA_TOTAL
Proagro_soja_mun_geral$VL_TOTAL_Medio_Area_Defl   = Proagro_soja_mun_geral$VL_TOTAL_deflacionado/Proagro_soja_mun_geral$NR_AREA_TOTAL
Proagro_soja_mun_geral$Premio_Liquido_Medio       = Proagro_soja_mun_geral$VL_Premio/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$Premio_Area                = Proagro_soja_mun_geral$VL_Premio/Proagro_soja_mun_geral$NR_AREA_TOTAL
Proagro_soja_mun_geral$Premio_Liquido_Medio_Defl  = Proagro_soja_mun_geral$VL_Premio_Deflacionado/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$Premio_Area_Defl           = Proagro_soja_mun_geral$VL_Premio_Deflacionado/Proagro_soja_mun_geral$NR_AREA_TOTAL
Proagro_soja_mun_geral$INDICE_SINISTRO_QTD        = Proagro_soja_mun_geral$QT_SINISTROS/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$VL_SINISTRO_MEDIO          = Proagro_soja_mun_geral$VL_SINISTROS/Proagro_soja_mun_geral$QT_SINISTROS
Proagro_soja_mun_geral$VL_SINISTRO_MEDIO_Defl     = Proagro_soja_mun_geral$VL_SINISTROS_Deflacionado/Proagro_soja_mun_geral$QT_SINISTROS
Proagro_soja_mun_geral$SINISTRALIDADE_Defl        = Proagro_soja_mun_geral$VL_SINISTROS_Deflacionado/Proagro_soja_mun_geral$num_seguros
Proagro_soja_mun_geral$INDICE_SINISTRO_VALOR      = Proagro_soja_mun_geral$VL_SINISTROS/Proagro_soja_mun_geral$VL_TOTAL
Proagro_soja_mun_geral$INDICE_SINISTRO_VALOR_Defl = Proagro_soja_mun_geral$VL_SINISTROS_Deflacionado/Proagro_soja_mun_geral$VL_TOTAL_deflacionado
Proagro_soja_mun_geral$Tipo                     = "Proagro"
Proagro_soja_mun_geral$Pelo_Menos_30 <- Proagro_soja_mun_geral$Cod_Municipio %in% Cont_Seg_Safra$Cod_Municipio




#Proagro - Agrupando Municipio Produto
Proagro_soja_mun <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA", SAFRA != '2019/2020') %>% 
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
Proagro_soja_mun$Pelo_Menos_30 <- Proagro_soja_mun$Cod_Municipio %in% Cont_Seg_Safra$Cod_Municipio






#Proagro - Agrupando Municipio Produto e Sagra
Proagro_soja_mun_safra <- data_Proagro %>% 
  filter(Produto_Padronizado == "SOJA") %>% 
  group_by(Cod_Municipio, Produto_Padronizado, PROGRAMA, TP_SEGURO, SAFRA) %>% 
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
Proagro_soja_mun_safra$Area_Media                 = Proagro_soja_mun_safra$NR_AREA_TOTAL/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$VL_TOTAL_Medio             = Proagro_soja_mun_safra$VL_TOTAL/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$VL_TOTAL_Medio_Defl        = Proagro_soja_mun_safra$VL_TOTAL_deflacionado/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$Subvencao_Media            = Proagro_soja_mun_safra$VL_SUBVENCAO_FEDERAL/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$Receita_Media              = Proagro_soja_mun_safra$RECEITA_ESTIMADA/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$VL_TOTAL_Medio_Area        = Proagro_soja_mun_safra$VL_TOTAL/Proagro_soja_mun_safra$NR_AREA_TOTAL
Proagro_soja_mun_safra$VL_TOTAL_Medio_Area_Defl   = Proagro_soja_mun_safra$VL_TOTAL_deflacionado/Proagro_soja_mun_safra$NR_AREA_TOTAL
Proagro_soja_mun_safra$Premio_Liquido_Medio       = Proagro_soja_mun_safra$VL_Premio/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$Premio_Area                = Proagro_soja_mun_safra$VL_Premio/Proagro_soja_mun_safra$NR_AREA_TOTAL
Proagro_soja_mun_safra$Premio_Liquido_Medio_Defl  = Proagro_soja_mun_safra$VL_Premio_Deflacionado/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$Premio_Area_Defl           = Proagro_soja_mun_safra$VL_Premio_Deflacionado/Proagro_soja_mun_safra$NR_AREA_TOTAL
Proagro_soja_mun_safra$INDICE_SINISTRO_QTD        = Proagro_soja_mun_safra$QT_SINISTROS/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$VL_SINISTRO_MEDIO          = Proagro_soja_mun_safra$VL_SINISTROS/Proagro_soja_mun_safra$QT_SINISTROS
Proagro_soja_mun_safra$VL_SINISTRO_MEDIO_Defl     = Proagro_soja_mun_safra$VL_SINISTROS_Deflacionado/Proagro_soja_mun_safra$QT_SINISTROS
Proagro_soja_mun_safra$SINISTRALIDADE_Defl        = Proagro_soja_mun_safra$VL_SINISTROS_Deflacionado/Proagro_soja_mun_safra$num_seguros
Proagro_soja_mun_safra$INDICE_SINISTRO_VALOR      = Proagro_soja_mun_safra$VL_SINISTROS/Proagro_soja_mun_safra$VL_TOTAL
Proagro_soja_mun_safra$INDICE_SINISTRO_VALOR_Defl = Proagro_soja_mun_safra$VL_SINISTROS_Deflacionado/Proagro_soja_mun_safra$VL_TOTAL_deflacionado
Proagro_soja_mun_safra$Tipo                     = "Proagro"
Proagro_soja_mun_safra$Pelo_Menos_30 <- Proagro_soja_mun_safra$Cod_Municipio %in% Cont_Seg_Safra$Cod_Municipio



#Transformando em Numeric
Proagro_soja_mun_geral$Cod_Municipio <- as.numeric(Proagro_soja_mun_geral$Cod_Municipio)
Proagro_soja_mun_safra$Cod_Municipio <- as.numeric(Proagro_soja_mun_safra$Cod_Municipio)
Proagro_soja_mun$Cod_Municipio <- as.numeric(Proagro_soja_mun$Cod_Municipio)
municipios$Cod_Municipio <- as.numeric(municipios$Cod_Municipio)


#limpando as variáveis
rm(menos_30_em_alguma_safra)
