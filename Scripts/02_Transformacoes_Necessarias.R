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

