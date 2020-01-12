teste_comparacao_premio_area <-  
  Proagro_soja_mun_geral_safra %>% ungroup() %>%   
  select(Cod_Municipio,SAFRA, VL_TOTAL_Medio_Area_Defl) %>% 
  rename(VL_PROAGRO = VL_TOTAL_Medio_Area_Defl) %>% 
full_join(   
  PSR_soja_mun_safra %>% filter(SAFRA != "2012/2013") %>% ungroup() %>% 
  select(Cod_Municipio,SAFRA, VL_LIMITE_GARANTIA_Area_Defl) %>% 
  rename(VL_PSR = VL_LIMITE_GARANTIA_Area_Defl),
  by = c("Cod_Municipio", "SAFRA")
)

unique(Proagro_soja_mun_geral_safra$SAFRA)
unique(PSR_soja_mun_safra$SAFRA)



teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PROAGRO),]
teste_comparacao_premio_area <- teste_comparacao_premio_area[!is.na(teste_comparacao_premio_area$VL_PSR),]
teste_comparacao_premio_area$dif <- (teste_comparacao_premio_area$VL_PSR - teste_comparacao_premio_area$VL_PROAGRO)/teste_comparacao_premio_area$VL_PSR

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

teste_comparacao_premio_area$media <- apply(teste_comparacao_premio_area[,2:7],1, FUN = mean)

teste_comparacao_premio_area <- teste_comparacao_premio_area[teste_comparacao_premio_area$Cod_Municipio != 4102703,]
teste_comparacao_premio_area <- 
  teste_comparacao_premio_area[
      abs(teste_comparacao_premio_area$`2013/2014`) <= 0.10 &
      abs(teste_comparacao_premio_area$`2014/2015`) <= 0.10 &
      abs(teste_comparacao_premio_area$`2015/2016`) <= 0.10 &
      abs(teste_comparacao_premio_area$`2016/2017`) <= 0.10 &
      abs(teste_comparacao_premio_area$`2017/2018`) <= 0.10 &
      abs(teste_comparacao_premio_area$`2018/2019`) <= 0.10
    ,]

esquisse::esquisser()

plot_mun_brasil_brazilmaps(teste_comparacao_premio_area, "media")  







