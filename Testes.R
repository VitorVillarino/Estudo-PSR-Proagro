

distritos <- read_xls("./Dados/Auxiliares/DTB_BRASIL_DISTRITO.xls",
                      col_types = c(
                        "text", #UF
                        "text", #Nome_UF
                        "text", #Sigla_UF
                        "text", #Mesorregião Geográfica
                        "text", #Nome_Mesorregião
                        "text", #Microrregião Geográfica
                        "text", #Nome_Microrregião
                        "text", #Município
                        "text", #Código Município Completo
                        "text", #Nome_Município
                        "text", #Distrito
                        "text", #Código de Distrito Completo
                        "text", #Nome_Distrito
                        "text" #Nome_Distrito_Maiuscula
                      ))




municipios_nao_encontrados_PSR <- anti_join(data_PSR, municipios, by = c("SG_UF_PROPRIEDADE" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
  left_join(distritos, by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
  distinct(MUNICIPIO_CORRIGIDO, SG_UF_PROPRIEDADE, Município, Nome_Município, Nome_Distrito)
municipios_nao_encontrados_PSR$testes <- paste('                                  NM_MUNICIPIO_PROPRIEDADE == "', municipios_nao_encontrados_PSR$MUNICIPIO_CORRIGIDO ,'" & SG_UF_PROPRIEDADE == "', municipios_nao_encontrados_PSR$SG_UF_PROPRIEDADE ,'" ~ "', toupper(municipios_nao_encontrados_PSR$Nome_Município),'",')

write_xlsx(municipios_nao_encontrados_PSR, "test.xlsx")


municipios_nao_encontrados_Proagro <- anti_join(data_Proagro, municipios, by = c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
  left_join(distritos, by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
  distinct(MUNICIPIO, UF, Município, Nome_Município, Nome_Distrito)
write.xlsx(municipios_nao_encontrados_Proagro, file = 'municipios_nao_encontrados.xlsx')









data_PSR2 <- data_PSR
data_PSR2$Premio_Area              = data_PSR2$VL_PREMIO_LIQUIDO/data_PSR2$NR_AREA_TOTAL


esquisse::esquisser()
p <- c(0.05, 0.95)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = c("min","max"))

p_funs

test <- data_PSR2 %>% 
  filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) %>% 
  group_by(Cod_Municipio, AN_SAFRA) %>% 
  summarize_at(vars(Premio_Area), funs(!!!p_funs))

test <- data_PSR2 %>% 
  filter(Produto_Padronizado == "SOJA", VL_LIMITE_GARANTIA <= 300000) %>% 
  group_by(Cod_Municipio, AN_SAFRA) %>% 
  summarise(std_dev = sd(Premio_Area))



test$range <- (test$max - test$min)/6
test %>% 
  group_by(AN_SAFRA) %>% 
  summarise(max = max(std_dev, na.rm = T)/6,
            min = min(std_dev, na.rm = T))




%>% 
  group_by(AN_SAFRA) %>% 
  summarise(
    max = max(range)
  ) 

