

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




# municipios_nao_encontrados_PSR <- anti_join(data_PSR, municipios, by = c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO, SG_UF_PROPRIEDADE, Município, Nome_Município, Nome_Distrito)

# municipios_nao_encontrados_Proagro <- anti_join(data_Proagro, municipios, by = c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(MUNICIPIO, UF, Município, Nome_Município, Nome_Distrito)
# write.xlsx(municipios_nao_encontrados_Proagro, file = 'municipios_nao_encontrados.xlsx')
