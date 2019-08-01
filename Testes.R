

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



# Tabela 6615 - Número de estab2
# por produtos da lavoura temporária - resultados preliminares 2017
# data_Censo_6615 <- read.csv("./Dados/Raw/Censo - 2017/tabela6615.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6616 - Número de estabelecimentos agropecuários e Número de pés existentes, por produtos da 
# lavoura permanente - resultados preliminares 2017
# data_Censo_6616 <- read.csv("./Dados/Raw/Censo - 2017/tabela6616.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6618 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos 
# da agroindústria rural - resultados preliminares 2017
# data_Censo_6618 <- read.csv("./Dados/Raw/Censo - 2017/tabela6618.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')


# Tabela 6619 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos 
# da horticultura - resultados preliminares 2017
# data_Censo_6619 <- read.csv("./Dados/Raw/Censo - 2017/tabela6619.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM')

# municipios_nao_encontrados_PSR <- anti_join(data_PSR, municipios, by = c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(NM_MUNICIPIO_PROPRIEDADE_CORRIGIDO, SG_UF_PROPRIEDADE, Município, Nome_Município, Nome_Distrito)

# municipios_nao_encontrados_Proagro <- anti_join(data_Proagro, municipios, by = c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula"))  %>%
#   left_join(distritos, by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Distrito_Maiuscula")) %>%
#   distinct(MUNICIPIO, UF, Município, Nome_Município, Nome_Distrito)
# write.xlsx(municipios_nao_encontrados_Proagro, file = 'municipios_nao_encontrados.xlsx')
