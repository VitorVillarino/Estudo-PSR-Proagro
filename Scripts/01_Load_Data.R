library(readxl)
library(dplyr)


#################################################### BASES IBGE ####################################################

#Base de Municípiops do IBGE. Pegaremos daqui o código do Município para fazer algo mais simples e eficiente do que 
#comparar strings.
municipios <- read_xls("./Dados/Auxiliares/DTB_BRASIL_MUNICIPIO.xls",
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
                         "text" #Nome_Município_Maiuscula 
                       )) 
names(municipios)[names(municipios)=="Código Município Completo"] <- "Cod_Municipio"
names(municipios)[names(municipios)=="Microrregião Geográfica"] <- "Cod_Microrregiao"



#################################################### PRODUTOS ####################################################


#Base de Produtos - Padronizados
produtos_padronizados <- read_xlsx("./Dados/Auxiliares/Padronização de Produtos.xlsx",
                                   col_types = c(
                                     "numeric", #idProduto
                                     "text", #Proagro
                                     "text", #PSR
                                     "text", #Tabela 949 - 2006
                                     "text", #Tabela 822 - 2006
                                     "text", #Tabela 6615 - 2017
                                     "text", #Tabela 6616 - 2017
                                     "text", #Tabela 6618
                                     "text", #Tabela 6619
                                     "text"  #Produto Padronizado
                                   ))



#################################################### PSR ####################################################

data_PSR <- read_excel("Dados/Raw/PSR_2013_2018_com_sinistros_Agrupado_pt1.xlsx", 
                             col_types = c("text", #ID_PROPOSTA
                                           "text", #DT_PROPOSTA
                                           "text", #DT_INICIO_VIGENCIA
                                           "text", #DT_FIM_VIGENCIA
                                           "text", #NR_DOCUMENTO_SEGURADO
                                           "text", #NM_MUNICIPIO_PROPRIEDADE
                                           "text", #SG_UF_PROPRIEDADE
                                           "text", #NM_CULTURA_GLOBAL
                                           "numeric", #ANO
                                           "text", #AN_SAFRA
                                           "numeric", #NR_AREA_TOTAL
                                           "numeric", #NR_PRODUTIVIDADE_ESTIMADA
                                           "numeric", #NR_PRODUTIVIDADE_SEGURADA
                                           "numeric", #VL_LIMITE_GARANTIA
                                           "numeric", #VL_PREMIO_LIQUIDO
                                           "numeric"  #VL_SUBVENCAO_FEDERAL
                                       ))

data_PSR <- union_all(data_PSR, read_excel("Dados/Raw/PSR_2013_2018_com_sinistros_Agrupado_pt2.xlsx", 
                                       col_types = c("text", #ID_PROPOSTA
                                                     "text", #DT_PROPOSTA
                                                     "text", #DT_INICIO_VIGENCIA
                                                     "text", #DT_FIM_VIGENCIA
                                                     "text", #NR_DOCUMENTO_SEGURADO
                                                     "text", #NM_MUNICIPIO_PROPRIEDADE
                                                     "text", #SG_UF_PROPRIEDADE
                                                     "text", #NM_CULTURA_GLOBAL
                                                     "numeric", #ANO
                                                     "text", #AN_SAFRA
                                                     "numeric", #NR_AREA_TOTAL
                                                     "numeric", #NR_PRODUTIVIDADE_ESTIMADA
                                                     "numeric", #NR_PRODUTIVIDADE_SEGURADA
                                                     "numeric", #VL_LIMITE_GARANTIA
                                                     "numeric", #VL_PREMIO_LIQUIDO
                                                     "numeric"  #VL_SUBVENCAO_FEDERAL
                                       )))

#Arrumando datas
data_PSR$DT_PROPOSTA <- as.Date(data_PSR$DT_PROPOSTA , origin = "1899-12-30")
data_PSR$DT_INICIO_VIGENCIA <- as.Date(data_PSR$DT_INICIO_VIGENCIA , origin = "1899-12-30")
data_PSR$DT_FIM_VIGENCIA <- as.Date(data_PSR$DT_FIM_VIGENCIA , origin = "1899-12-30")

#Premio Pag
data_PSR$VL_PREMIO_PAGO <- data_PSR$VL_PREMIO_LIQUIDO - data_PSR$VL_SUBVENCAO_FEDERAL 

#Base PSR Indenizações
PSR_Indenizacoes <- read_excel("Dados/Raw/PSR_Indenizacoes.xlsx")
data_PSR <- data_PSR %>% 
            left_join(
                PSR_Indenizacoes,
                by =c("ID_PROPOSTA" = "ID_PROPOSTA")
            )


#Nome do Município maiúscula - Normalizando para cruzar com a base do IBGE 
data_PSR$NM_MUNICIPIO_PROPRIEDADE <- toupper(data_PSR$NM_MUNICIPIO_PROPRIEDADE)


#Agora arrumando os municípios que estão errados (grafia, acento, etc.) ou que na verdade são nomes de distritos. Neste último caso,
#o nome foi trocado para o do município.
data_PSR <- data_PSR %>% mutate(MUNICIPIO_CORRIGIDO = 
                                    case_when( 
                                    NM_MUNICIPIO_PROPRIEDADE == "FOZ DO IGUACU" & SG_UF_PROPRIEDADE == "PR" ~ "FOZ DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "CANDEIA" & SG_UF_PROPRIEDADE == "PR" ~ "MARIPÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMBE" & SG_UF_PROPRIEDADE == "PR" ~ "CAMBÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABERA" & SG_UF_PROPRIEDADE == "SP" ~ "ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO BONITO" & SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MAUA DA SERRA" & SG_UF_PROPRIEDADE == "PR" ~ "MAUÁ DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SENGES" & SG_UF_PROPRIEDADE == "PR" ~ "SENGÉS",
                                    NM_MUNICIPIO_PROPRIEDADE == "JAGUARIAIVA" & SG_UF_PROPRIEDADE == "PR" ~ "JAGUARIAÍVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITARARE" & SG_UF_PROPRIEDADE == "SP" ~ "ITARARÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "QUEDAS DO IGUACU" & SG_UF_PROPRIEDADE == "PR" ~ "QUEDAS DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAUCARIA" & SG_UF_PROPRIEDADE == "PR" ~ "ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPAO ALTO" & SG_UF_PROPRIEDADE == "SC" ~ "CAPÃO ALTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOM JESUS" & SG_UF_PROPRIEDADE == "GO" ~ "BOM JESUS DE GOIÁS",
                                    NM_MUNICIPIO_PROPRIEDADE == "UBERLANDIA" & SG_UF_PROPRIEDADE == "MG" ~ "UBERLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDIANOPOLIS" & SG_UF_PROPRIEDADE == "MG" ~ "INDIANÓPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BRAGANCA PAULISTA" & SG_UF_PROPRIEDADE == "SP" ~ "BRAGANÇA PAULISTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MÁGDA" & SG_UF_PROPRIEDADE == "SP" ~ "MAGDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRIGÜI" & SG_UF_PROPRIEDADE == "SP" ~ "BIRIGUI",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARACOIABA DA SERRA" & SG_UF_PROPRIEDADE == "SP" ~ "ARAÇOIABA DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MOGI GUACU" & SG_UF_PROPRIEDADE == "SP" ~ "MOGI GUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "VARZEA DA PALMA" & SG_UF_PROPRIEDADE == "MG" ~ "VÁRZEA DA PALMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA VICOSA" & SG_UF_PROPRIEDADE == "BA" ~ "NOVA VIÇOSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CACADOR" & SG_UF_PROPRIEDADE == "SC" ~ "CAÇADOR",
                                    NM_MUNICIPIO_PROPRIEDADE == "BAIRRO LIMOEIRO" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "AMAMBAÍ" & SG_UF_PROPRIEDADE == "MS" ~ "AMAMBAI",
                                    NM_MUNICIPIO_PROPRIEDADE == "INDÁPOLIS" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA CANDELÁRIA" & SG_UF_PROPRIEDADE == "PR" ~ "BANDEIRANTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SEDE ALVORADA" & SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOÃO D'OESTE" & SG_UF_PROPRIEDADE == "PR" ~ "CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURO PRETO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAQUARUNA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO CLEMENTE" & SG_UF_PROPRIEDADE == "PR" ~ "SANTA HELENA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FLORÍNIA" & SG_UF_PROPRIEDADE == "SP" ~ "FLORÍNEA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIRAPÓ" & SG_UF_PROPRIEDADE == "PR" ~ "APUCARANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "LERROVILLE" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "DEZ DE MAIO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAETANO MENDES" & SG_UF_PROPRIEDADE == "PR" ~ "TIBAGI",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DOESTE" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA ROXA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DO OESTE" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA ROXA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MALU" & SG_UF_PROPRIEDADE == "PR" ~ "TERRA BOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SELVA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOA VISTA" & SG_UF_PROPRIEDADE == "PR" ~ "PRESIDENTE CASTELO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ DO PARAITINGA" & SG_UF_PROPRIEDADE == "SP" ~ "SÃO LUÍS DO PARAITINGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "RESTINGA SECA" & SG_UF_PROPRIEDADE == "RS" ~ "SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITAÓCA" & SG_UF_PROPRIEDADE == "SP" ~ "ITAOCA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITABOA" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "APIAÍ-MIRIM" & SG_UF_PROPRIEDADE == "SP" ~ "CAPÃO BONITO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA LÚCIA DO PIAÍ" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SILVEIRA" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO JOSÉ DOS AUSENTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAZENDA SOUZA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAIQUERÊ" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DA APARECIDA" & SG_UF_PROPRIEDADE == "PR" ~ "ROLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA SÃO JOÃO" & SG_UF_PROPRIEDADE == "RS" ~ "CRUZ ALTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA IPIRANGA" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COMANDAI" & SG_UF_PROPRIEDADE == "RS" ~ "SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "FLUVIÓPOLIS" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO MATEUS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "HOLAMBRA II" & SG_UF_PROPRIEDADE == "SP" ~ "HOLAMBRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FELPUDO" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMPINA DE FORA" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO BRANCO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTANA DO LIVRAMENTO" & SG_UF_PROPRIEDADE == "RS" ~ "SANT'ANA DO LIVRAMENTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITAHUM" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "LUIS EDUARDO MAGALHAES" & SG_UF_PROPRIEDADE == "BA" ~ "LUÍS EDUARDO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "CHAPADAO DO SUL" & SG_UF_PROPRIEDADE == "MS" ~ "CHAPADÃO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "GOIANAPOLIS" & SG_UF_PROPRIEDADE == "GO" ~ "GOIANÁPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMAQUA" & SG_UF_PROPRIEDADE == "RS" ~ "CAMAQUÃ",
                                    NM_MUNICIPIO_PROPRIEDADE == "UNIAO DA VITORIA" & SG_UF_PROPRIEDADE == "PR" ~ "UNIÃO DA VITÓRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO HORIZONTE" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARGARIDA" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "IGUIPORÃ" & SG_UF_PROPRIEDADE == "PR" ~ "MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "JUTÍ" & SG_UF_PROPRIEDADE == "MS" ~ "JUTI",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUARAVERA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "AMANDINA" & SG_UF_PROPRIEDADE == "MS" ~ "IVINHEMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PÉROLA INDEPENDENTE" & SG_UF_PROPRIEDADE == "PR" ~ "MARIPÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ENTRE RIOS" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LOURENÇO" & SG_UF_PROPRIEDADE == "PR" ~ "CIANORTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA LOURDES" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO JOÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA SOCORRO" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTO ANTÔNIO DO RIO VERDE" & SG_UF_PROPRIEDADE == "GO" ~ "CATALÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ DAS LARANJEIRAS" & SG_UF_PROPRIEDADE == "SP" ~ "MARACAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIRACÁ" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO SARANDI" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TELEMACO BORBA" & SG_UF_PROPRIEDADE == "PR" ~ "TELÊMACO BORBA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SAO JOSE DOS PINHAIS" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO JOSÉ DOS PINHAIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRIÚVA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA CECILIA" & SG_UF_PROPRIEDADE == "SC" ~ "SANTA CECÍLIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA VITÓRIA" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PALMEIRINHA" & SG_UF_PROPRIEDADE == "PR" ~ "GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPANÉ" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "FERREIRA" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "UVAIA" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ DA RESERVA" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA CRUZ DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ALTO DO AMPARO" & SG_UF_PROPRIEDADE == "PR" ~ "TIBAGI",
                                    NM_MUNICIPIO_PROPRIEDADE == "JANSEN" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA SARDENHA" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GRAMADINHO" & SG_UF_PROPRIEDADE == "SP" ~ "ITAPETININGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIPORÃ" & SG_UF_PROPRIEDADE == "PR" ~ "CAFEZAL DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "LAGEADO DE ARAÇAÍBA" & SG_UF_PROPRIEDADE == "SP" ~ "APIAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ÁGUAS CLARAS" & SG_UF_PROPRIEDADE == "RS" ~ "VIAMÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "MONTE ALVERNE" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA CRUZ DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ENGENHEIRO MAIA" & SG_UF_PROPRIEDADE == "SP" ~ "ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CERRO DO ROQUE" & SG_UF_PROPRIEDADE == "RS" ~ "BUTIÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUARAGI" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÍTIO GRANDE" & SG_UF_PROPRIEDADE == "BA" ~ "SÃO DESIDÉRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAJUVIRA" & SG_UF_PROPRIEDADE == "PR" ~ "ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO SOBRADINHO" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "WESTFALIA" & SG_UF_PROPRIEDADE == "RS" ~ "WESTFÁLIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURILÂNDIA" & SG_UF_PROPRIEDADE == "PR" ~ "BARBOSA FERRAZ",
                                    NM_MUNICIPIO_PROPRIEDADE == "BATEIAS" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PERICÓ" & SG_UF_PROPRIEDADE == "SC" ~ "SÃO JOAQUIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA IZABEL" & SG_UF_PROPRIEDADE == "SC" ~ "SÃO JOAQUIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAPINAS" & SG_UF_PROPRIEDADE == "SP" ~ "ITÁPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "DORIZON" & SG_UF_PROPRIEDADE == "PR" ~ "MALLET",
                                    NM_MUNICIPIO_PROPRIEDADE == "VALE DOS VINHEDOS" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "FARIA LEMOS" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA MILANO" & SG_UF_PROPRIEDADE == "RS" ~ "FARROUPILHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VICENTINÓPOLIS" & SG_UF_PROPRIEDADE == "SP" ~ "SANTO ANTÔNIO DO ARACANGUÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "IPOMÉIA" & SG_UF_PROPRIEDADE == "SC" ~ "RIO DAS ANTAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "PALMITÓPOLIS" & SG_UF_PROPRIEDADE == "PR" ~ "NOVA AURORA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PASSO DO VERDE" & SG_UF_PROPRIEDADE == "RS" ~ "SANTA MARIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "LUIS EDUARDO MAGALHÃES" & SG_UF_PROPRIEDADE == "BA" ~ "LUÍS EDUARDO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "JORDÃO" & SG_UF_PROPRIEDADE == "PR" ~ "FOZ DO JORDÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA SECA" & SG_UF_PROPRIEDADE == "RS" ~ "CAXIAS DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARARANGUA" & SG_UF_PROPRIEDADE == "SC" ~ "ARARANGUÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "VENANCIO AIRES" & SG_UF_PROPRIEDADE == "RS" ~ "VENÂNCIO AIRES",
                                    NM_MUNICIPIO_PROPRIEDADE == "RIBEIRAO PRETO" & SG_UF_PROPRIEDADE == "SP" ~ "RIBEIRÃO PRETO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SERTAOZINHO" & SG_UF_PROPRIEDADE == "SP" ~ "SERTÃOZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "JACAREI" & SG_UF_PROPRIEDADE == "SP" ~ "JACAREÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRISPIM JAQUES" & SG_UF_PROPRIEDADE == "MG" ~ "TEÓFILO OTONI",
                                    NM_MUNICIPIO_PROPRIEDADE == "VESPASIANO CORREA" & SG_UF_PROPRIEDADE == "RS" ~ "VESPASIANO CORRÊA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COUTO DE MAGALHÃES" & SG_UF_PROPRIEDADE == "TO" ~ "COUTO MAGALHÃES",
                                    NM_MUNICIPIO_PROPRIEDADE == "PEDRO LUSTOSA" & SG_UF_PROPRIEDADE == "PR" ~ "RESERVA DO IGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAÇAÍBA" & SG_UF_PROPRIEDADE == "SP" ~ "APIAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PRIMAVERA" & SG_UF_PROPRIEDADE == "SP" ~ "ROSANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TUBARAO" & SG_UF_PROPRIEDADE == "SC" ~ "TUBARÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ICARA" & SG_UF_PROPRIEDADE == "SC" ~ "IÇARA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PÓLO PETROQUÍMICO DE TRIUNFO" & SG_UF_PROPRIEDADE == "RS" ~ "TRIUNFO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAINA" & SG_UF_PROPRIEDADE == "PR" ~ "PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO VALÉRIO DA NATIVIDADE" & SG_UF_PROPRIEDADE == "TO" ~ "SÃO VALÉRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAÍNA DO SUL" & SG_UF_PROPRIEDADE == "SC" ~ "BOCAINA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRITIBA-MIRIM" & SG_UF_PROPRIEDADE == "SP" ~ "BIRITIBA MIRIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "ESPIGÃO DO OESTE" & SG_UF_PROPRIEDADE == "RO" ~ "ESPIGÃO D'OESTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "DUERÊ" & SG_UF_PROPRIEDADE == "TO" ~ "DUERÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PARANAITÁ" & SG_UF_PROPRIEDADE == "MT" ~ "PARANAÍTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA OLIVA" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO FRANCISCO DE PAULA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BARRO VERMELHO" & SG_UF_PROPRIEDADE == "RS" ~ "CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO PEDRO" & SG_UF_PROPRIEDADE == "PR" ~ "SÃO PEDRO DO PARANÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO PEDRO" & SG_UF_PROPRIEDADE == "RS" ~ "SÃO PEDRO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "TUIUTI" & SG_UF_PROPRIEDADE == "RS" ~ "BENTO GONÇALVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "RETIRO GRANDE" & SG_UF_PROPRIEDADE == "PR" ~ "CAMPO LARGO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA NOVA" & SG_UF_PROPRIEDADE == "PR" ~ "TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GLÓRIA" & SG_UF_PROPRIEDADE == "RS" ~ "PORTO ALEGRE",
                                    NM_MUNICIPIO_PROPRIEDADE == "CARIPARE" & SG_UF_PROPRIEDADE == "BA" ~ "RIACHÃO DAS NEVES",
                                    NM_MUNICIPIO_PROPRIEDADE == "BELA VISTA" & SG_UF_PROPRIEDADE == "RS" ~ "PORTO ALEGRE",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARAVILHA" & SG_UF_PROPRIEDADE == "PR" ~ "LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAIM" & SG_UF_PROPRIEDADE == "RS" ~ "RIO GRANDE",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO MARTINHO" & SG_UF_PROPRIEDADE == "PR" ~ "ROLÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PATROCÍNIO DE CARATINGA" & SG_UF_PROPRIEDADE == "MG" ~ "CARATINGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PORTO VILMA" & SG_UF_PROPRIEDADE == "MS" ~ "DEODÁPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BONFIM DA FEIRA" & SG_UF_PROPRIEDADE == "BA" ~ "FEIRA DE SANTANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "RODA VELHA" & SG_UF_PROPRIEDADE == "BA" ~ "SÃO DESIDÉRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PONTÕES" & SG_UF_PROPRIEDADE == "ES" ~ "AFONSO CLÁUDIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "OUROANA" & SG_UF_PROPRIEDADE == "GO" ~ "RIO VERDE",
                                    NM_MUNICIPIO_PROPRIEDADE == "TREVO DO JOSÉ ROSÁRIO" & SG_UF_PROPRIEDADE == "GO" ~ "LEOPOLDO DE BULHÕES",
                                    NM_MUNICIPIO_PROPRIEDADE == "PINDARÉ MIRIM" & SG_UF_PROPRIEDADE == "MA" ~ "PINDARÉ-MIRIM",
                                    NM_MUNICIPIO_PROPRIEDADE == "AMPARO DA SERRA" & SG_UF_PROPRIEDADE == "MG" ~ "AMPARO DO SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GRACCHO CARDOSO" & SG_UF_PROPRIEDADE == "SE" ~ "GRACHO CARDOSO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA MARIA" & SG_UF_PROPRIEDADE == "SC" ~ "LAGES",
                                    NM_MUNICIPIO_PROPRIEDADE == "OTÁVIO ROCHA" & SG_UF_PROPRIEDADE == "RS" ~ "FLORES DA CUNHA",
                                    NM_MUNICIPIO_PROPRIEDADE == "APARECIDA DE MINAS" & SG_UF_PROPRIEDADE == "MG" ~ "FRUTAL",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOA VISTA DE SANTA MARIA" & SG_UF_PROPRIEDADE == "MG" ~ "UNA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIPAVA" & SG_UF_PROPRIEDADE == "MG" ~ "PARAGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "JACARANDIRA" & SG_UF_PROPRIEDADE == "MG" ~ "RESENDE COSTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SARANDIRA" & SG_UF_PROPRIEDADE == "MG" ~ "JUIZ DE FORA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VEREDAS" & SG_UF_PROPRIEDADE == "MG" ~ "JOÃO PINHEIRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAJÁ" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRUZALTINA" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAÇU" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "MONTESE" & SG_UF_PROPRIEDADE == "MS" ~ "ITAPORÃ",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA FORMOSA" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA VARGAS" & SG_UF_PROPRIEDADE == "MS" ~ "DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BARREIRAS" & SG_UF_PROPRIEDADE == "PA" ~ "SANTA MARIA DAS BARREIRAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "ÁGUA BOA"& SG_UF_PROPRIEDADE == "PR"~"PAIÇANDU",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO CAMILO"& SG_UF_PROPRIEDADE =="PR"~"PALOTINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOAQUIM"& SG_UF_PROPRIEDADE =="PR"~"ARAPONGAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CONSELHEIRO ZACARIAS"& SG_UF_PROPRIEDADE =="PR"~"SANTO ANTÔNIO DA PLATINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TORRINHAS"& SG_UF_PROPRIEDADE =="RS"~"PINHEIRO MACHADO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAINS"& SG_UF_PROPRIEDADE =="RS"~"SANTA MARIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "TAQUARA VERDE"& SG_UF_PROPRIEDADE =="SC"~"CAÇADOR",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA JORDÃOZINHO"& SG_UF_PROPRIEDADE == "PR"~"GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ENCANTADO D'OESTE"& SG_UF_PROPRIEDADE == "PR"~"ASSIS CHATEAUBRIAND",
                                    NM_MUNICIPIO_PROPRIEDADE == "VISTA ALEGRE"& SG_UF_PROPRIEDADE == "PR"~"CORONEL VIVIDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VISTA ALEGRE"& SG_UF_PROPRIEDADE == "PR"~"ENÉAS MARQUES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTO ANTÔNIO DO PALMITAL"& SG_UF_PROPRIEDADE == "PR"~"RIO BOM",
                                    NM_MUNICIPIO_PROPRIEDADE == "UBAUNA"& SG_UF_PROPRIEDADE == "PR"~"SÃO JOÃO DO IVAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOA VISTA DE SANTA MARIA"& SG_UF_PROPRIEDADE == "MG"~"UNAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "APARECIDA DO OESTE"& SG_UF_PROPRIEDADE == "PR"~"TUNEIRAS D'OESTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARIZA"& SG_UF_PROPRIEDADE == "PR"~"SÃO PEDRO DO IVAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO FRANCISCO"& SG_UF_PROPRIEDADE == "PR"~"CHOPINZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAPORÉ"& SG_UF_PROPRIEDADE == "PR"~"GUARANIAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA FÉ DO PIRAPÓ"& SG_UF_PROPRIEDADE == "PR"~"MARIALVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "JARDIM"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ DO OESTE"& SG_UF_PROPRIEDADE == "PR"~"TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CONCÓRDIA DO OESTE"& SG_UF_PROPRIEDADE == "PR"~"TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "JUCIARA"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAMPO SECO"& SG_UF_PROPRIEDADE == "RS"~"ROSÁRIO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "RIO TOLDO"& SG_UF_PROPRIEDADE == "RS"~"GETÚLIO VARGAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "CASCATA"& SG_UF_PROPRIEDADE == "RS"~"HORIZONTINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CASCATA"& SG_UF_PROPRIEDADE == "RS"~"PELOTAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOM PROGRESSO"& SG_UF_PROPRIEDADE == "PR"~"SABÁUDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITORORÓ DO PARANAPANEMA"& SG_UF_PROPRIEDADE == "SP"~"PIRAPOZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIQUIRIVAÍ"& SG_UF_PROPRIEDADE == "PR"~"CAMPO MOURÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CONGONHAS"& SG_UF_PROPRIEDADE == "PR"~"CORNÉLIO PROCÓPIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CALÓGERAS"& SG_UF_PROPRIEDADE == "PR"~"ARAPOTI",
                                    NM_MUNICIPIO_PROPRIEDADE == "BONFIM PAULISTA"& SG_UF_PROPRIEDADE == "SP"~"RIBEIRÃO PRETO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TRIÂNGULO"& SG_UF_PROPRIEDADE == "PR"~"ENGENHEIRO BELTRÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA GANDHI"& SG_UF_PROPRIEDADE == "PR"~"PRIMEIRO DE MAIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GEREMIA LUNARDELLI"& SG_UF_PROPRIEDADE == "PR"~"NOVA CANTU",
                                    NM_MUNICIPIO_PROPRIEDADE == "MONTESE"& SG_UF_PROPRIEDADE == "MS"~"ITAPORÃ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PORTEIRA PRETA"& SG_UF_PROPRIEDADE == "PR"~"FÊNIX",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA APARECIDA"& SG_UF_PROPRIEDADE == "PR"~"ANDIRÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CLARINIA"& SG_UF_PROPRIEDADE == "SP"~"SANTA CRUZ DO RIO PARDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GAMADINHO"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BOCAJÁ"& SG_UF_PROPRIEDADE == "MS"~"DOURADINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ESPERANÇA DO NORTE"& SG_UF_PROPRIEDADE == "PR"~"ALVORADA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPÃO GRANDE"& SG_UF_PROPRIEDADE == "RS"~"MUITOS CAPÕES",
                                    NM_MUNICIPIO_PROPRIEDADE == "PINHEIRO MARCADO"& SG_UF_PROPRIEDADE == "RS"~"CARAZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRUZALTINA"& SG_UF_PROPRIEDADE == "MS"~"DOURADINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ROMEÓPOLIS"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MOSTARDAS"& SG_UF_PROPRIEDADE == "SP"~"MONTE ALEGRE DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "BACURITI"& SG_UF_PROPRIEDADE == "SP"~"CAFELÂNDIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BELA VISTA DO PIQUIRI"& SG_UF_PROPRIEDADE == "PR"~"CAMPINA DA LAGOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VIDA NOVA"& SG_UF_PROPRIEDADE == "PR"~"SAPOPEMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "VIDIGAL"& SG_UF_PROPRIEDADE == "PR"~"CIANORTE",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTO ANTÔNIO DO PARANAPANEMA"& SG_UF_PROPRIEDADE == "SP"~"CÂNDIDO MOTA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO BENTO"& SG_UF_PROPRIEDADE == "RS"~"CARAZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO BENTO"& SG_UF_PROPRIEDADE == "RS"~"PALMEIRA DAS MISSÕES",
                                    NM_MUNICIPIO_PROPRIEDADE == "PULINÓPOLIS"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SERRINHA"& SG_UF_PROPRIEDADE == "PR"~"CONTENDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRISTO REI"& SG_UF_PROPRIEDADE == "PR"~"CAPANEMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CRISTO REI"& SG_UF_PROPRIEDADE == "PR"~"PARANAVAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "ESPIGÃO"& SG_UF_PROPRIEDADE == "SP"~"REGENTE FEIJÓ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAU D'ALHO DO SUL"& SG_UF_PROPRIEDADE == "PR"~"ASSAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "FREI TIMÓTEO"& SG_UF_PROPRIEDADE == "PR"~"JATAIZINHO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VALÉRIO"& SG_UF_PROPRIEDADE == "PR"~"PLANALTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PORTO MENDES"& SG_UF_PROPRIEDADE == "PR"~"MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "TEREZA BREDA"& SG_UF_PROPRIEDADE == "PR"~"BARBOSA FERRAZ",
                                    NM_MUNICIPIO_PROPRIEDADE == "LAGOA BRANCA"& SG_UF_PROPRIEDADE == "SP"~"CASA BRANCA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAXINAL"& SG_UF_PROPRIEDADE == "RS"~"BOA VISTA DO CADEADO",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAXINAL"& SG_UF_PROPRIEDADE == "RS"~"SÃO LOURENÇO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "FAXINAL"& SG_UF_PROPRIEDADE == "RS"~"VICTOR GRAEFF",
                                    NM_MUNICIPIO_PROPRIEDADE == "JURUPEMA"& SG_UF_PROPRIEDADE == "SP"~"TAQUARITINGA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA CASTROLÂNDA"& SG_UF_PROPRIEDADE == "PR"~"CASTRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "JACIPORÃ"& SG_UF_PROPRIEDADE == "SP"~"DRACENA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA PÁTRIA"& SG_UF_PROPRIEDADE == "SP"~"PRESIDENTE BERNARDES",
                                    NM_MUNICIPIO_PROPRIEDADE == "ABAPÃ"& SG_UF_PROPRIEDADE == "PR"~"CASTRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "APARECIDA DE MINAS"& SG_UF_PROPRIEDADE == "MG"~"FRUTAL",
                                    NM_MUNICIPIO_PROPRIEDADE == "DOIS IRMÃOS"& SG_UF_PROPRIEDADE == "PR"~"SÃO JOÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "DOIS IRMÃOS"& SG_UF_PROPRIEDADE == "PR"~"TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BRAGANTINA"& SG_UF_PROPRIEDADE == "PR"~"ASSIS CHATEAUBRIAND",
                                    NM_MUNICIPIO_PROPRIEDADE == "ITAPUCÁ"& SG_UF_PROPRIEDADE == "RS"~"ITAPUCA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPÃO DA PORTEIRA"& SG_UF_PROPRIEDADE == "RS"~"VIAMÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VEREDAS"& SG_UF_PROPRIEDADE == "MG"~"JOÃO PINHEIRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BASÍLIO"& SG_UF_PROPRIEDADE == "RS"~"HERVAL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SARANDIRA"& SG_UF_PROPRIEDADE == "MG"~"JUIZ DE FORA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PARANÁ D'OESTE"& SG_UF_PROPRIEDADE == "PR"~"MOREIRA SALES",
                                    NM_MUNICIPIO_PROPRIEDADE == "XAXIM"& SG_UF_PROPRIEDADE == "PR"~"TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA VARGAS"& SG_UF_PROPRIEDADE == "MS"~"DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO MIGUEL"& SG_UF_PROPRIEDADE == "PR"~"TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PAULISTÂNIA"& SG_UF_PROPRIEDADE == "PR"~"ALTO PIQUIRI",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA RITA DA FLORESTA"& SG_UF_PROPRIEDADE == "RJ"~"CANTAGALO",
                                    NM_MUNICIPIO_PROPRIEDADE == "IPIABAS"& SG_UF_PROPRIEDADE == "RJ"~"BARRA DO PIRAÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIRIQUITOS"& SG_UF_PROPRIEDADE == "PR"~"PONTA GROSSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "MAUÁ"& SG_UF_PROPRIEDADE == "RS"~"ARROIO GRANDE",
                                    NM_MUNICIPIO_PROPRIEDADE == "MAUÁ"& SG_UF_PROPRIEDADE == "RS"~"IJUÍ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CARAJÁ"& SG_UF_PROPRIEDADE == "PR"~"JESUÍTAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "TURIBA DO SUL"& SG_UF_PROPRIEDADE == "SP"~"ITABERÁ",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA CRUZ DO TIMBÓ"& SG_UF_PROPRIEDADE == "SC"~"PORTO UNIÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "HERVEIRA"& SG_UF_PROPRIEDADE == "PR"~"CAMPINA DA LAGOA",
                                    NM_MUNICIPIO_PROPRIEDADE == "FORMIGUEIRO"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO LUIZ DO PURUNÃ"& SG_UF_PROPRIEDADE == "PR"~"BALSA NOVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PEDREIRAS"& SG_UF_PROPRIEDADE == "RS"~"ARROIO GRANDE",
                                    NM_MUNICIPIO_PROPRIEDADE == "BEXIGA"& SG_UF_PROPRIEDADE == "RS"~"RIO PARDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PASSO DAS PEDRAS"& SG_UF_PROPRIEDADE == "RS"~"CAPÃO DO LEÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "TOLEDO"& SG_UF_PROPRIEDADE == "SP"~"PEDRO DE TOLEDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BIRITIBA-USSU"& SG_UF_PROPRIEDADE == "SP"~"MOGI DAS CRUZES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA ESMERALDA"& SG_UF_PROPRIEDADE == "PR"~"SANTA CRUZ DE MONTE CASTELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PANEMA"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "AQUIDABAN"& SG_UF_PROPRIEDADE == "PR"~"MARIALVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVO TRÊS PASSOS"& SG_UF_PROPRIEDADE == "PR"~"MARECHAL CÂNDIDO RONDON",
                                    NM_MUNICIPIO_PROPRIEDADE == "RIO DO SALTO"& SG_UF_PROPRIEDADE == "PR"~"CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "OURO VERDE DO PIQUIRI"& SG_UF_PROPRIEDADE == "PR"~"CORBÉLIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CAPINZAL"& SG_UF_PROPRIEDADE == "PR"~"ARAUCÁRIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO GABRIEL"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "RIO DAS ANTAS"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "DOUTOR OLIVEIRA CASTRO"& SG_UF_PROPRIEDADE == "PR"~"GUAÍRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "ARAPUAN"& SG_UF_PROPRIEDADE == "PR"~"JANIÓPOLIS",
                                    NM_MUNICIPIO_PROPRIEDADE == "BARREIRAS"& SG_UF_PROPRIEDADE == "PA"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOVA CARDOSO"& SG_UF_PROPRIEDADE == "SP"~"ITAJOBI",
                                    NM_MUNICIPIO_PROPRIEDADE == "JUVINÓPOLIS"& SG_UF_PROPRIEDADE == "PR"~"CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "IBIACI"& SG_UF_PROPRIEDADE == "PR"~"PRIMEIRO DE MAIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA MELISSA"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA ESPERANÇA"& SG_UF_PROPRIEDADE == "PR"~"ARAPONGAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "ESPÍRITO SANTO"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COROADOS"& SG_UF_PROPRIEDADE == "RS"~"SÃO VALÉRIO DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "RINCÃO DOS MENDES"& SG_UF_PROPRIEDADE == "RS"~"SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "BURITI"& SG_UF_PROPRIEDADE == "RS"~"SANTO ÂNGELO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAIPAVA"& SG_UF_PROPRIEDADE == "MG"~"PARAGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "ALTO SANTA FÉ"& SG_UF_PROPRIEDADE == "PR"~"NOVA SANTA ROSA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SAPIRANGA"& SG_UF_PROPRIEDADE == "SC"~"MELEIRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PULADOR"& SG_UF_PROPRIEDADE == "RS"~"PASSO FUNDO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PULADOR"& SG_UF_PROPRIEDADE == "RS"~"UNIÃO DA SERRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "NOSSA SENHORA DE CARAVAGGIO"& SG_UF_PROPRIEDADE == "SC"~"NOVA VENEZA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BELA VISTA DO SUL"& SG_UF_PROPRIEDADE == "SC"~"MAFRA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CORDILHEIRA"& SG_UF_PROPRIEDADE == "RS"~"CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO SEBASTIÃO"& SG_UF_PROPRIEDADE == "RS"~"ESMERALDA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO SEBASTIÃO"& SG_UF_PROPRIEDADE == "RS"~"IBIRAIARAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUARIZINHO"& SG_UF_PROPRIEDADE == "SP"~"ITAPEVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "JANGADA DO SUL"& SG_UF_PROPRIEDADE == "PR"~"GENERAL CARNEIRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "ALTANEIRA"& SG_UF_PROPRIEDADE == "PR"~"MANDAGUAÇU",
                                    NM_MUNICIPIO_PROPRIEDADE == "IVAILÂNDIA"& SG_UF_PROPRIEDADE == "PR"~"ENGENHEIRO BELTRÃO",
                                    NM_MUNICIPIO_PROPRIEDADE == "GUAÇU"& SG_UF_PROPRIEDADE == "MS"~"DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "PITANGUI"& SG_UF_PROPRIEDADE == "PR"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "CORREIA DE FREITAS"& SG_UF_PROPRIEDADE == "PR"~"APUCARANA",
                                    NM_MUNICIPIO_PROPRIEDADE == "IRERÊ"& SG_UF_PROPRIEDADE == "PR"~"LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "WARTA"& SG_UF_PROPRIEDADE == "PR"~"LONDRINA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA CENTENÁRIO"& SG_UF_PROPRIEDADE == "PR"~"CASCAVEL",
                                    NM_MUNICIPIO_PROPRIEDADE == "BARRO PRETO"& SG_UF_PROPRIEDADE == "PR"~"VENTANIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA CACHOEIRA"& SG_UF_PROPRIEDADE == "PR"~"GUARAPUAVA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SANTA CRUZ DA ESTRELA"& SG_UF_PROPRIEDADE == "SP"~"SANTA RITA DO PASSA QUATRO",
                                    NM_MUNICIPIO_PROPRIEDADE == "COLÔNIA Z-3"& SG_UF_PROPRIEDADE == "RS"~"PELOTAS",
                                    NM_MUNICIPIO_PROPRIEDADE == "PIQUIRI"& SG_UF_PROPRIEDADE == "RS"~"CACHOEIRA DO SUL",
                                    NM_MUNICIPIO_PROPRIEDADE == "PASSINHOS"& SG_UF_PROPRIEDADE == "RS"~"OSÓRIO",
                                    NM_MUNICIPIO_PROPRIEDADE == "PEDRA BRANCA DE ITARARÉ"& SG_UF_PROPRIEDADE == "SP"~"ITARARÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "CEDRO"& SG_UF_PROPRIEDADE == "PR"~"PEROBAL",
                                    NM_MUNICIPIO_PROPRIEDADE == "VILA FORMOSA"& SG_UF_PROPRIEDADE == "MS"~"DOURADOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"ALTO ALEGRE",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"MONTE ALEGRE DOS CAMPOS",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"PLANALTO",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"SANTO ANTÔNIO DAS MISSÕES",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"SANTO ANTÔNIO DO PALMA",
                                    NM_MUNICIPIO_PROPRIEDADE == "SÃO JOSÉ"& SG_UF_PROPRIEDADE == "RS"~"SÃO MIGUEL DAS MISSÕES",
                                    NM_MUNICIPIO_PROPRIEDADE == "VÁRZEA"& SG_UF_PROPRIEDADE == "RS"~"PANTANO GRANDE",
                                    NM_MUNICIPIO_PROPRIEDADE == "VÁRZEA"& SG_UF_PROPRIEDADE == "RS"~"SÃO JOSÉ DOS AUSENTES",
                                    NM_MUNICIPIO_PROPRIEDADE == "ROLÂNDIA"& SG_UF_PROPRIEDADE == "ES"~"NA",
                                    NM_MUNICIPIO_PROPRIEDADE == "BATATUBA"& SG_UF_PROPRIEDADE == "SP"~"PIRACAIA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PRATA"& SG_UF_PROPRIEDADE == "PR"~"CAMBÉ",
                                    NM_MUNICIPIO_PROPRIEDADE == "MARAJÓ"& SG_UF_PROPRIEDADE == "PR"~"NOVA AURORA",
                                    NM_MUNICIPIO_PROPRIEDADE == "PARANAGI"& SG_UF_PROPRIEDADE == "PR"~"SERTANEJA",
                                    TRUE ~ NM_MUNICIPIO_PROPRIEDADE)
                                )


#Adicionando código Município e Microrregião
data_PSR <- data_PSR %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("SG_UF_PROPRIEDADE" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
  )

#Padronizando Produtos
data_PSR <- data_PSR %>% 
  left_join(
    produtos_padronizados %>% select(PSR,Produto_Padronizado), 
    by = c("NM_CULTURA_GLOBAL" = "PSR")
  )

#Dropando Colunas não utilizadas de MUNICIPIO
data_PSR$NM_MUNICIPIO_PROPRIEDADE <- NULL
data_PSR$NM_CULTURA_GLOBAL <- NULL
data_PSR <- data_PSR %>% rename(SAFRA = AN_SAFRA)


#Adicionando  Prêmio por área
data_PSR$Premio_Area <- data_PSR$VL_PREMIO_LIQUIDO/data_PSR$NR_AREA_TOTAL



#################################################### Proagro ####################################################


data_Proagro <- read_excel("Dados/Raw/CONTRATACAO PROAGRO 20190801.xlsx")
data_Proagro <- data_Proagro[!data_Proagro$MUNICIPIO=="Não informado",]
data_Proagro <- data_Proagro %>% mutate(MUNICIPIO_CORRIGIDO = 
                                  case_when( 
                                    MUNICIPIO == "BIRITIBA-MIRIM" & UF == "SP" ~ "BIRITIBA MIRIM",
                                    MUNICIPIO == "MOJI MIRIM" & UF == "SP" ~ "MOGI MIRIM",
                                    MUNICIPIO == "RESTINGA SECA" & UF == "RS" ~ "SANTO ÂNGELO",
                                    MUNICIPIO == "FLORÍNIA" & UF == "SP" ~ "FLORÍNEA",
                                    MUNICIPIO == "LAURO MULLER" & UF == "SC" ~ "LAURO MÜLLER",
                                    MUNICIPIO == "SÃO CRISTOVÃO DO SUL" & UF == "SC" ~ "SÃO CRISTÓVÃO DO SUL",
                                    MUNICIPIO == "VESPASIANO CORREA" & UF == "RS" ~ "VESPASIANO CORRÊA",
                                    MUNICIPIO == "ATILIO VIVACQUA" & UF == "ES" ~ "ATÍLIO VIVACQUA",
                                    MUNICIPIO == "SÃO VICENTE FERRER" & UF == "PE" ~ "SÃO VICENTE FÉRRER",
                                    MUNICIPIO == "WESTFALIA" & UF == "RS" ~ "WESTFÁLIA",
                                    MUNICIPIO == "SÃO LUÍZ DO NORTE" & UF == "GO" ~ "SÃO LUIZ DO NORTE",
                                    MUNICIPIO == "POXORÉO" & UF == "MT" ~ "POXORÉU",
                                    MUNICIPIO == "BRASILIA (BRAZLANDIA)" & UF == "DF" ~ "BRASÍLIA",
                                    MUNICIPIO == "BRASILIA (PLANALTINA)" & UF == "DF" ~ "BRASÍLIA",
                                    MUNICIPIO == "BRASILIA (PARANOA)" & UF == "DF" ~ "BRASÍLIA",
                                    TRUE ~ MUNICIPIO))


#Adicionando código Município e Microrregião
data_Proagro <- data_Proagro %>% 
  left_join(
    municipios %>% select(Sigla_UF,Nome_Município_Maiuscula,Cod_Microrregiao,Cod_Municipio),
    by =c("UF" = "Sigla_UF", "MUNICIPIO_CORRIGIDO" = "Nome_Município_Maiuscula")
  )

#Padronizando Produtos
data_Proagro <- data_Proagro %>% 
  left_join(
    produtos_padronizados %>% select(Proagro,Produto_Padronizado), 
    by =c("PRODUTO" = "Proagro")
  )


#Dropando Colunas não utilizadas de MUNICIPIO
data_Proagro$MUNICIPIO <- NULL
data_Proagro$PRODUTO <- NULL






#################################################### Inflação #################################################### 
inflacao <- data.frame(jul2014 = 6.5, jul2015 = 9.56, jul2016 = 8.74, jul2017=2.71, jul2018=4.48, jul2019=3.22)




#################################################### CENSO RURAL #################################################### 

# Tabela 6615 - Número de estabelecimentos por produtos da lavoura temporária - resultados preliminares 2017
data_Censo_6615 <- read.csv("./Dados/Raw/Censo - 2017/tabela6615.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
data_Censo_6615[data_Censo_6615=="-"]<-0
data_Censo_6615[data_Censo_6615 == "X"] <- NA


# Tabela 6616 - Número de estabelecimentos agropecuários e Número de pés existentes, por produtos da lavoura permanente - resultados preliminares 2017
data_Censo_6616 <- read.csv("./Dados/Raw/Censo - 2017/tabela6616.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
data_Censo_6616[data_Censo_6616=="-"]<-0
data_Censo_6616[data_Censo_6616=="X"]<-NA


# Tabela 6618 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos da agroindústria rural - resultados preliminares 2017
data_Censo_6618 <- read.csv("./Dados/Raw/Censo - 2017/tabela6618.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
data_Censo_6618[data_Censo_6618=="-"]<-0
data_Censo_6618[data_Censo_6618=="X"]<-NA


# Tabela 6619 - Número de estabelecimentos agropecuários e Quantidade produzida, por produtos da horticultura - resultados preliminares 2017
data_Censo_6619 <- read.csv("./Dados/Raw/Censo - 2017/tabela6619.csv", header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)
data_Censo_6619[data_Censo_6619=="-"]<-0
data_Censo_6619[data_Censo_6619=="X"]<-NA



######################### Dados Censo Geral ##################################

data_Censo_Geral <- NULL

#Tabela 6635 - Número de estabelecimentos agropecuários, Área dos estabelecimentos agropecuários, Área territorial total e Condição legal das terras - resultados preliminares 2017																					
data_Censo_6635 <- read_excel("Dados/Raw/Censo - 2017/tabela6635.xlsx", col_types = "text")
data_Censo_6635[data_Censo_6635=="-"]<-0
data_Censo_6635[data_Censo_6635=="X"]<-NA
data_Censo_6635[, c(3:30)] <- sapply(data_Censo_6635[, c(3:30)], as.numeric)

data_Censo_Geral <- data_Censo_6635[, -c(11,13,15,17,19,21,23,25,27,29)]
rm(data_Censo_6635)

#Tabela 6639 - Número de estabelecimentos agropecuários e Número de unidades armazenadoras e capacidade, por tipo de unidade armazenadora - resultados preliminares 2017														
data_Censo_6639 <- read_excel("Dados/Raw/Censo - 2017/tabela6639.xlsx")
data_Censo_6639[data_Censo_6639=="-"]<-0
data_Censo_6639[data_Censo_6639=="X"]<-NA
data_Censo_6639[, c(3:26)] <- sapply(data_Censo_6639[, c(3:26)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6639[, -c(3,5,7,9,11,13,15,17,19,21,23,25)] , by = "Cód.")
rm(data_Censo_6639)

#Tabela 6640 - Número de estabelecimentos agropecuários, Sistema de preparo do solo e Área com plantio direto na palha - resultados preliminares 2017					
data_Censo_6640 <- read_excel("Dados/Raw/Censo - 2017/tabela6640.xlsx")
data_Censo_6640[data_Censo_6640=="-"]<-0
data_Censo_6640[data_Censo_6640=="X"]<-NA
data_Censo_6640[, c(3:12)] <- sapply(data_Censo_6640[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6640[, -c(3,5,7,9,11)], by = "Cód.")
rm(data_Censo_6640)


#Tabela 6641 - Número de estabelecimentos agropecuários e Número de tratores, implementos e máquinas existentes nos estabelecimentos agropecuários - resultados preliminares 2017
data_Censo_6641 <- read_excel("Dados/Raw/Censo - 2017/tabela6641.xlsx")
data_Censo_6641[data_Censo_6641=="-"]<-0
data_Censo_6641[data_Censo_6641=="X"]<-NA
data_Censo_6641[, c(3:18)] <- sapply(data_Censo_6641[, c(3:18)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6641[, -c(3,5,7,9,11,13,15,17)], by = "Cód.")
rm(data_Censo_6641)


#Tabela 6642 - Número de estabelecimentos agropecuários e Número de veículos existentes nos estabelecimentos agropecuários - resultados preliminares 2017												
data_Censo_6642 <- read_excel("Dados/Raw/Censo - 2017/tabela6642.xlsx")
data_Censo_6642[data_Censo_6642=="-"]<-0
data_Censo_6642[data_Censo_6642=="X"]<-NA
data_Censo_6642[, c(3:22)] <- sapply(data_Censo_6642[, c(3:22)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6642[, -c(3,5,7,9,11,13,15,17,19,21)], by = "Cód.")
rm(data_Censo_6642)



######################### Ver ##################################



##Ver warnings
#Tabela 6643 - Número de estabelecimentos agropecuários por telefone, e-mail e internet - resultados preliminares 2017											
data_Censo_6643 <- read_excel("Dados/Raw/Censo - 2017/tabela6643.xlsx", col_types = "text")
data_Censo_6643[data_Censo_6643=="-"]<-0
data_Censo_6643[data_Censo_6643=="X"]<-NA
data_Censo_6643[, c(3:11)] <- sapply(data_Censo_6643[, c(3:11)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6643, by = "Cód.")
rm(data_Censo_6643)


#Tabela 6650 - Número de estabelecimentos agropecuários por forma de obtenção das terras - resultados preliminares 2017													
data_Censo_6650 <- read_excel("Dados/Raw/Censo - 2017/tabela6650.xlsx")
data_Censo_6650[data_Censo_6650=="-"]<-0
data_Censo_6650[data_Censo_6650=="X"]<-NA
data_Censo_6650[, c(3:13)] <- sapply(data_Censo_6650[, c(3:13)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6650, by = "Cód.")
rm(data_Censo_6650)


#Tabela 6710 - Número de estabelecimentos agropecuários, Área dos estabelecimentos agropecuários, por condição legal das terras, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																										
data_Censo_6710 <- read_excel("Dados/Raw/Censo - 2017/tabela6710.xlsx", col_types = "text")
data_Censo_6710[data_Censo_6710=="-"]<-0
data_Censo_6710[data_Censo_6710=="X"]<-NA
data_Censo_6710[data_Censo_6710==".."]<-NA
data_Censo_6710[data_Censo_6710=="..."]<-NA
data_Censo_6710[, c(3:26)] <- sapply(data_Censo_6710[, c(3:26)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6710, by = "Cód.")
rm(data_Censo_6710)



#Tabela 6659 - Número de estabelecimentos agropecuários por agente financeiro responsável pelo financiamento - resultados preliminares 2017
data_Censo_6659 <- read_excel("Dados/Raw/Censo - 2017/tabela6659.xlsx", col_types = "text")
data_Censo_6659[data_Censo_6659=="-"]<-0
data_Censo_6659[data_Censo_6659=="X"]<-NA
data_Censo_6659[, c(3:12)] <- sapply(data_Censo_6659[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6659, by = "Cód.")
rm(data_Censo_6659)



#Tabela 6647 - Número de estabelecimentos agropecuários por sexo, alfabetização, idade e cor ou raça do produtor - resultados preliminares 2017														
data_Censo_6647 <- read_excel("Dados/Raw/Censo - 2017/tabela6647.xlsx", col_types = "text")
data_Censo_6647[data_Censo_6647=="-"]<-0
data_Censo_6647[data_Censo_6647=="X"]<-NA
data_Censo_6647[data_Censo_6647==".."]<-NA
data_Censo_6647[data_Censo_6647=="..."]<-NA
data_Censo_6647[, c(3:14)] <- sapply(data_Censo_6647[, c(3:14)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6647, by = "Cód.")
rm(data_Censo_6647)


#Tabela 6649 - Número de estabelecimentos agropecuários por residência, finalidade da produção e DAP - resultados preliminares 2017																
data_Censo_6649 <- read_excel("Dados/Raw/Censo - 2017/tabela6649.xlsx", col_types = "text")
data_Censo_6649[data_Censo_6649=="-"]<-0
data_Censo_6649[data_Censo_6649=="X"]<-NA
data_Censo_6649[, c(3:16)] <- sapply(data_Censo_6649[, c(3:16)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6649, by = "Cód.")
rm(data_Censo_6649)


#Tabela 6652 - Número de estabelecimentos agropecuários por uso de agricultura orgânica - resultados preliminares 2017							
data_Censo_6652 <- read_excel("Dados/Raw/Censo - 2017/tabela6652.xlsx")
data_Censo_6652[data_Censo_6652=="-"]<-0
data_Censo_6652[data_Censo_6652=="X"]<-NA
data_Censo_6652[, c(3:7)] <- sapply(data_Censo_6652[, c(3:7)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6652, by = "Cód.")
rm(data_Censo_6652)


#Tabela 6655 - Número de estabelecimentos agropecuários por nascentes, rios/riachos, poços e cisternas - resultados preliminares 2017													
data_Censo_6655 <- read_excel("Dados/Raw/Censo - 2017/tabela6655.xlsx", col_types = "text")
data_Censo_6655[data_Censo_6655=="-"]<-0
data_Censo_6655[data_Censo_6655=="X"]<-NA
data_Censo_6655[, c(3:13)] <- sapply(data_Censo_6655[, c(3:13)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6655, by = "Cód.")
rm(data_Censo_6655)


#Tabela 6709 - Número de estabelecimentos agropecuários, por existência de energia elétrica, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017				
data_Censo_6709 <- read_excel("Dados/Raw/Censo - 2017/tabela6709.xlsx", col_types = "text")
data_Censo_6709[data_Censo_6709=="-"]<-0
data_Censo_6709[data_Censo_6709=="X"]<-NA
data_Censo_6709[, c(3:4)] <- sapply(data_Censo_6709[, c(3:4)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6709, by = "Cód.")
rm(data_Censo_6709)


#Tabela 6713 - Número de estabelecimentos agropecuários por contratação de serviços, tipo de prestador de serviços e dias trabalhados, sexo do produtor, escolaridade do produtor, condição legal do produtor e origem da orientação técnica recebida - resultados preliminares 2017
data_Censo_6713 <- read_excel("Dados/Raw/Censo - 2017/tabela6713.xlsx", col_types = "text")
data_Censo_6713[data_Censo_6713=="-"]<-0
data_Censo_6713[data_Censo_6713=="X"]<-NA
data_Censo_6713[data_Censo_6713==".."]<-NA
data_Censo_6713[data_Censo_6713=="..."]<-NA
data_Censo_6713[, c(3:12)] <- sapply(data_Censo_6713[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6713, by = "Cód.")
rm(data_Censo_6713)


#Tabela 6790 - Número de estabelecimentos agropecuários por classes de idade do produtor - resultados preliminares 2017																		
data_Censo_6790 <- read_excel("Dados/Raw/Censo - 2017/tabela6790.xlsx", col_types = "text")
data_Censo_6790[data_Censo_6790=="-"]<-0
data_Censo_6790[data_Censo_6790=="X"]<-NA
data_Censo_6790[, c(3:16)] <- sapply(data_Censo_6790[, c(3:16)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6790, by = "Cód.")
rm(data_Censo_6790)


#Tabela 6764 - Número de estabelecimentos agropecuários com uso de irrigação e Área irrigada, por método utilizado para irrigação, direção dos trabalhos do estabelecimento agropecuário e origem da orientação técnica recebida - resultados preliminares 2017																									
data_Censo_6764 <- read_excel("Dados/Raw/Censo - 2017/tabela6764.xlsx")
data_Censo_6764[data_Censo_6764=="-"]<-0
data_Censo_6764[data_Censo_6764=="X"]<-NA
data_Censo_6764[, c(3:24)] <- sapply(data_Censo_6764[, c(3:24)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6764, by = "Cód.")
rm(data_Censo_6764)


#Tabela 6722 - Número de estabelecimentos agropecuários e Área dos estabelecimentos, por utilização das terras, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																										
data_Censo_6722 <- read_excel("Dados/Raw/Censo - 2017/tabela6722.xlsx", col_types = "text")
data_Censo_6722[data_Censo_6722=="-"]<-0
data_Censo_6722[data_Censo_6722=="X"]<-NA
data_Censo_6722[, c(3:24)] <- sapply(data_Censo_6722[, c(3:24)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6722, by = "Cód.")
rm(data_Censo_6722)


#Tabela 6654 - Número de estabelecimentos agropecuários por tipo de prática agrícola - resultados preliminares 2017												
data_Censo_6654 <- read_excel("Dados/Raw/Censo - 2017/tabela6654.xlsx")
data_Censo_6654[data_Censo_6654=="-"]<-0
data_Censo_6654[data_Censo_6654=="X"]<-NA
data_Censo_6654[, c(3:12)] <- sapply(data_Censo_6654[, c(3:12)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6654, by = "Cód.")
rm(data_Censo_6654)


#Tabela 6658 - Número de estabelecimentos agropecuários por financiamentos/empréstimos - resultados preliminares 2017																
data_Censo_6658 <- read_excel("Dados/Raw/Censo - 2017/tabela6658.xlsx", col_types = "text")
data_Censo_6658[data_Censo_6658=="-"]<-0
data_Censo_6658[data_Censo_6658=="X"]<-NA
data_Censo_6658[, c(3:16)] <- sapply(data_Censo_6658[, c(3:16)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6658, by = "Cód.")
rm(data_Censo_6658)



#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017										
#Condição legal do produtor
data_Censo_6707 <- read_excel("Dados/Raw/Censo - 2017/tabela6707.xlsx", col_types = "text")
data_Censo_6707[data_Censo_6707=="-"]<-0
data_Censo_6707[data_Censo_6707=="X"]<-NA
data_Censo_6707[, c(3:10)] <- sapply(data_Censo_6707[, c(3:10)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6707, by = "Cód.")
rm(data_Censo_6707)



#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																
#Alfabetização
data_Censo_6707_2 <- read_excel("Dados/Raw/Censo - 2017/tabela6707 (1).xlsx", col_types = "text")
data_Censo_6707_2[data_Censo_6707_2=="-"]<-0
data_Censo_6707_2[data_Censo_6707_2=="X"]<-NA
data_Censo_6707_2[, c(3:16)] <- sapply(data_Censo_6707_2[, c(3:16)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6707_2, by = "Cód.")
rm(data_Censo_6707_2)


#Tabela 6653 - Número de estabelecimentos agropecuários por adubação, calagem e agrotóxicos - resultados preliminares 2017													
data_Censo_6653 <- read_excel("Dados/Raw/Censo - 2017/tabela6653.xlsx", col_types = "text")
data_Censo_6653[data_Censo_6653=="-"]<-0
data_Censo_6653[data_Censo_6653=="X"]<-NA
data_Censo_6653[, c(3:13)] <- sapply(data_Censo_6653[, c(3:13)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6653, by = "Cód.")
rm(data_Censo_6653)



#Tabela 6792 - Número de estabelecimentos agropecuários por outras receitas do estabelecimento e do produtor - resultados preliminares 2017															
data_Censo_6792 <- read_excel("Dados/Raw/Censo - 2017/tabela6792.xlsx", col_types = "text")
data_Censo_6792[data_Censo_6792=="-"]<-0
data_Censo_6792[data_Censo_6792=="X"]<-NA
data_Censo_6792[, c(3:15)] <- sapply(data_Censo_6792[, c(3:15)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6792, by = "Cód.")
rm(data_Censo_6792)



#Tabela 6651 - Número de estabelecimentos agropecuários por associação, uso de energia elétrica, orientação técnica e obtenção de informação - resultados preliminares 2017																												
data_Censo_6651 <- read_excel("Dados/Raw/Censo - 2017/tabela6651.xlsx", col_types = "text")
data_Censo_6651[data_Censo_6651=="-"]<-0
data_Censo_6651[data_Censo_6651=="X"]<-NA
data_Censo_6651[, c(3:28)] <- sapply(data_Censo_6651[, c(3:28)], as.numeric)

data_Censo_Geral <- data_Censo_Geral %>% left_join(data_Censo_6651, by = "Cód.")
rm(data_Censo_6651)




