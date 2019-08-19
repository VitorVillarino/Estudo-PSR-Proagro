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



######################### Ver ###################################



##Ver warnings
#Tabela 6643 - Número de estabelecimentos agropecuários por telefone, e-mail e internet - resultados preliminares 2017											
data_Censo_6643 <- read_excel("Dados/Raw/Censo - 2017/tabela6643.xlsx", col_types = "text")
data_Censo_6643[data_Censo_6643=="-"]<-0
data_Censo_6643[data_Censo_6643=="X"]<-NA
data_Censo_6643[, c(3:11)] <- sapply(data_Censo_6643[, c(3:11)], as.numeric)


#Tabela 6650 - Número de estabelecimentos agropecuários por forma de obtenção das terras - resultados preliminares 2017													
data_Censo_6650 <- read_excel("Dados/Raw/Censo - 2017/tabela6650.xlsx")
data_Censo_6650[data_Censo_6650=="-"]<-0
data_Censo_6650[data_Censo_6650=="X"]<-NA
data_Censo_6650[, c(3:13)] <- sapply(data_Censo_6650[, c(3:13)], as.numeric)


#Tabela 6710 - Número de estabelecimentos agropecuários, Área dos estabelecimentos agropecuários, por condição legal das terras, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																										
data_Censo_6710 <- read_excel("Dados/Raw/Censo - 2017/tabela6710.xlsx", col_types = "text")
data_Censo_6710[data_Censo_6710=="-"]<-0
data_Censo_6710[data_Censo_6710=="X"]<-NA
data_Censo_6710[data_Censo_6710==".."]<-NA
data_Censo_6710[data_Censo_6710=="..."]<-NA
data_Censo_6710[, c(3:26)] <- sapply(data_Censo_6710[, c(3:26)], as.numeric)


#Tabela 6659 - Número de estabelecimentos agropecuários por agente financeiro responsável pelo financiamento - resultados preliminares 2017
data_Censo_6659 <- read_excel("Dados/Raw/Censo - 2017/tabela6659.xlsx", col_types = "text")
data_Censo_6659[data_Censo_6659=="-"]<-0
data_Censo_6659[data_Censo_6659=="X"]<-NA
data_Censo_6659[, c(3:12)] <- sapply(data_Censo_6659[, c(3:12)], as.numeric)




#Tabela 6647 - Número de estabelecimentos agropecuários por sexo, alfabetização, idade e cor ou raça do produtor - resultados preliminares 2017														
data_Censo_6647 <- read_excel("Dados/Raw/Censo - 2017/tabela6647.xlsx", col_types = "text")
data_Censo_6647[data_Censo_6647=="-"]<-0
data_Censo_6647[data_Censo_6647=="X"]<-NA
data_Censo_6647[data_Censo_6647==".."]<-NA
data_Censo_6647[data_Censo_6647=="..."]<-NA
data_Censo_6647[, c(3:14)] <- sapply(data_Censo_6647[, c(3:14)], as.numeric)


#Tabela 6649 - Número de estabelecimentos agropecuários por residência, finalidade da produção e DAP - resultados preliminares 2017																
data_Censo_6649 <- read_excel("Dados/Raw/Censo - 2017/tabela6649.xlsx", col_types = "text")
data_Censo_6649[data_Censo_6649=="-"]<-0
data_Censo_6649[data_Censo_6649=="X"]<-NA
data_Censo_6649[, c(3:16)] <- sapply(data_Censo_6649[, c(3:16)], as.numeric)


#Tabela 6652 - Número de estabelecimentos agropecuários por uso de agricultura orgânica - resultados preliminares 2017							
data_Censo_6652 <- read_excel("Dados/Raw/Censo - 2017/tabela6652.xlsx")
data_Censo_6652[data_Censo_6652=="-"]<-0
data_Censo_6652[data_Censo_6652=="X"]<-NA
data_Censo_6652[, c(3:7)] <- sapply(data_Censo_6652[, c(3:7)], as.numeric)


#Tabela 6655 - Número de estabelecimentos agropecuários por nascentes, rios/riachos, poços e cisternas - resultados preliminares 2017													
data_Censo_6655 <- read_excel("Dados/Raw/Censo - 2017/tabela6655.xlsx", col_types = "text")
data_Censo_6655[data_Censo_6655=="-"]<-0
data_Censo_6655[data_Censo_6655=="X"]<-NA
data_Censo_6655[, c(3:13)] <- sapply(data_Censo_6655[, c(3:13)], as.numeric)


#Tabela 6709 - Número de estabelecimentos agropecuários, por existência de energia elétrica, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017				
data_Censo_6709 <- read_excel("Dados/Raw/Censo - 2017/tabela6709.xlsx", col_types = "text")
data_Censo_6709[data_Censo_6709=="-"]<-0
data_Censo_6709[data_Censo_6709=="X"]<-NA
data_Censo_6709[, c(3:4)] <- sapply(data_Censo_6709[, c(3:4)], as.numeric)


#Tabela 6713 - Número de estabelecimentos agropecuários por contratação de serviços, tipo de prestador de serviços e dias trabalhados, sexo do produtor, escolaridade do produtor, condição legal do produtor e origem da orientação técnica recebida - resultados preliminares 2017
data_Censo_6713 <- read_excel("Dados/Raw/Censo - 2017/tabela6713.xlsx", col_types = "text")
data_Censo_6713[data_Censo_6713=="-"]<-0
data_Censo_6713[data_Censo_6713=="X"]<-NA
data_Censo_6713[data_Censo_6713==".."]<-NA
data_Censo_6713[data_Censo_6713=="..."]<-NA
data_Censo_6713[, c(3:12)] <- sapply(data_Censo_6713[, c(3:12)], as.numeric)


#Tabela 6790 - Número de estabelecimentos agropecuários por classes de idade do produtor - resultados preliminares 2017																		
data_Censo_6790 <- read_excel("Dados/Raw/Censo - 2017/tabela6790.xlsx", col_types = "text")
data_Censo_6790[data_Censo_6790=="-"]<-0
data_Censo_6790[data_Censo_6790=="X"]<-NA
data_Censo_6790[, c(3:16)] <- sapply(data_Censo_6790[, c(3:16)], as.numeric)


#Tabela 6764 - Número de estabelecimentos agropecuários com uso de irrigação e Área irrigada, por método utilizado para irrigação, direção dos trabalhos do estabelecimento agropecuário e origem da orientação técnica recebida - resultados preliminares 2017																									
data_Censo_6764 <- read_excel("Dados/Raw/Censo - 2017/tabela6764.xlsx")
data_Censo_6764[data_Censo_6764=="-"]<-0
data_Censo_6764[data_Censo_6764=="X"]<-NA
data_Censo_6764[, c(3:24)] <- sapply(data_Censo_6764[, c(3:24)], as.numeric)


#Tabela 6722 - Número de estabelecimentos agropecuários e Área dos estabelecimentos, por utilização das terras, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																										
data_Censo_6722 <- read_excel("Dados/Raw/Censo - 2017/tabela6722.xlsx", col_types = "text")
data_Censo_6722[data_Censo_6722=="-"]<-0
data_Censo_6722[data_Censo_6722=="X"]<-NA
data_Censo_6722[, c(3:24)] <- sapply(data_Censo_6722[, c(3:24)], as.numeric)


#Tabela 6654 - Número de estabelecimentos agropecuários por tipo de prática agrícola - resultados preliminares 2017												
data_Censo_6654 <- read_excel("Dados/Raw/Censo - 2017/tabela6654.xlsx")
data_Censo_6654[data_Censo_6654=="-"]<-0
data_Censo_6654[data_Censo_6654=="X"]<-NA
data_Censo_6654[, c(3:12)] <- sapply(data_Censo_6654[, c(3:12)], as.numeric)


#Tabela 6658 - Número de estabelecimentos agropecuários por financiamentos/empréstimos - resultados preliminares 2017																
data_Censo_6658 <- read_excel("Dados/Raw/Censo - 2017/tabela6658.xlsx", col_types = "text")
data_Censo_6658[data_Censo_6658=="-"]<-0
data_Censo_6658[data_Censo_6658=="X"]<-NA
data_Censo_6658[, c(3:16)] <- sapply(data_Censo_6658[, c(3:16)], as.numeric)


#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017										
#Condição legal do produtor
data_Censo_6707 <- read_excel("Dados/Raw/Censo - 2017/tabela6707.xlsx", col_types = "text")
data_Censo_6707[data_Censo_6707=="-"]<-0
data_Censo_6707[data_Censo_6707=="X"]<-NA
data_Censo_6707[, c(3:10)] <- sapply(data_Censo_6707[, c(3:10)], as.numeric)


#Tabela 6707 - Número de estabelecimentos agropecuários, por associação do produtor à cooperativa e/ou à entidade de classe, sexo do produtor, escolaridade do produtor, condição legal do produtor, direção dos trabalhos do estabelecimento agropecuário e grupos de área total - resultados preliminares 2017																
#Alfabetização
data_Censo_6707_2 <- read_excel("Dados/Raw/Censo - 2017/tabela6707 (1).xlsx", col_types = "text")
data_Censo_6707_2[data_Censo_6707_2=="-"]<-0
data_Censo_6707_2[data_Censo_6707_2=="X"]<-NA
data_Censo_6707_2[, c(3:16)] <- sapply(data_Censo_6707_2[, c(3:16)], as.numeric)


#Tabela 6653 - Número de estabelecimentos agropecuários por adubação, calagem e agrotóxicos - resultados preliminares 2017													
data_Censo_6653 <- read_excel("Dados/Raw/Censo - 2017/tabela6653.xlsx", col_types = "text")
data_Censo_6653[data_Censo_6653=="-"]<-0
data_Censo_6653[data_Censo_6653=="X"]<-NA
data_Censo_6653[, c(3:13)] <- sapply(data_Censo_6653[, c(3:13)], as.numeric)


#Tabela 6792 - Número de estabelecimentos agropecuários por outras receitas do estabelecimento e do produtor - resultados preliminares 2017															
data_Censo_6792 <- read_excel("Dados/Raw/Censo - 2017/tabela6792.xlsx", col_types = "text")
data_Censo_6792[data_Censo_6792=="-"]<-0
data_Censo_6792[data_Censo_6792=="X"]<-NA
data_Censo_6792[, c(3:15)] <- sapply(data_Censo_6792[, c(3:15)], as.numeric)


#Tabela 6651 - Número de estabelecimentos agropecuários por associação, uso de energia elétrica, orientação técnica e obtenção de informação - resultados preliminares 2017																												
data_Censo_6651 <- read_excel("Dados/Raw/Censo - 2017/tabela6651.xlsx", col_types = "text")
data_Censo_6651[data_Censo_6651=="-"]<-0
data_Censo_6651[data_Censo_6651=="X"]<-NA
data_Censo_6651[, c(3:28)] <- sapply(data_Censo_6651[, c(3:28)], as.numeric)



openxlsx::write.xlsx(data_Censo_6635, "data_Censo_6635_2.xlsx")



data_Censo <- data_Censo_6635 %>%  
              left_join(data_Censo_6639, by = c("Cód.","Município")) %>% 
              left_join(data_Censo_6640, by = c("Cód.","Município")) %>% 
              left_join(data_Censo_6641, by = c("Cód.","Município")) %>% 
              left_join(data_Censo_6642, by = c("Cód.","Município")) 

  
data_Censo_6639 <- NULL


  

