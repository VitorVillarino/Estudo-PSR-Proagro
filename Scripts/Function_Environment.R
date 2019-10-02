
save_environment <-  function () {
    source("./Scripts/01_Load_Data.R", encoding = "UTF-8")
    source("./Scripts/02_Transformacoes_Necessarias.R", encoding = "UTF-8")
    source("./Scripts/Function_Mapas.R", encoding = "UTF-8")
    load_maps()
    save.image(file='environment.RData')
}


load_environment <- function() {
  load('environment.RData',envir = globalenv())
}