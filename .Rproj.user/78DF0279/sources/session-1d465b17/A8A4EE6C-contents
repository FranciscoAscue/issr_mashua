load_data <- function(file,ladders = c(1, 10, 21)){
  tmp <- readxl::read_excel(file)
  # Limpiar Base.Pairs..bp. (quitar comas, pasar a numérico y redondear)
  tmp <- tmp %>%
    mutate(
      Size_pb = Size_pb %>%
        as.character() %>%
        str_replace_all(",", "") %>%
        as.numeric() %>%
        round(0),
      Banda_tamanho = as.numeric(Banda_tamanho )    # por si viene como factor/char
    )
  
  # Nos quedamos con columnas relevantes y quitamos las escaleras
  tmp <- tmp %>%
    select(Lane,Accesion,Size_pb, Banda, Banda_tamanho) %>%
    filter(!(Lane %in% ladders)) %>%   # quitar escaleras
    drop_na(Banda)
  
  return(tmp)
}


