matriz_binaria <- function(data_markers, grosor = FALSE){
  
  if(grosor == TRUE){
    matriz <- data_markers %>%
      filter(!is.na(marcador_ab), !is.na(Lane)) %>%
      mutate(valor = 1L) %>%
      select(Lane, marcador_ab, valor) %>%
      distinct(Lane, marcador_ab, .keep_all = TRUE) %>%  # evitar duplicados exactos
      pivot_wider(
        id_cols    = Lane,
        names_from = marcador_ab,
        values_from = valor,
        values_fill = 0L,
        values_fn  = max   # 0 = ausencia, 1 = presente
      ) %>%
      arrange(Lane)
  }else{
    matriz <- data_markers %>%
      filter(!is.na(marcador_probable), !is.na(Accesion)) %>%
      mutate(valor = 1L) %>%
    select(Accesion, marcador_probable, valor) %>%
    distinct(Accesion, marcador_probable, .keep_all = TRUE) %>%
    pivot_wider(
      id_cols    = Accesion,
      names_from = marcador_probable,
      values_from = valor,
      values_fill = 0L,
      values_fn  = max
    ) %>%
    arrange(Accesion)
  }
  
  return(matriz)
}

