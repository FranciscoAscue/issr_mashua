


find_breaks <- function(x, gap_thr = 10) {
  xs   <- sort(na.omit(x))
  gaps <- diff(xs)
  
  # puntos donde el "salto" es grande
  idx_big_gap <- which(gaps > gap_thr)
  
  # punto medio entre los dos valores que forman el hueco
  cut_points <- xs[idx_big_gap] + gaps[idx_big_gap] / 2
  
  # extremos del rango + un pequeño margen
  breaks <- c(min(xs) - 1, cut_points, max(xs) + 1)
  breaks
}


delimitador_bandas <- function(issr, label, gap ){
  # Puedes ajustar gap_thr según lo cerrado/abierto que quieras los grupos
  breaks  <- find_breaks(issr$Size_pb, gap_thr = gap)
  
  centers <- (head(breaks, -1) + tail(breaks, -1)) / 2
  
  # etiquetas de marcador, por ejemplo: M1_1576, M2_972, ...
  labels <- paste0(label, "_", round(centers))
  
  # =========================
  # 3) Asignar marcador por tamaño + tipo de banda (a/b)
  # =========================
  
  data_markers <- issr %>%
    mutate(
      marcador_probable = cut(
        Size_pb,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      )
    ) %>%
    # Clasificamos grosor relativo dentro de cada marcador:
    # a = banda gruesa (>= mediana de grosor)
    # b = banda delgada (< mediana de grosor)
    group_by(marcador_probable) %>%
    mutate(
      umbral_grosor = median(Banda_tamanho, na.rm = TRUE),
      tipo_banda    = if_else(Banda_tamanho >= umbral_grosor, "a", "b")
    ) %>%
    ungroup() %>%
    mutate(
      marcador_ab = paste0(as.character(marcador_probable), "_", tipo_banda)
    )
  
  # =========================
  # 4) Gráfico de control (tamaños + cortes)
  # =========================
  
  p <- ggplot(data_markers, aes(x = Size_pb)) +
    geom_histogram(bins = 60, fill = "grey70") +
    geom_vline(xintercept = centers, linetype = 2) +
    labs(x = "Base pairs (bp)", y = "Frecuencia") +
    theme_minimal()
  
  print(p)
  
  return(data_markers)
  
}



