library(tidyverse)

isrr6_1_17 <- read.table("I6_1_17.csv", sep = ',', header = TRUE)

tmp <-  sub(pattern = "\\..*", replacement = "", isrr6_1_17$Base.Pairs..bp.)
isrr6_1_17$Base.Pairs..bp. <- as.numeric(sub(',','', tmp))

data <- isrr6_1_17 %>% select(Lane,Base.Pairs..bp., Band.Label)
filter_bands_ladder <- c(1,10,20)

data <- data %>% filter(!(Lane %in% filter_bands_ladder))

# barplot(data$Base.Pairs..bp.)

library(ggplot2)

ggplot(data = data) + geom_histogram(aes(x=Base.Pairs..bp.), bins = 60)











library(tidyverse)

isrr6_1_17 <- read.table("I6_1_17.csv", sep = ",", header = TRUE)

# tmp <- sub(pattern = "\\..*", replacement = "", isrr6_1_17$Base.Pairs..bp.)
tmp <- sub(pattern = ",", replacement = "", isrr6_1_17$Base.Pairs..bp.)
# isrr6_1_17$Base.Pairs..bp. <- as.numeric(sub(",", "", tmp))
isrr6_1_17$Base.Pairs..bp. <- round(as.numeric(tmp),0)

data <- isrr6_1_17 %>%
  select(Lane, Band.Label, Base.Pairs..bp.) %>%
  filter(!(Lane %in% c(1, 10, 20)))  # quitamos las escaleras



# x: vector numérico de tamaños (bp)
# gap_thr: diferencia mínima (en bp) para considerar que hay un cambio de grupo
find_breaks <- function(x, gap_thr = 40) {
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

breaks <- find_breaks(data$Base.Pairs..bp., gap_thr = 30)
breaks
length(breaks) - 1  # número de intervalos


# centros de cada intervalo (en bp)
centers <- (head(breaks, -1) + tail(breaks, -1)) / 2

# etiquetas de marcador, puedes personalizar el prefijo
labels <- paste0("M", seq_along(centers), "_", round(centers))

data_markers <- data %>%
  mutate(
    marcador_probable = cut(
      Base.Pairs..bp.,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
  )

table(data_markers$marcador_probable)
ggplot(data_markers, aes(x = Base.Pairs..bp.)) +
  geom_histogram(bins = 60, fill = "grey70") +
  geom_vline(xintercept = centers, linetype = 2) +
  labs(x = "Base pairs (bp)", y = "Frecuencia") +
  theme_minimal()






matriz_pa <- data_markers %>%
  mutate(valor = 1L) %>%                     # presencia = 1
  distinct(Lane, marcador_probable, .keep_all = TRUE) %>%  # por si hay bandas duplicadas
  pivot_wider(
    names_from  = marcador_probable,
    values_from = valor,
    values_fill = list(valor = 0L)          # ausencia = 0
  )


matriz_pa <- data_markers %>%
  mutate(valor = 1L) %>%
  pivot_wider(
    names_from  = marcador_probable,
    values_from = valor,
    values_fill = list(valor = 0L),
    values_fn   = list(valor = max)   # 0 = ausencia, 1 = presente
  )


matriz_pa <- data_markers %>%
  mutate(valor = 1L) %>%                      # presencia = 1
  select(Lane, marcador_probable, valor) %>%  # quitamos Base.Pairs..bp. y otras
  pivot_wider(
    id_cols    = Lane,                        # <- SOLO una fila por Lane
    names_from = marcador_probable,
    values_from = valor,
    values_fill = 0L,                         # ausencia = 0
    values_fn  = max                          # si hay varias, 1 si aparece al menos una vez
  )

