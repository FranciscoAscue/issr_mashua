library(tidyverse)
source('Scripts/load_data.R', local = TRUE)
source('Scripts/matriz_binaria.R', local = TRUE)
source('Scripts/delimitador_bandas.R', local = TRUE)
source('Scripts/install_load.R')
# =========================
# 1) Leer y limpiar datos
# =========================

isrr5_1_18 <- load_data("Data/EXCEL_I5-1AL18.xlsx", c(1,10,21))
isrr5_19_35 <- load_data("Data/EXCEL_I5-19AL35.xlsx", c(1,10,20))
isrr5_36_52 <- load_data("Data/EXCEL_I5-36AL52.xlsx", c(1,10,20))
isrr5_53_69 <- load_data("Data/EXCEL_I5-53AL69.xlsx", c(1,10,20))
isrr5_70_86 <- load_data("Data/EXCEL_I5-70AL86.xlsx", c(1,10,20))
issr5 <- rbind(isrr5_1_18,isrr5_19_35,isrr5_36_52,isrr5_53_69,isrr5_70_86)

isrr7_1_18 <- load_data("Data/EXCEL_I7- 1AL16.xlsx", c(1,10,20))
isrr7_19_35 <- load_data("Data/EXCEL_I7- 17AL33.xlsx", c(1,10,20))
isrr7_36_52 <- load_data("Data/EXCEL_I7- 34AL50.xlsx", c(1,10,20))
isrr7_53_69 <- load_data("Data/EXCEL_I7- 51AL67.xlsx", c(1,10,20))
isrr7_70_86 <- load_data("Data/EXCEL_I7- 68AL86.xlsx", c(1,10,20))
issr7 <- rbind(isrr7_1_18,isrr7_19_35,isrr7_36_52,isrr7_53_69,isrr7_70_86)

isrr8_1_18 <- load_data("Data/EXCEL_I8_1_16.xlsx", c(1,10,20))
isrr8_19_35 <- load_data("Data/", c(1,10,20))
isrr8_36_52 <- load_data("Data/", c(1,10,20))
isrr8_53_69 <- load_data("Data/", c(1,10,20))
isrr8_70_86 <- load_data("Data/", c(1,10,20))
issr8 <- rbind(isrr8_1_18,isrr8_19_35,isrr8_36_52,isrr8_53_69,isrr8_70_86)

#===============
# 2) Definir intervalos de tamaño (marcadores)
# =========================

data_markers5 <- delimitador_bandas(issr5, 'ISSR5',10)
data_markers7 <- delimitador_bandas(issr7, 'ISSR7',10)
data_markers8 <- delimitador_bandas(issr8, 'ISSR8',10)
data_markers1 <- delimitador_bandas(issr1, 'ISSR1',10)
data_markers11 <- delimitador_bandas(issr11, 'ISSR11',10)
data_markers10 <- delimitador_bandas(issr10, 'ISSR10',10)
data_markers13 <- delimitador_bandas(issr13, 'ISSR13',10)
data_markers14 <- delimitador_bandas(issr14, 'ISSR14',10)

# =========================
# 3) Matriz presencia/ausencia
#    (marcador + tipo de banda a/b)
# =========================

matriz_pa_size5 <- matriz_binaria(data_markers5)
matriz_pa_size7 <- matriz_binaria(data_markers7)
matriz_pa_size8 <- matriz_binaria(data_markers8)
matriz_pa_size1 <- matriz_binaria(data_markers1)
matriz_pa_size11 <- matriz_binaria(data_markers11)
matriz_pa_size10 <- matriz_binaria(data_markers10)
matriz_pa_size13 <- matriz_binaria(data_markers13)
matriz_pa_size14 <- matriz_binaria(data_markers14)

# =========================
# 4) Analisis de Genetica poblacional.
# =========================


matriz_pa_size <- merge(matriz_pa_size5, matriz_pa_size7, by = "Accesion")


genind_object <- function(object, ploidy = 1, metadata){
  ploidy(object) <- ploidy
  indNames(object) <- metadata$Sample
  strata(object) <- data.frame(metadata$Pop)
  nameStrata(object) <- ~Pop
  
  # Summary of object
  print(table(strata(object, ~Pop)))
  print(summary(object))
  # object@tab
  return(object)
}

matriz_pa_size <- as.data.frame(matriz_pa_size)
row.names(matriz_pa_size) <- matriz_pa_size$Accesion
## Convert genepop data to complete genind data
my_genlight <- adegenet::df2genind(matriz_pa_size,
                           ploidy = 1,   # o 1 si son haploides
                           ncode  = 1)   # 1 dígito por alelo (0,1,2)
## Load and set up metadata
mapa_ind_tree <- read.csv2("Data/Data_pasaporte.csv")
rownames(mapa_ind_tree) <- mapa_ind_tree$CODIGO.DE.ACCESIÓN



id_gen <- indNames(my_genlight)   # nombres de individuos en genlight
id_meta <- mapa_ind_tree$CODIGO.DE.ACCESIÓN       # columna con IDs en tu CSV
## Create complete genind object

mapa_ind_tmp <- mapa_ind_tree %>% filter(CODIGO.DE.ACCESIÓN %in% matriz_pa_size$Accesion)

pop(my_genlight) <- mapa_ind_tmp$PROVINCIA


MASHUA <- my_genlight
library(adegenet)
library(hierfstat)
library(poppr)
library(pegas)   #



MASHUA
sum_MASHUA <- summary(MASHUA)

# Número de individuos, loci y poblaciones
n_ind <- nInd(MASHUA)
n_loc <- nLoc(MASHUA)
pops  <- levels(pop(MASHUA))

n_ind; n_loc; pops
# Para mirar el resumen completo:
sum_MASHUA

