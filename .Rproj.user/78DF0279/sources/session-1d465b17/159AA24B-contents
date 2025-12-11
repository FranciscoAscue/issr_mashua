# Asegúrate de que la matriz sea solo numérica (sin la columna Accesion)
# Como ya pusiste los row.names, podemos quitar la columna 1
datos_binarios <- matriz_pa_size[, -1] 

# Crear el objeto genind especificando tipo Presencia/Ausencia
MASHUA <- df2genind(datos_binarios,
                    ploidy = 1,      # Para datos dominantes P/A se usa ploidy=1 (pseudo-haploide)
                    type = "PA",     # CRÍTICO: Indica que son datos binarios (0/1)
                    pop = mapa_ind_tmp$PROVINCIA) # Asignamos la población directamente aquí

# Verificar
MASHUA

library(poppr)

# Tabla básica de diversidad por población
# Hexp = Diversidad genética de Nei (equivalente a He para dominantes)
# ia = Índice de asociación (reproducción clonal vs sexual)
tabla_diversidad <- poppr(MASHUA)

print(tabla_diversidad)

# Si quieres graficar la diversidad (Hexp)
library(ggplot2)
ggplot(tabla_diversidad, aes(x = Pop, y = Hexp, fill = Pop)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  labs(title = "Diversidad Genética de Nei por Provincia", y = "He (Nei)")




library(ade4)   # Necesaria para s.class y dudi.pco
library(scales) # Necesaria para la función alpha()

# 1. Calcular distancia (si no lo hiciste arriba)
dist_mashua <- dist(MASHUA, method = "euclidean")

# 2. Calcular PCoA
pcoa_mashua <- dudi.pco(dist_mashua, scannf = FALSE, nf = 3)

# 3. Graficar con la corrección de transparencia
# Usamos scales::alpha en lugar de transpdf
s.class(pcoa_mashua$li, pop(MASHUA), 
        col = alpha(funky(nPop(MASHUA)), 0.6),  # <--- CORRECCIÓN AQUÍ
        cstar = 0, 
        cellipse = 1, 
        axesell = FALSE)

title("PCoA de poblaciones de Mashua (ISSR)")


library(ape)

# Usamos la distancia calculada anteriormente
arbol_upgma <- hclust(dist_mashua, method = "average") # UPGMA

# Graficar árbol simple
plot(arbol_upgma, main = "Dendrograma UPGMA - Mashua", cex = 0.6)

# O una versión filogenética más bonita con ape
arbol_phylo <- as.phylo(arbol_upgma)
plot(arbol_phylo, type = "fan", tip.color = funky(nPop(MASHUA))[pop(MASHUA)], 
     cex = 0.5, label.offset = 0.5)
legend("topleft", legend = levels(pop(MASHUA)), 
       fill = funky(nPop(MASHUA)), cex = 0.8)



# Realizar AMOVA
# Nota: poppr.amova maneja distancias euclidianas por defecto
amova_res <- poppr.amova(MASHUA, hier = ~Pop)

# Ver resultados
amova_res
write.table(amova_res$results, "amova_results.csv")

# Prueba de significancia (Permutaciones)
amova_signif <- randtest(amova_res, nrepet = 999)
plot(amova_signif)
amova_signif



library(adegenet)

# 1. Ejecutar un DAPC preliminar conservando todos los PCs
# n.da = número de poblaciones - 1
# n.pca = null para que use todos inicialmente
dapc_prelim <- dapc(MASHUA, n.pca = NULL, n.da = nPop(MASHUA) - 1) 
# Nota: Si te pide seleccionar interactivamente, selecciona un número alto (ej. 40) para esta prueba.

# 2. Calcular el a-score para optimizar
# El a-score mide qué tan estable es la asignación de grupos.
# Queremos el número de PCs que maximice este score sin sobreajustar.
test_a_score <- optim.a.score(dapc_prelim, plot = TRUE)

# Ver el número óptimo sugerido
best_n_pca <- test_a_score$best
print(paste("El número óptimo de PCs es:", best_n_pca))


# Ejecutamos el DAPC final
dapc_final <- dapc(MASHUA, 
                   n.pca = best_n_pca, 
                   n.da = nPop(MASHUA) - 1) # Generalmente n.pops - 1

# Resumen del modelo
dapc_final

# Scatterplot básico
scatter(dapc_final, 
        scree.da = FALSE, # Ocultar el gráfico pequeño de eigenvalores
        bg = "white",     # Fondo
        pch = 20,         # Tipo de punto
        cell = 1.5,       # Tamaño de las elipses
        cstar = 0,        # Quitar las líneas de estrella (más limpio)
        col = funky(nPop(MASHUA)), # Colores
        clab = 0.75,      # Tamaño de las etiquetas
        legend = TRUE)    # Mostrar leyenda




# Gráfico de barras de asignación
compoplot(dapc_final, 
          col = funky(nPop(MASHUA)), 
          lab = indNames(MASHUA), # Nombres de individuos abajo
          legend = FALSE, 
          cex.names = 0.5)        # Tamaño de letra de los nombres

# Versión más limpia sin nombres individuales (mejor si tienes muchos datos)
compoplot(dapc_final, 
          col = funky(nPop(MASHUA)), 
          show.lab = FALSE, 
          legend = TRUE,
          posi = "topright")




# Ver la contribución de los alelos al primer eje discriminante
contrib <- loadingplot(dapc_final$var.contr, 
                       axis = 1, 
                       thres = 0.05,  # Umbral para etiquetar (ajústalo según tus datos)
                       lab.jitter = 1)
