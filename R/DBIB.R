#'Diseño de bloques incompletos balanceados
#'
#'Realiza un calculo de anova para un diseño de bloques incompletos
#'
#'@param respuesta (string) variable respuesta.
#'@param tratamiento (string) variable que representa los tratamientos.
#'@param bloque (string) variable que representan los bloques.
#'@param data (\code{data.frame}) Tabla de datos en formato largo con los datos.
#'@return Devuelve una tabla en formato \code{data.frame} el análisis de varianza.
#'
#'@export


Aov_DBIB <- function(respuesta, tratamiento, bloque, data){

  # Defino la variable respuesta y los tratamientos y bloques como factores
  y <- data[[tratamiento]]
  trata <- factor(data[,tratamiento])
  bloque <- factor(data[,bloque])
  a <- nlevels(trata)
  b <- nlevels(bloque)

  # Corrección para la media
  suma_total <- sum(y)
  C <- suma_total^2 /(a*b)

  # SumaC Total
  sc_total <- sum(y^2) - C
  gl_total <- a*b-1
  cm_total <- sc_total / gl_total

  # SumaC tratamientos
  sumasxtrata <- tapply(y, INDEX = trata, FUN = sum)
  n_trata <- tapply(y, INDEX = trata, FUN = length)
  sc_trata <- sum(sumasxtrata^2 / n_trata) - C
  gl_trata <- a-1
  cm_trata <- sc_trata / gl_trata

  # SumaC bloques
  sumasxbloques <- tapply(y, INDEX = bloque, FUN = sum)
  n_bloques <- tapply(y, INDEX = bloque, FUN = length)
  sc_bloques <- sum(sumasxbloques^2 / n_bloques) - C
  gl_bloques <- b-1
  cm_bloques <- sc_bloques / gl_bloques

  # residuales
  sc_res <- sc_total - sc_trata - sc_bloques
  gl_res <- (a-1)*(b-1)
  cm_res <- sc_res / gl_res

  # Valores de F
  F_trata <- cm_trata / cm_res
  F_bloques <- cm_bloques / cm_res

  # P-values
  p_value_trata <- pf(F_trata, gl_trata, gl_res, lower.tail = FALSE)
  p_value_bloques <- pf(F_bloques, gl_bloques, gl_res, lower.tail = FALSE)


  # Creacion del dataframe
  tabladf <- data.frame(FV = c("Tratamientos", "Bloques", "Residuales", "Total"),
                        SC = c(sc_trata, sc_bloques, sc_res, sc_total),
                        GL = c(gl_trata, gl_bloques, gl_res, gl_total),
                        CM = c(cm_trata, cm_bloques, cm_res, NA),
                        F = c(F_trata, F_bloques, NA, NA),
                        `Pr(>F)` = c(p_value_trata, p_value_bloques, NA, NA),
                        check.names = FALSE)
  rownames(tabladf) <- NULL
  anov <- format(tabladf)
  anov[is.na(tabladf)] <- ""

  # realizar un boxplot

  boxplot(bloque ~ trata, data = data, main = "Boxplot", xlab = "Tratamientos", ylab = "Bloques")

  #boxplot(trata ~ bloque, data = data, main = "Boxplot", xlab = "Bloques", ylab = "Tratamientos")


  return(anov)
}

# ejemplo de ejecucion

library(readxl)
DATOSTB <- read_excel("~/2 SEMESTRE ZOOTECNIA/I.COMPUTO/DATOSTB.xlsx")
datosdf<- data.frame (DATOSTB)
datosdf
Aov_DBIB(respuesta = "y", tratamiento = "TRATAMIENTOS", bloque = "BLOQUES", data = datosdf)
