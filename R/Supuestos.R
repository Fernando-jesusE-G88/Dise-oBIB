# SUPUESTOS

res2 <- mod1$Residuals
shapiro.test(res2)

bartlett.test(res2, tratamientos)

boxplot (bloques~tratamientos)

library(readxl)
DATOSTB <- read_excel("~/2 SEMESTRE ZOOTECNIA/I.COMPUTO/DATOSTB.xlsx")
datos<- data.frame(DATOSTB)
datos
Aov_DBIB(respuesta = "TRATAMIENTOS", tratamiento = "TRATAMIENTOS", bloque = "BLOQUES", data = datos)
