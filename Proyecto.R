#Se define el número de iteraciones a utilizar
Numerodeiteraciones <- 10000000 

#CÁLCULO DE LA CONSTANTE DE RESTITUCIÓN PARA LOS 4 RESORTES

#Definición de los valores experimentales para la constante de restitución
lista_kA <- c(307.6923077, 314.9606299, 312.5, 310.0775194, 314.9606299, 305.3435115, 312.5, 314.9606299, 300.7518797, 300.7518797)
lista_kB <- c(555.5555556, 547.9452055, 555.5555556, 547.9452055, 555.5555556, 540.5405405, 555.5555556, 540.5405405, 547.9452055, 547.9452055)
lista_kC <- c(7.936507937, 7.936507937, 7.96812749, 7.936507937, 7.936507937, 7.936507937, 7.90513834, 7.936507937, 7.936507937, 7.96812749)
lista_kD <- c(7.194244604, 7.142857143, 7.042253521, 7.168458781, 7.142857143, 7.142857143, 7.194244604, 7.067137809, 7.042253521, 7.092198582)


#Definición de los valores medios para las variables
kA = mean(lista_kA)
kB = mean(lista_kB)
kC = mean(lista_kC)
kD = mean(lista_kD)


#Definición de la desviación de los valores experimentales para las variables
desv_kA = sd(lista_kA)
desv_kB = sd(lista_kB)
desv_kC = sd(lista_kC)
desv_kD = sd(lista_kD)


#Definición de las incertidumbres a contemplar para la constante de restitución
res_Dina <- runif(Numerodeiteraciones, -1/2, 1/2)
res_0_Dina <- runif(Numerodeiteraciones, -1/2, 1/2)
res_Regla  <- runif(Numerodeiteraciones, -0.0005/2, 0.000/2)

repet_A <- rnorm(Numerodeiteraciones, 0, desv_kA/sqrt(10)) 
repet_B <- rnorm(Numerodeiteraciones, 0, desv_kB/sqrt(10)) 
repet_C <- rnorm(Numerodeiteraciones, 0, desv_kC/sqrt(10)) 
repet_D <- rnorm(Numerodeiteraciones, 0, desv_kD/sqrt(10)) 


#Definición de los valores de constante de restitución para cada resorte

rest_A = kA + res_Dina + res_0_Dina + res_Regla + repet_A
rest_B = kB + res_Dina + res_0_Dina + res_Regla + repet_B
rest_C = kC + res_Dina + res_0_Dina + res_Regla + repet_C
rest_D = kD + res_Dina + res_0_Dina + res_Regla + repet_D


#CÁLCULO DE LA GRAVEDAD PARA LOS 4 RESORTES

#Masa de los intrumentos en gramos
m_Plato = 42.32188
mA = 81.4666
mB = 50.09249
mC = 10.0028
mD = 5.29575
p50 = 50.00008
p20 = 20.00005
p10 = 9.99995
p500 = 499.997
p700 = 699.997
p1000 = 999.995

#Elonganción de los resortes en metros
lis_Elong_A500 = c(0.011, 0.011, 0.011, 0.011, 0.011)
lis_Elong_A700 = c(0.016, 0.016, 0.016, 0.016, 0.016)
lis_Elong_A1000 = c(0.026, 0.026, 0.026, 0.026, 0.026)

lis_Elong_B500 = c(0.004, 0.004, 0.004, 0.004, 0.004)
lis_Elong_B700 = c(0.008, 0.008, 0.008, 0.008, 0.008)
lis_Elong_B1000 = c(0.016, 0.016, 0.016, 0.016, 0.016)

lis_Elong_C50 = c(0.112, 0.112, 0.112, 0.112, 0.112)
lis_Elong_C20 = c(0.069, 0.069, 0.069, 0.069, 0.069)
lis_Elong_C10 = c(0.055, 0.055, 0.055, 0.055, 0.055)

lis_Elong_D50 = c(0.12, 0.12, 0.12, 0.12, 0.12)
lis_Elong_D20 = c(0.078, 0.078, 0.078, 0.078, 0.078)
lis_Elong_D10 = c(0.065, 0.065, 0.065, 0.065, 0.065)


#Valor de variable dependiente del modelo (kx)
kxA500 = lis_Elong_A500 * mean(rest_A)
kxA700 = lis_Elong_A700 * mean(rest_A)
kxA1000 = lis_Elong_A1000 * mean(rest_A)

kxB500 = lis_Elong_B500 * mean(rest_B)
kxB700 = lis_Elong_B700 * mean(rest_B)
kxB1000 = lis_Elong_B1000 * mean(rest_B)

kxC50 = lis_Elong_C50 * mean(rest_C)
kxC20 = lis_Elong_C20 * mean(rest_C)
kxC10 = lis_Elong_C10 * mean(rest_C)

kxD50 = lis_Elong_D50 * mean(rest_D)
kxD20 = lis_Elong_D20 * mean(rest_D)
kxD10 = lis_Elong_D10 * mean(rest_D)


#Cálculo de las masas para cada medición (variable independiente)
mA500 = (m_Plato + p500)/1000
mA700 = (m_Plato + p700)/1000
mA1000 = (m_Plato + p1000)/1000

mB500 = (m_Plato + p500)/1000
mB700 = (m_Plato + p700)/1000
mB1000 = (m_Plato + p1000)/1000

mC50 = (m_Plato + p50)/1000
mC20 = (m_Plato + p20)/1000
mC10 = (m_Plato + p10)/1000

mD50 = (m_Plato + p50)/1000
mD20 = (m_Plato + p20)/1000
mD10 = (m_Plato + p10)/1000


#Definición de los valores medios para las variables

Elong_A500 = mean (lis_Elong_A500)
Elong_A700 = mean (lis_Elong_A700)
Elong_A1000 = mean (lis_Elong_A1000)

Elong_B500 = mean (lis_Elong_B500)
Elong_B700 = mean (lis_Elong_B700)
Elong_B1000 = mean (lis_Elong_B1000)

Elong_C50 = mean (lis_Elong_C50)
Elong_C20 = mean (lis_Elong_C20)
Elong_C10 = mean (lis_Elong_C10)

Elong_D50 = mean (lis_Elong_D50)
Elong_D20 = mean (lis_Elong_D20)
Elong_D10 = mean (lis_Elong_D10)


#Definición de la desviación de los valores experimentales para las variables

desv_Elon_A500 = sd (lis_Elong_A500)
desv_Elon_A700 = sd (lis_Elong_A700)
desv_Elon_A1000 = sd (lis_Elong_A1000)

desv_Elon_B500 = sd (lis_Elong_B500)
desv_Elon_B700 = sd (lis_Elong_B700)
desv_Elon_B1000 = sd (lis_Elong_B1000)

desv_Elon_C50 = sd (lis_Elong_C50)
desv_Elon_C20 = sd (lis_Elong_C20)
desv_Elon_C10 = sd (lis_Elong_C10)

desv_Elon_D50 = sd (lis_Elong_D50)
desv_Elon_D20 = sd (lis_Elong_D20)
desv_Elon_D10 = sd (lis_Elong_D10)


#Cálculo de la regresión para el resorte A

col1XA <- unlist(list(mA500, mA500, mA500, mA500, mA500, 
                      mA700, mA700, mA700, mA700, mA700, 
                      mA1000, mA1000, mA1000, mA1000, mA1000))      # Se unifican todas las masas del resorte A
matXA <- matrix (col1XA)                          # Se transforma a una matriz
XA <- cbind (1,matXA)                             # Se añade la columna de 1's

colYA <- unlist(list(kxA500, kxA700, kxA1000))    # Se unifican todas las listas del resorte A
YA <- matrix (colYA) 

XTA <- t(XA)

XTXA <- XTA %*% XA                                # Incertidumbre de la ecuación

Inv_XTXA <- solve(XTXA)

XTXXTA <- Inv_XTXA %*% XTA

BetaA <- XTXXTA %*% YA                            # Resultado de la ecuación


#Cálculo de la regresión para el resorte B

col1XB <- unlist(list(mB500, mB500, mB500, mB500, mB500, 
                      mB700, mB700, mB700, mB700, mB700, 
                      mB1000, mB1000, mB1000, mB1000, mB1000))      # Se unifican todas las masas del resorte B
matXB <- matrix (col1XB)                          # Se transforma a una matriz
XB <- cbind (1,matXB)                             # Se añade la columna de 1's

colYB <- unlist(list(kxB500, kxB700, kxB1000))    # Se unifican todas las listas del resorte B
YB <- matrix (colYB) 

XTB <- t(XB)

XTXB <- XTB %*% XB                                # Incertidumbre de la ecuación

Inv_XTXB <- solve(XTXB)

XTXXTB <- Inv_XTXB %*% XTB

BetaB <- XTXXTB %*% YB                            # Resultado de la ecuación


#Cálculo de la regresión para el resorte C

col1XC <- unlist(list(mC50, mC50, mC50, mC50, mC50, 
                      mC20, mC20, mC20, mC20, mC20, 
                      mC10, mC10, mC10, mC10, mC10))      # Se unifican todas las masas del resorte C
matXC <- matrix (col1XC)                          # Se transforma a una matriz
XC <- cbind (1,matXC)                             # Se añade la columna de 1's

colYC <- unlist(list(kxC50, kxC20, kxC10))    # Se unifican todas las listas del resorte C
YC <- matrix (colYC) 

XTC <- t(XC)

XTXC <- XTC %*% XC                                # Incertidumbre de la ecuación

Inv_XTXC <- solve(XTXC)

XTXXTC <- Inv_XTXC %*% XTC

BetaC <- XTXXTC %*% YC                            # Resultado de la ecuación


#Cálculo de la regresión para el resorte D

col1XD <- unlist(list(mD50, mD50, mD50, mD50, mD50, 
                      mD20, mD20, mD20, mD20, mD20, 
                      mD10, mD10, mD10, mD10, mD10))      # Se unifican todas las masas del resorte D
matXD <- matrix (col1XD)                          # Se transforma a una matriz
XD <- cbind (1,matXD)                             # Se añade la columna de 1's

colYD <- unlist(list(kxD50, kxD20, kxD10))    # Se unifican todas las listas del resorte D
YD <- matrix (colYD) 

XTD <- t(XD)

XTXD <- XTD %*% XD                                # Incertidumbre de la ecuación

Inv_XTXD <- solve(XTXD)

XTXXTD <- Inv_XTXD %*% XTD

BetaD <- XTXXTC %*% YD                            # Resultado de la ecuación


#Cálculo de las incertidumbres para la gravedad

res_BalanzaPlato <- runif(Numerodeiteraciones, -0.00000001/2, 0.00000001/2)

res_BalanzaPesaAB <- runif(Numerodeiteraciones, -0.000001/2, 0.000001/2)

res_BalanzaPesaCD <- runif(Numerodeiteraciones, -0.00000001/2, 0.00000001/2)

res_Regla  <- runif(Numerodeiteraciones, -0.001/2, 0.001/2)

cer_PesaAB500 <- rnorm(Numerodeiteraciones, 0, 0.00005)

cer_PesaAB700 <- rnorm(Numerodeiteraciones, 0, 0.00005)

cer_PesaAB1000 <- rnorm(Numerodeiteraciones, 0, 0.0001)

cer_PesaCD50 <- rnorm(Numerodeiteraciones, 0, 0.000007)

cer_PesaCD20 <- rnorm(Numerodeiteraciones, 0, 0.000003)

cer_PesaCD10 <- rnorm(Numerodeiteraciones, 0, 0.000002)

#Resorte A
ecua_A <- rnorm(Numerodeiteraciones, 0, XTXA[2,2]*2) 

res_BalanzaResoA <- runif(Numerodeiteraciones, -0.0000001/2, 0.0000001/2)

cer_ResorteA <- rnorm(Numerodeiteraciones, 0, sd (rest_A) * 2) 

repA500 <- rnorm(Numerodeiteraciones, 0, desv_Elon_A500/sqrt(5))

repA700 <- rnorm(Numerodeiteraciones, 0, desv_Elon_A700/sqrt(5))

repA1000 <- rnorm(Numerodeiteraciones, 0, desv_Elon_A1000/sqrt(5))

#Resorte B
ecua_B <- rnorm(Numerodeiteraciones, 0, XTXB[2,2]*2) 
  
res_BalanzaResoB <- runif(Numerodeiteraciones, -0.00000001/2, 0.00000001/2)

cer_ResorteB <- rnorm(Numerodeiteraciones, 0, sd (rest_B) * 2) 

repB500 <- rnorm(Numerodeiteraciones, 0, desv_Elon_B500/sqrt(5))

repB700 <- rnorm(Numerodeiteraciones, 0, desv_Elon_B700/sqrt(5))

repB1000 <- rnorm(Numerodeiteraciones, 0, desv_Elon_B1000/sqrt(5))

#Resorte C
ecua_C <- rnorm(Numerodeiteraciones, 0, XTXC[2,2]*2) 
  
res_BalanzaResoC <- runif(Numerodeiteraciones, -0.0000001/2, 0.0000001/2)

cer_ResorteC <- rnorm(Numerodeiteraciones, 0, sd (rest_C) * 2) 

repC50 <- rnorm(Numerodeiteraciones, 0, desv_Elon_C50/sqrt(5))

repC20 <- rnorm(Numerodeiteraciones, 0, desv_Elon_C20/sqrt(5))

repC10 <- rnorm(Numerodeiteraciones, 0, desv_Elon_C10/sqrt(5))

#Resorte D
ecua_D <- rnorm(Numerodeiteraciones, 0, XTXD[2,2]*2) 

res_BalanzaResoD <- runif(Numerodeiteraciones, -0.00000001/2, 0.00000001/2)

cer_ResorteD <- rnorm(Numerodeiteraciones, 0, sd (rest_D) * 2) 

repD50 <- rnorm(Numerodeiteraciones, 0, desv_Elon_D50/sqrt(5))

repD20 <- rnorm(Numerodeiteraciones, 0, desv_Elon_D20/sqrt(5))

repD10 <- rnorm(Numerodeiteraciones, 0, desv_Elon_D10/sqrt(5))


#Definición de los valores de la gravedad para cada resorte

gA <- BetaA[2,1] + res_BalanzaPlato + res_BalanzaPesaAB + res_Regla + cer_PesaAB500 + cer_PesaAB700 + cer_PesaAB1000 + ecua_A + res_BalanzaResoA + repA500 + repA700 + repA1000

gB <- BetaB[2,1] + res_BalanzaPlato + res_BalanzaPesaAB + res_Regla + cer_PesaAB500 + cer_PesaAB700 + cer_PesaAB1000 + ecua_B + res_BalanzaResoB + repB500 + repB700 + repB1000

gC <- BetaC[2,1] + res_BalanzaPlato + res_BalanzaPesaCD + res_Regla + cer_PesaCD50 + cer_PesaCD20 + cer_PesaCD10 + ecua_C + res_BalanzaResoC + repC50 + repC20 + repC10

gD <- BetaD[2,1] + res_BalanzaPlato + res_BalanzaPesaCD + res_Regla + cer_PesaCD50 + cer_PesaCD20 +  cer_PesaCD10 + ecua_D + res_BalanzaResoD + repD50 + repD20 + repD10



#Estudio de Correlación y ANOVA

#Datos a analizar de los resorte A y B

gravedad_A500 <- mean(kxA500) / mA500
gravedad_A700 <- mean(kxA700) / mA700
gravedad_A1000 <- mean(kxA1000) / mA1000
gravedad_B500 <- mean(kxB500) / mB500
gravedad_B700 <- mean(kxB700) / mB700
gravedad_B1000 <- mean(kxB1000) / mB1000

datosAB <- matrix(c("A", "A", "A", "B", "B", "B", 
                  mA500, mA700, mA1000, mB500, mB700, mB1000, 
                  gravedad_A500, gravedad_A700, gravedad_A1000, gravedad_B500, gravedad_B700, gravedad_B1000),
                  nrow = 6, ncol = 3)

# Encabezados de filas y columnas
encabezados_filasAB <- c(1, 2, 3, 4, 5, 6)
encabezados_columnasAB <- c("Resorte", "Masa", "Gravedad")

# Asignar encabezados a la matriz
matrizAB <- datosAB
rownames(matrizAB) <- encabezados_filasAB
colnames(matrizAB) <- encabezados_columnasAB

ResorteAB <- matrizAB[,1]
MasaAB <- matrizAB[,2]
GravedadAB <- matrizAB[,3]

matrizAB.lm = lm(GravedadAB ~ ( ResorteAB + MasaAB))

anova(matrizAB.lm)

summary(matrizAB.lm)
plot(matrizAB.lm)

pairwise.t.test(GravedadAB, MasaAB, p.adj = "none")



#Datos a analizar de los resorte C y D
gravedad_C50 <- mean(kxC50) / mC50
gravedad_C20 <- mean(kxC20) / mC20
gravedad_C10 <- mean(kxC10) / mC10
gravedad_D50 <- mean(kxD50) / mD50
gravedad_D20 <- mean(kxD20) / mD20
gravedad_D10 <- mean(kxD10) / mD10

datosCD <- matrix(c("C", "C", "C", "D", "D", "D", 
                    mC50, mC20, mC10, mD50, mD20, mD10, 
                    gravedad_C50, gravedad_C20, gravedad_C10, gravedad_D50, gravedad_D20, gravedad_D10),
                  nrow = 6, ncol = 3)

# Encabezados de filas y columnas
encabezados_filasCD <- c(1, 2, 3, 4, 5, 6)
encabezados_columnasCD <- c("Resorte", "Masa", "Gravedad")

# Asignar encabezados a la matriz
matrizCD <- datosCD
rownames(matrizCD) <- encabezados_filasCD
colnames(matrizCD) <- encabezados_columnasCD

ResorteCD <- matrizCD[,1]
MasaCD <- matrizCD[,2]
GravedadCD <- matrizCD[,3]

matrizCD.lm = lm(GravedadCD ~ ( ResorteCD + MasaCD))

anova(matrizCD.lm)

summary(matrizCD.lm)
plot(matrizCD.lm)

pairwise.t.test(GravedadCD, MasaCD, p.adj = "none")





#Resultado de las constantes de restitución con sus incertidumbres
print("RESULTADOS DE LA CONSTANTE DE RESITUTICIÓN")
print ("CON UN FACTOR DE COBERTURA IGUAL A 2 Y PROBABILIDAD DE 95.45%")

print ("Constante de Restitución del resorte A:")
mean (rest_A)

print("Incertidumbre de la constante de restitución")
sd (rest_A) * 2


print ("Constante de Restitución del resorte B:")
mean (rest_B)

print("Incertidumbre de la constante de restitución")
sd (rest_B) * 2


print ("Constante de Restitución del resorte C:")
mean (rest_C)

print("Incertidumbre de la constante de restitución")
sd (rest_C) * 2


print ("Constante de Restitución del resorte D:")
mean (rest_D)

print("Incertidumbre de la constante de restitución")
sd (rest_D) * 2


#Resultados de la regresión para la gravedad
print("Resultado de la gravedad para el resorte A")
BetaA[2,1]

print("Incertidumbre de la ecuación de la gravedad para el resorte A")
XTXA[2,2]

print("Resultado de la gravedad para el resorte B")
BetaB[2,1]

print("Incertidumbre de la ecuación de la gravedad para el resorte B")
XTXB[2,2]

print("Resultado de la gravedad para el resorte C")
BetaC[2,1]

print("Incertidumbre de la ecuación de la gravedad para el resorte C")
XTXC[2,2]

print("Resultado de la gravedad para el resorte D")
BetaD[2,1]

print("Incertidumbre de la ecuación de la gravedad para el resorte D")
XTXD[2,2]

#Resultados de la fuerza extra no considerada (Intersección con el eje "Y")
print("Resultado de la fuerza extra el resorte A")
BetaA[1,1]

print("Incertidumbre de la ecuación de la fuerza extra para el resorte A")
XTXA[1,2]

print("Resultado de la fuerza extra para el resorte B")
BetaB[1,1]

print("Incertidumbre de la ecuación de la fuerza extra para el resorte B")
XTXB[1,2]

print("Resultado de la fuerza extra para el resorte C")
BetaC[1,1]

print("Incertidumbre de la ecuación de la fuerza extra para el resorte C")
XTXC[1,2]

print("Resultado de la fuerza extra para el resorte D")
BetaD[1,1]

print("Incertidumbre de la ecuación de la fuerza extra para el resorte D")
XTXD[1,2]


print("RESULTADOS DE LA GRAVEDAD")
print ("CON UN FACTOR DE COBERTURA IGUAL A 2 Y PROBABILIDAD DE 95.45%")

print ("Gravedad del resorte A:")
mean (gA)

print("Incertidumbre de la gravedad")
sd (gA) * 2

print ("Gravedad del resorte B:")
mean (gB)

print("Incertidumbre de la gravedad")
sd (gB) * 2

print ("Gravedad del resorte C:")
mean (gC)

print("Incertidumbre de la gravedad")
sd (gC) * 2

print ("Gravedad del resorte D:")
mean (gD)

print("Incertidumbre de la gravedad")
sd (gD) * 2

