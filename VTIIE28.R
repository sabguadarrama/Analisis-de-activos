# clear workspace
rm(list = ls(all = TRUE))

library(fGarch)
library(stargazer)

source("C:/Users/ruths/OneDrive/Escritorio/Catalán Series de Tiempo/Prueba Dickey Fuller Completa.R")

# Cargamos los datos correspondientes al valor de la TIIE28 del año 2018
datos_tiie<-read.csv("C:/Users/ruths/Documents/UNAM/4to semestre/Admin Riesgo/tiie28.csv")

# Aplicamos logaritmos
datos_tiie[,2]<-log(datos_tiie[,2])

# Generamos las diferencias
rend_close<-vector()

for(i in 2:252){
  rend_close[i]<-datos_tiie[i,2]-datos_tiie[(i-1),2]
}

rend_close<-rend_close[2:251]
hist(rend_close, main = "Histograma de rendimientos de TIIE28 durante 2018", xlab = "Rendimientos TIIE28", col="aquamarine2")

# Elevar al cuadrado las diferencias
rend_cuadrado<-vector()
rend2lamb<-vector()
lambda<-.9

for(r in 1:250){
  rend_cuadrado[r]<-rend_close[r]^2
  rend2lamb[r]<-rend_cuadrado[r]*(lambda^(250-r))
}
rend_cuadrado<-rend_cuadrado[1:250]

## Volatilidad Historica
volatilidad_hist<-sqrt(sum(rend_cuadrado)/250)

## Volatilidad dinámica - EWMA
volatilidad_ewma<-sqrt(sum(rend2lamb)*(1-lambda))

# Ahora se buscara la volatilidad por los modelos ARCH
# Realizamos pruebas de raíz unitaria (Dickey-Fuller)
dickeyfuller(rend_close,3,"AIC")

## Estimamos un ARCH(1)
m1 <- garchFit(~garch(1,0), data = rend_close, trace = FALSE)
summary(m1)

## Estimamos un GARCH(1,1)
m2<-garchFit(~garch(1,1), data = rend_close, trace = FALSE)
summary(m2)

stargazer(m1,m2,type="text", report=('vc*p'))

### De lo anterior vemos que el modelo 1 es el más adecuado
### Obtenemos los residuales de este modelo

residuos_garch<-m1@residuals

### Obtenemos a su vez la desv estandar y la varianza estimada por el modelo 
des_estandar<-volatility(m1,type="sigma")
varianza<-volatility(m1,type="h")

### Estimamos la volatilidad (ARCH)
m1@fit$coef

volatilidad_arch<-sqrt(m1@fit$coef[[2]]+m1@fit$coef[[3]]*(residuos_garch[249])^2)