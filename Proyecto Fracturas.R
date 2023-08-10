
## Librerias de Trabajo
  
library(spatstat)
library(spatstat.data)
library(readxl)
library(dplyr)
library(tidyverse)
library(scatterplot3d)
library(akima)
library(gstat)
library(geoR)
library(lattice)
library(maptools)
library(sp)
library(spatial)
library(graphics)
library(aplpack)
library(fields)
library(rgdal)
library(raster)
library(randomcoloR)
library(spdep)
require(RColorBrewer)
library(mapview)
library(readxl)
library(prettymapr)
library(readxl)
library(rgeos)
library(rgdal)
library(dbmss)
library(deldir)
library(spatstat.utils)
library(GET)



## Base de Datos

library(readxl)
library(knitr)
#Datos agrupados por familia

Datos2 <- read_excel("D:/DiscoD/TRABAJO DE GRADO/CAJON/Familia Total Cajon.xlsx")



##Coordenadas y ventana de trabajo 



W <- owin(range(Datos2$X), range(Datos2$Y))
W<- owin(c(0,9),c(0,5))
ppxy <- ppp(x = Datos2$X, y = Datos2$Y, owin(W),marks = factor(Datos2$Familia))
plot.ppp(ppxy, which.marks = "Familia",)
points(ppxy, pch = 20,cex = 0.4, col=Datos2$Indice)
legend(x = 19, y = 13,legend=c("Familia 1", "Familia 2", "Familia 3"),
       fill=c("red", "blue", "black"), cex=0.7)


## Intensidad por cuadrantes
cuadr <- quadratcount(ppxy,nx=6,ny=6)
plot(cuadr, main="Conteo Cuadrantes")
intcuadr<-intensity.quadratcount(cuadr,image=T)
plot(intcuadr,main="Intensidad por cuadrantes", axes=T, las=1)
```

### ESTADÍSTICAS PRIMER ORDEN

### Prueba chi cuadrado 
qtchi<-quadrat.test(ppxy, nx=6,ny=6,method="Chisq")
print(qtchi)

#Test Monte Carlo
Mont<-quadrat.test(ppxy, nx=6,ny=6,method="MonteCarlo", nsim = 1000)
print(Mont)

### Intensidad no parametrica

# se utilizá un kernel gaussiano isotrópico, con un valor predeterminado de sigma calculado mediante una regla básica que depende únicamente del tamaño de la ventana, use la 
# corrección de bordes mejorada de Jones-Diggle, que es más precisa que la correccion predeterminada.


densidad<-density(ppxy, kernel = "gaussian", diggle=TRUE, adjust=1)
plot(densidad, main="Intensidad no parametrica",axes=T,las=1)
contour(densidad,add=T)


## Analisis a travez del punto medio

## Estadisticas de Segundo Orden

# Todas las familias con el punto medio
library(readxl)
familia <- read_excel("D:/DiscoD/TRABAJO DE GRADO/CAJON/Pto Medio Cajon.xlsx", 
                      sheet = "Todos")

##Coordenadas y ventana de trabajo

W1 <- owin(range(familia$X_m), range(familia$Y_m))
W1<- owin(c(0,9),c(0,5))
ppxy_puntom<- ppp(x = familia$X_m, y = familia$Y_m, owin(W1),marks = factor(familia$Nombre))
plot.ppp(ppxy_puntom, which.marks = "Nombre")
points(ppxy_puntom, pch = 20, cex = 1, col=familia$Total)


k<-KinhomEnvelope(as.wmppp(ppxy_puntom) ,lambda = densidad,SimulationType = 
                    "RandomPosition")
#plot(k)

res<-global_envelope_test(k, type="erl")
plot(res)


# Ahora debo segmentar las familias de puntos medios para realizar 
#un analisis de correlacion segmentado por familias y luego segmentado 
#por marcas

### familias como marcas

familia$Total<-as.factor(familia$Total)

Con1 <- familia %>% filter(familia$Total=="2") 
Con2 <- familia %>% filter(familia$Total=="4") 
Con3 <- familia %>% filter(familia$Total=="1")

dat_Con1=data.frame(X=Con1$X_m,Y=Con1$Y_m)
dat_sp1_pm = SpatialPoints(dat_Con1[c("X", "Y")])

dat_Con2=data.frame(X=Con2$X_m,Y=Con2$Y_m)
dat_sp2_pm = SpatialPoints(dat_Con2[c("X", "Y")]) 

dat_Con3=data.frame(X=Con3$X_m,Y=Con3$Y_m)
dat_sp3_pm = SpatialPoints(dat_Con3[c("X", "Y")]) 


par(mfrow=c(1,3))

plot(W1 ,main="Familia 1")
points(dat_sp1_pm, pch = 1, cex = 0.6, col="red", bg="green")
plot(W1 ,main="Familia 2")
points(dat_sp2_pm, pch = 1, cex = 0.6, col="blue", bg="green")
plot(W1 ,main="Familia 3")
points(dat_sp3_pm, pch = 1, cex = 0.6, col="black", bg="green")


pp_pm1 = as.ppp(dat_Con1, owin(W1))
pp_pm1 = unique(pp_pm1)

pp_pm2 = as.ppp(dat_Con2, owin(W1))
pp_pm2 = unique(pp_pm2)

pp_pm3 = as.ppp(dat_Con3, owin(W1))
pp_pm3 = unique(pp_pm3)


#Familias como Marcas

Datos2$Familia<-as.factor(Datos2$Familia)

Con1 <- Datos2 %>% filter(Datos2$Familia=="Rojo") 
Con2 <- Datos2 %>% filter(Datos2$Familia=="Azul") 
Con3 <- Datos2 %>% filter(Datos2$Familia=="Negro")

dat_Con1=data.frame(X=Con1$X,Y=Con1$Y)
dat_sp1 = SpatialPoints(dat_Con1[c("X", "Y")])

dat_Con2=data.frame(X=Con2$X,Y=Con2$Y)
dat_sp2 = SpatialPoints(dat_Con2[c("X", "Y")]) 

dat_Con3=data.frame(X=Con3$X,Y=Con3$Y)
dat_sp3 = SpatialPoints(dat_Con3[c("X", "Y")])



plot(W ,main="Familia 1")
points(dat_sp1, pch = 1, cex = 0.6, col="red", bg="green")
plot(W ,main="Familia 2")
points(dat_sp2, pch = 1, cex = 0.6, col="blue", bg="green")
plot(W ,main="Familia 3")
points(dat_sp3, pch = 1, cex = 0.6, col="black", bg="green")

## Familia 1

mapa_ppp_s = as.ppp(dat_Con1, owin(W))
mapa_ppp_s = unique(mapa_ppp_s)
# creación de los cuadrantes
mapa_cont_s = quadratcount.ppp(mapa_ppp_s, nx=6, ny=6, image=TRUE)
#Intensidad a traves de cuadrantes
mapa_intensidad_s = intensity(mapa_cont_s, image=TRUE)         

#par(mfrow = c(1,2))
plot(mapa_ppp_s, pch = 21, cex = 0.6,  bg="cyan", main="Intensidad por zona control")
plot(mapa_cont_s,col="red", add=T)
plot(mapa_intensidad_s, main ="Intensidad Cuadrantes Control")
#plot(mapa_intensidad_s, main ="Intensidad Cuadrantes Control", col = cols1)

### Estadisticas de primer orden

#### Pruebas de Homogeneidad

mapa_tes_X2_s2 = quadrat.test(mapa_ppp_s, nx = 6, ny = 6, method = "Chisq") 
mapa_tes_X2_s2

mapa_tes_monte_s2 = quadrat.test(mapa_ppp_s, nx = 6, ny = 6, method = "MonteCarlo",nsim=1000) #Pueba de homogeneidad Monte Carlo
mapa_tes_monte_s2

#### Estimación de la intensidad
densidad_con = density.ppp(mapa_ppp_s, kernel = "gaussian", 
                           diggle=TRUE, adjust=1)

plot(densidad_con,main="Mapa de contorno", las=1, axes= T)
contour(densidad_con,add=T)
#plot(densidad_con2,main="Mapa con patrones")
points(dat_sp1, pch = 25, cex = 0.6, col="blue", bg="green")
#persp(densidad_con2,xlab = "", ylab ="" , zlab = "Intensidad", main="Estimación Intensidad")


#### Estadisticas de segundo orden

#funcion k de ripley
k<-KinhomEnvelope(as.wmppp(pp_pm1) ,lambda = densidad_con,SimulationType = "RandomPosition")
plot(k)

res<-global_envelope_test(k, type="erl")
plot(res)


## Familia 2

mapa_ppp_s2 = as.ppp(dat_Con2, owin(W))
mapa_ppp_s2 = unique(mapa_ppp_s2)
# creación de los cuadrantes
mapa_cont_s2 = quadratcount.ppp(mapa_ppp_s2, nx=6, ny=6, image=TRUE)
#Intensidad a traves de cuadrantes
mapa_intensidad_s2 = intensity(mapa_cont_s2, image=TRUE)          


#inicio_par = par()
par(mfrow = c(1,2))
plot(mapa_ppp_s2, pch = 21, cex = 0.6,  bg="cyan", main="Intensidad por zona control")
plot(mapa_cont_s2,col="red", add=T)
plot(mapa_intensidad_s2, main ="Intensidad Cuadrantes Control" ) 


### Estadisticas de primer orden
#### Pruebas de Homogeneidad

mapa_tes_X2_s = quadrat.test(mapa_ppp_s2, nx = 6, ny = 6, method = "Chisq") 
mapa_tes_X2_s

mapa_tes_monte_s2 = quadrat.test(mapa_ppp_s2, nx = 6, ny = 6, method = "MonteCarlo",nsim=1000) #Pueba de homogeneidad Monte Carlo
mapa_tes_monte_s2

#### Estimación de la intensidad


densidad_con2 = density.ppp(mapa_ppp_s2, sigma=0.8392981 , kernel = "epanechnikov", diggle=TRUE,
                            positive=TRUE)

plot(densidad_con2,main="Mapa de contorno", las=1, axes=T)
contour(densidad_con2,add=T)



#### Estadisticas de segundo orden

#funcion k de ripley
k<-KinhomEnvelope(as.wmppp(pp_pm2) ,lambda = densidad_con,SimulationType = "RandomPosition")
plot(k)

res<-global_envelope_test(k, type="erl")
plot(res)


## Familia 3

mapa_ppp_s3 = as.ppp(dat_Con3, owin(W))
mapa_ppp_s3 = unique(mapa_ppp_s3)
# creación de los cuadrantes
mapa_cont_s3 = quadratcount.ppp(mapa_ppp_s3, nx=8, ny=8, image=TRUE)
#Intensidad a traves de cuadrantes
mapa_intensidad_s3 = intensity(mapa_cont_s3, image=TRUE)          

#inicio_par = par()
par(mfrow = c(1,2))
plot(mapa_ppp_s3, pch = 21, cex = 0.6,  bg="cyan", main="Intensidad por zona control")
plot(mapa_cont_s3,col="red", add=T)
plot(mapa_intensidad_s3, main ="Intensidad Cuadrantes Control" )


### Estadisticas de primer orden
#### Pruebas de Homogeneidad

mapa_tes_X2_s = quadrat.test(mapa_ppp_s3, nx = 8, ny = 8, method = "Chisq") 
mapa_tes_X2_s

mapa_tes_monte_s3 = quadrat.test(mapa_ppp_s3, nx = 8, ny = 8, method = "MonteCarlo",nsim=1000) #Pueba de homogeneidad Monte Carlo
mapa_tes_monte_s3

#### Estimación de la intensidad

#### Estimación de la intensidad

densidad_con3 = density.ppp(mapa_ppp_s3, 
                            kernel = "gaussian", diggle=TRUE)

#par(mfrow=c(1,2))
plot(densidad_con3,main="Mapa de contorno", axes= T, las=1)
contour(densidad_con3,add=T)
#plot(densidad_con3,main="Mapa con patrones")
#points(dat_sp3, pch = 25, cex = 0.6, col="blue", bg="green")

#persp(densidad_con3,xlab = "", ylab ="" , zlab = "Intensidad", main="Estimación Intensidad")

#### Estadisticas de segundo orden


#funcion k de ripley
k<-KinhomEnvelope(as.wmppp(pp_pm3) ,lambda = densidad_con,SimulationType = "RandomPosition")
plot(k)

res<-global_envelope_test(k, type="erl")
plot(res)


### Magnitud de las discuntinuidades como marca



CA<-cut(familia$Longitud,breaks=c(0,2,10),labels=c("Baja","Alta"))
familia$CA<-as.factor(CA)


Con1 <- familia %>% filter(CA=="Baja") 
Con2 <- familia %>% filter(CA=="Alta") 

dat_Con1=data.frame(X=Con1$X_m,Y=Con1$Y_m)
dat_sp1 = SpatialPoints(dat_Con1[c("X", "Y")]) 

dat_Con2=data.frame(X=Con2$X_m,Y=Con2$Y_m)
dat_sp2 = SpatialPoints(dat_Con2[c("X", "Y")]) 

#par(mfrow=c(1,2))
plot(W,main=NULL)
points(dat_sp1, pch = 1, cex = 0.6, col="blue", bg="green")

plot(W,main=NULL)
points(dat_sp2, pch = 2, cex = 0.6, col="red", bg="green")


# Magnitud Alta

mapa_ppp_s = as.ppp(dat_Con1, owin(W))
mapa_ppp_s = unique(mapa_ppp_s)
# creación de los cuadrantes
mapa_cont_s = quadratcount.ppp(mapa_ppp_s, nx=6, ny=6, image=TRUE)
#Intensidad a traves de cuadrantes
mapa_intensidad_s = intensity(mapa_cont_s, image=TRUE)          

plot(mapa_ppp_s, pch = 21, cex = 0.6,  bg="cyan", main="Intensidad por zona control")
plot(mapa_cont_s,col="red", add=T)
plot(mapa_intensidad_s, main ="Intensidad Cuadrantes Control" )
```

### Estadisticas de primer orden


#### Pruebas de Homogeneidad
mapa_tes_X2_s = quadrat.test(mapa_ppp_s, nx = 6, ny = 6, method = "Chisq") 
mapa_tes_X2_s


#Pueba de homogeneidad Monte Carlo
mapa_tes_monte_s = quadrat.test(mapa_ppp_s, nx = 6, ny = 6, method = "MonteCarlo",nsim=1000)
mapa_tes_monte_s


### Estimación de la intensidad 

densidad_con = density(mapa_ppp_s, kernel = "gaussian", 
                       diggle=TRUE, adjust=1)

plot(densidad_con,main=NULL, las=1, axes=T)
contour(densidad_con,add=T)


### función K de ripley

#krypley homogenea
khom <- envelope(mapa_ppp_s, fun= Kest, nsim= 100, verbose=F)
plot(khom)

k<-Kest(mapa_ppp_s )
plot(k)

#Correlacion por pares

# Magnitud baja

mapa_ppp_s2 = as.ppp(dat_Con2, owin(W))
mapa_ppp_s2 = unique(mapa_ppp_s2)
# creación de los cuadrantes
mapa_cont_s2 = quadratcount.ppp(mapa_ppp_s2, nx=6, ny=6, image=TRUE)
#Intensidad a traves de cuadrantes
mapa_intensidad_s2 = intensity(mapa_cont_s2, image=TRUE)          

#inicio_par = par()
#par(mfrow = c(1,2))
plot(mapa_ppp_s2, pch = 21, cex = 0.6,  bg="cyan", main="Intensidad por zona control")
plot(mapa_cont_s2,col="red", add=T)
plot(mapa_intensidad_s2, main ="Intensidad Cuadrantes Control" )



### Estadisticas de primer orden

#### Pruebas de Homogeneidad

mapa_tes_X2_s2 = quadrat.test(mapa_ppp_s2, nx = 6, ny = 6, method = "Chisq") 
mapa_tes_X2_s2

mapa_tes_monte_s2 = quadrat.test(mapa_ppp_s2, nx = 6, ny = 6, method = "MonteCarlo",nsim=1000) #Pueba de homogeneidad Monte Carlo
mapa_tes_monte_s2

clark<-clarkevans.test(mapa_ppp_s2,nsim=1000)
clark
```

### Estimación de la intensidad

#estimación del sigma 
bw.ppl(mapa_ppp_s2)
#264,9707


densidad_con2 = density(mapa_ppp_s2, kernel = "gaussian", 
                        diggle=TRUE, adjust=1)
plot(densidad_con2,main=NULL, las=1, axes= T)
contour(densidad_con2,add=T)
points(dat_sp2, pch = 25, cex = 0.6, col="blue", bg="green")

### función K de ripley

#krypley homogenea
khom <- envelope(mapa_ppp_s2, fun= Kest, nsim= 100, verbose=F)
plot(khom)

k<-Kest(mapa_ppp_s2)
plot(k)

#Correlacion por pares
#PCF

### Funciones K Bivariadas



par(mfrow = c(1,3))

kcruz1<-Kcross.inhom(ppxy,"Azul", "Rojo",lambdaI = densidad_con2,lambdaJ = densidad_con)
kcruz2<-Kcross.inhom(ppxy,"Azul", "Negro",lambdaI = densidad_con,lambdaJ = densidad_con3)
kcruz3<-Kcross.inhom(ppxy,"Negro", "Rojo",lambdaI = densidad_con3,lambdaJ = densidad_con2)


plot(kcruz1)
plot(kcruz2)
plot(kcruz3)

kcruz_env1<-envelope(ppxy,fun = "Kcross.inhom",nsim = 10,level=0.95 ,savefuns = TRUE,
                     i="Azul", j="Rojo",lambdaI = densidad_con2,lambdaJ = densidad_con)

#plot(kcruz_env1)

kcruz_env2<-envelope(ppxy_puntom,fun = "Kcross.inhom",nsim = 10,level=0.95 ,savefuns = TRUE,
                     i="Familia 2", j="Familia 3",lambdaI = densidad_con2,lambdaJ = densidad_con3)

#plot(kcruz_env2)

kcruz_env3<-envelope(ppxy_puntom,fun = "Kcross.inhom",nsim = 100,level=0.95 ,savefuns = TRUE,
                     i="Familia 3", j="Familia 1",lambdaI = densidad_con3,lambdaJ = densidad_con)

#plot(kcruz_env3)


