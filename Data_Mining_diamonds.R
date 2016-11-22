## INSTRUCCIONES PARA LA REALIZACI?N DE LA PR?CTICA ##
# cada uno de los puntos a realizar son independientes, 
# esto es, se puede trabajar con todo el conjunto o con una muestra representativa
# Se trata de evaluar vuestro conocimiento en la interpretaci?n de los resultados.


############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendr? el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relaci?n al punto m?s ancho 

# Limpiar el workspace
rm(list = ls(all = TRUE))

#Cargamos librerias
library(ggplot2)
library(sampling)
library(ggm)

getwd()
setwd("//ruta")

#Cargamos el data frame y revisamos su composicion
df<-as.data.frame(diamonds)
attach(df)
head(df)
str(df)
summary(df)
num_diamonds<-dim(df)[1]
num_variables<-dim(df)[2]



########Muestra representativa##########
# 1. Selecciona una muestra representativa para "cut"

#Analizamos la variable cut
str(cut) # Vemos que es una cualitativa ordinal, un factor ordinal
levels(cut) # Cuyos levels son "Fair"<"Good"<"Very Good"<"Premium"<"Ideal"
head(df,10) # Vemos los 10 primeros valores para hacernos una idea


#Analizamos la frecuencia de los valores de la variable en la poblacion
# En datos absolutos
summary(cut) # tambien con table(cut)
# En probabilidad (muy util)
prop.table(table(cut))


# Podemos escoger de los 53940 diamantes una muestra aleatoria representativa escogida al azar 
# que se aproxime a la frecuencia de los valores de cut en toda la poblacion
# utilizando para ello sample().
# Lo importante es elegir el tamaÃ±o minimo de la muestra para que sea representativo.
# Para ello nos basaremos en el numero minimo de muestra que debe cumplir:
#           n = N*Z^2*p*q/d^2*(N-1)+Z^2*p*q
# siendo:
#     p: proporcion del fenomeno del estudio = 0.2 (1/5 pos.valores)
#     q:1-p=0.8
#     N: tamaÃ±o de la poblacion = num_diamonds
#     Z: nivel de confianza al 95% = 1.96
#     d: nivel de precision (en nuestro caso 0.04)

z_cuadrado<-1.96^2
p<-0.2
q<-1-p
d_cuadrado<-0.04^2
tam_muestra_min<-(num_diamonds*z_cuadrado*p*q)/(d_cuadrado*(num_diamonds-1)+z_cuadrado*(p*q))

tam_muestra_min #El tamaÃ±o de la muestra minimo para que sea fiable es 381 elementos

#Recogemos la muestra
muestra_aleatoria<-sample(cut,tam_muestra_min)

#Comprobamos que la frecuencia de valores para la muestra es similar al de la poblacion
layout(matrix(1:2, nrow = 1))
pie(table(muestra_aleatoria), main = "tarta muestra")
pie(table(cut), main = "tarta poblacion")

barplot(table(muestra_aleatoria), main = "barplot muestra")
barplot(table(cut), main = "barplot poblacion")

prop.table(table(muestra_aleatoria))
prop.table(table(cut))


# Otra forma de hacer el muestreo de forma mas eficiente seria hacerlo por estratos.
# Como sabemos la proporcion de los valores de cut sobre la poblacion necesitamos
# coger estratos con esa proporcion
# Multiplicamos cada probabilidad por el tamaÃ±o de la muestra minimo
# Con esto conseguimos tener un vector de cantidades a recoger para cada level de cut
freq_cut<-sapply(prop.table(table(cut)),function(x){x*tam_muestra_min})
freq_cut

#Tenemos que ordenar diamods por tipo de corte
# Y redondear las frecuencias
estratos <- strata( diamonds[order(diamonds$cut),], stratanames = c("cut"), size = round(freq_cut), method = "srswor" )
muestra_estratificada <- getdata( cut, estratos )
head(muestra_estratificada)

# Comprobamos que los estratos estan bien repartidos
pie(table(muestra_estratificada$cut), main = "tarta muestra est.")
pie(table(cut), main = "tarta poblacion")

barplot(table(muestra_estratificada$cut), main = "barplot muestra est.")
barplot(table(cut), main = "barplot poblacion")

prop.table(table(muestra_estratificada$cut))
prop.table(table(cut))

# Conclusion: Como vemos la muestra estratificada esta muchisimo mas ajustada. Puesto que partimos 
# de las proporciones. Nos quedariamos con esta muestra.
# Observacion hemos cogido 381 elementos, pero podiamos haber cogido un tamaño mayor;381 es el tamño minimo.

layout(matrix(1:1, nrow = 1))



#######An?lisis de las variables########
# 2. An?lisis descriptivo de las variables: Tipo de variable, distribuci?n y representaci?n
# y Detecci?n de casos at?picos y su tratamiento

# Lo primero es ver los valores de las variables. Nos interesa encontrar valores nulos
summary(df)
df[!complete.cases(df),] # Parece que no hay

# Podemos revisar uno a uno:
df[is.na(price),]
df[is.na(carat),]
df[is.na(color),]
df[is.na(clarity),]
df[is.na(depth),]
df[is.na(table),]
df[is.na(x),]
df[is.na(y),]
df[is.na(z),]
#.... No hay en ninguno
# El (Other de clarity significa otros valores)

# a) price: Precio en dolares americanos
str(price) # Variable cuantitativa continua
summary(price) # Valores maximos muy altos con respecto a la media
sd(price) # Confirmamos que tiene una dispersion muy alta
boxplot(price) # Tiene muchisimos valores atipicos fuera del bigote superior
#¿Cuantos?
maximo<-quantile(price,.75)+1.5*IQR(price)
num_val_atipicos<-length(price[price>maximo]) #3540 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 6,5% de la muestra

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(price) # Cuadra perfectamente

# Revisamos su distribucion
hist(price)
plot(density(price)) # Es claramente una lognormal--> Lo cual es logico que tenga muchos valores atipicos

# La tratamos como una lognormal
log_price<-log(price)
boxplot(log_price) # Se han ido los valores atipicos
hist(log_price) # Ahora parece una normal

# Histograma final, con agrupaciones
hist(log_price, breaks=c(5,6.5,8.5,10), main = "Histograma log(precio)", xlab = "log(precio)")
lines(density(log_price))

#CONCLUSION: Precio es una variable cuantitativa continua que sigue una distribucion
# lognormal. Ni mucho menos se pueden quitar los valores atipicos. Se debe tratar con el log(price)
# ya que esta funcion conservaria la media y varianza de la funcion original y elimina los valores atipicos.

# b) carat: peso del diamante
str(carat) # Variable cuantitativa continua
summary(carat) 
# Valores entre 0.2 y 5, pero realmente la mayoria estan entre 0.4 y 1
# Valores maximos muy altos con respecto a la media, los bajos son relativamente cercanos
sd(carat) # Tiene una dispersion relativamente alta
boxplot(carat) # Tiene muchisimos valores atipicos fuera del bigote superior
#¿Cuantos?
maximo<-quantile(carat,.75)+1.5*IQR(carat)
num_val_atipicos<-length(carat[carat>maximo]) #1889 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 3,5% de la muestra

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(carat) # Pasa lo mismo que con price, esta vez mas pronunciado en los quantiles altos

# Revisamos su distribucion
hist(carat)
plot(density(carat)) # Parece tambien una lognormal

# La tratamos como una lognormal
log_carat<-log(carat)
boxplot(log_carat) # Efectivamente tratantolo como una lognormal solo nos quedan 2 valores atipicos asumibles.
hist(log_carat) # Ahora parece una normal

# Histograma final, con agrupaciones
hist(log_carat, breaks=c(-2,-1,-0.5,0,0.5,1.5,2), main = "Histograma log(carat)", xlab = "log(carat)")
lines(density(log_carat))

#CONCLUSION: carat es una variable cuantitativa continua que sigue una distribucion
# lognormal. Ni mucho menos se pueden quitar los valores atipicos. Se debe tratar con el log(carat)
# ya que esta funcion conservaria la media y varianza de la funcion original y 
# deja solo 2 valores atipicos que son asumibles. Se debe ver a posteriori si infieren o no en la regresion




# c) cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
str(cut) # Variable cualitativa discreta: Factor ordinal--> Fair<Good<Very Good<Premium<Ideal
levels(cut)
summary(cut) # Vemos el numero de elementos por categoria

# Revisamos proporcion: Tenemos mayor proporcion segun el orden
pie(prop.table(table(cut))) 
barplot(prop.table(table(cut)))
barplot(cumsum(prop.table(table(cut))))

# Obviamente no tiene valores atipicos, es un factor ordinal
# Aunque no tenga mucho sentido se puede plantear el boxplot para demostrarlo
boxplot(cut)
# Se denota que la media de diamantes seria "Premium"

#CONCLUSION: Variable cualitativa ordinal.Cada una de las categorias sigue una
# distribucion bernuilli. 

# d) colour: color del diamante (desde D el mejor hasta J el peor)

str(color) # Variable cualitativa discreta: Factor ordinal--> D<E<F<G<H<I<J
levels(color)
summary(color) # Vemos el numero de elementos por categoria

# Revisamos proporcion: Vemos que tenemos una dispersion mayor a cut. 
# En este caso el orden no indica mayor frecuencia
pie(prop.table(table(color))) 
barplot(prop.table(table(color)))
barplot(cumsum(prop.table(table(color))))

# Obviamente no tiene valores atipicos, es un factor ordinal
# Aunque no tenga mucho sentido se puede plantear el boxplot para demostrarlo
boxplot(color)
# Se denota que la media de diamantes tendria color "G". No aporta mucha informacion

#CONCLUSION: Variable cualitativa.Cada una de las categorias sigue una
# distribucion bernuilli. Y en general sigue una distribucion normal teniendo 
# en cuenta el orden de sus levels


# d) clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)

str(clarity) # Variable cualitativa discreta: Factor ordinal--> I1<SI2<SI1<VS2<VS1<VVS2<VVS1<IF
levels(clarity)
summary(clarity) # Vemos el numero de elementos por categoria

# Revisamos proporcion: Vemos que tenemos una dispersion mayor a cut. 
# En este caso el orden no indica mayor frecuencia
pie(prop.table(table(clarity))) 
barplot(prop.table(table(clarity)))
barplot(cumsum(prop.table(table(clarity))))

# Obviamente no tiene valores atipicos, es un factor ordinal
# Aunque no tenga mucho sentido se puede plantear el boxplot para demostrarlo
boxplot(clarity)
# Se denota que la media de diamantes tendria claridad "VS2". No aporta mucha informacion

#CONCLUSION: Variable cualitativa.Cada una de las categorias sigue una
# distribucion bernuilli. Y en general sigue una distribucion normal teniendo 
# en cuenta el orden de sus levels




# e) x: longitud en mm 

str(x) # Variable cuantitativa continua
summary(x) 
# Valores entre 0 y 10, pero realmente la mayoria estan entre 4.7 y 6.5
# Valores maximos y minimos muy altos/bajos con respecto a la media
sd(x) # Tiene una dispersion relativamente alta
boxplot(x) # Tiene muchisimos valores atipicos fuera del bigote superior, y algunos en el bigote inferior

# No tiene ningun sentido un diamante de 0mm de longitud--> Ha sido un error en la toma del dato
# Lo eliminamos de la muestra
length(which(x==0)) # 8 valores atipicos
df<-subset(df,x!=0) # Los eliminamos
num_diamonds<-dim(df)[1] # Y ahora tenemos 53932 diamantes
attach(df)

#Revisamos 
summary(x) # Ahora no tenemos valores atipicos por debajo 
sd(x) # Menor dispersion
boxplot(x)

#¿Cuantos tiene en el bigote superior?
maximo<-quantile(x,.75)+1.5*IQR(x)
num_val_atipicos<-length(x[x>maximo]) #24 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 0.044% de la muestra
# Es un porcentaje muy bajo como para quitarlos y estan la mayoria muy cerca del maximo

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(x) # Confirmamos los valores atipicos altos

# Revisamos su distribucion
hist(x)
plot(density(x)) # Parece tambien una lognormal

# La tratamos como una lognormal
log_x<-log(x)
boxplot(log_x) # Se han ido los valores atipicos
hist(log_x) # Ahora parece una normal

# Histograma final, con agrupaciones
hist(log_x, breaks=c(1,1.5,1.7,2,2.2,2.4), main = "Histograma log(x)", xlab = "log(x)")
lines(density(log_x))

#CONCLUSION: Variable cuantitativa continua que sige una distribucion normal o incluso lognormal.
# Se ha hecho un tratamiento para quitar los valores atipicos minimos (por no tener logica), 
# los valores atipicos maximos no los vamos a quitar(porcentaje muy bajo), si en algun momento
# nos molestan podemos utilizar el logaritmo para ajustarlos




# f) y: ancho en  mm 

str(y) # Variable cuantitativa continua
summary(y) 
# Valores entre 4.6 y 58.9, pero realmente la mayoria estan entre 4.7 y 6.5
# Valores maximos muy altos con respecto a la media
sd(y) # Tiene una dispersion relativamente alta
boxplot(y) # Tiene muchisimos valores atipicos fuera del bigote superior

# No tiene ningun sentido un diamante de mas de 30mm de longitud--> Ha sido un error en la toma del dato
# Lo eliminamos de la muestra
length(which(y>30)) # 2 valores atipicos
df<-subset(df,y<30) # Los eliminamos
num_diamonds<-dim(df)[1] # Y ahora tenemos 53930 diamantes
attach(df)

#Revisamos 
summary(y) # Ahora no tenemos valores atipicos tan altos
sd(y) # Menor dispersion
boxplot(y)

#¿Cuantos quedan?
maximo<-quantile(y,.75)+1.5*IQR(y)
num_val_atipicos<-length(y[y>maximo]) #20 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 0.037% de la muestra
# Es un porcentaje muy bajo como para quitarlos y estan la mayoria muy cerca del maximo

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(y) # Confirmamos los valores atipicos altos

# Revisamos su distribucion
hist(y)
plot(density(y)) # Parece tambien una lognormal

# La tratamos como una lognormal
log_y<-log(y)
boxplot(log_y) # Se han ido los valores atipicos
hist(log_y) # Ahora parece una normal

# Histograma final, con agrupaciones
hist(log_y, breaks=c(1,1.5,1.7,2,2.2,2.4), main = "Histograma log(y)", xlab = "log(y)")
lines(density(log_y))

#CONCLUSION: Variable cuantitativa continua que sige una distribucion normal o incluso lognormal.
# Se ha hecho un tratamiento para quitar los valores atipicos muy altos (por no tener logica), 
# los valores atipicos cercanos al maximo no los vamos a quitar(porcentaje muy bajo), si en algun momento
# nos molestan podemos utilizar el logaritmo para ajustarlos


# g) z: profundidad en mm 


str(z) # Variable cuantitativa continua
summary(z) 
# Valores entre 0 y 31, pero realmente la mayoria estan entre 2.9 4.04
# Valores maximos/minimos muy altos/bajos con respecto a la media
sd(z) 
boxplot(z) # Tiene muchisimos valores atipicos fuera del bigote superior

# No tiene ningun sentido un diamante de mas de 30mm de profundida o 0mm--> Ha sido un error en la toma del dato
# Lo eliminamos de la muestra
length(which(z>30|z==0)) # 2 valores atipicos
df<-subset(df,z<30&z!=0) # Los eliminamos
num_diamonds<-dim(df)[1] # Y ahora tenemos 53917 diamantes
attach(df)

#Revisamos 
summary(z) # Ahora no tenemos valores atipicos tan altos
sd(z) # Menor dispersion
boxplot(z)

#¿Cuantos quedan?
maximo<-quantile(z,.75)+1.5*IQR(z)
minimo<-quantile(z,.25)-1.5*IQR(z)
num_val_atipicos<-length(z[z>maximo|z<minimo]) #27 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 0.05% de la muestra
# Es un porcentaje muy bajo como para quitarlos y estan la mayoria muy cerca del maximo

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(z) # Confirmamos los valores atipicos altos

# Revisamos su distribucion
hist(z)
plot(density(z)) # Distribucion Normal


#CONCLUSION: Variable cuantitativa continua que sige una distribucion normal.
# Se ha hecho un tratamiento para quitar los valores atipicos muy altos o 0 (por no tener logica), 
# los valores atipicos cercanos al maximo o minimo no los vamos a quitar(porcentaje muy bajo), si en algun momento
# nos molestan podemos utilizar el logaritmo para ajustarlos

# h) depth: porcentaje total de profundidad 

str(depth) # Variable cuantitativa continua (Probabilidades)
summary(depth) 
# Valores entre 43% y 79%, pero realmente la mayoria estan entre 61% y 62,5%
# Valores maximos y minimos muy altos/bajos con respecto a la media
sd(depth) # Tiene una dispersion alta
boxplot(depth) # Tiene muchisimos valores atipicos fuera del bigote superior e inferior

#¿Cuantos?
maximo<-quantile(depth,.75)+1.5*IQR(depth)
minimo<-quantile(depth,.25)-1.5*IQR(depth)
num_val_atipicos<-length(depth[depth>maximo|depth<minimo]) #2543 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 4.7% de la muestra
# Es un porcentaje muy alto como para quitarlos y estan la mayoria muy cerca del maximo y del minimo

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(depth) # Confirmamos los valores atipicos altos

# Revisamos su distribucion
hist(depth)
plot(density(depth)) # Es una distribucion normal pero tiene muchos outlayers

# Lo mejor es reclasificarlo
df$depth.clasif<-ifelse(depth<57, "<57%", 
                  ifelse(depth<60, "57-60%",
                      ifelse(depth<62, "60-62%",
                          ifelse(depth<64, "62-64%",
                          ">64%"))))
attach(df)

summary(factor(depth.clasif))
prop.table(table(depth.clasif))
pie(prop.table(table(depth.clasif))) 
barplot(prop.table(table(depth.clasif)))
# De esta forma teniendolas ordenadas tendriamos perfectamente una distribucion normal
# E incluyendo a todos los outlayers


#CONCLUSION: Variable cuantitativa continua que sige una distribucion normal.
# Se ha hecho una reclasificacion para incluir todos los outlayers puesto que tenian una porcentaje
# muy alto y por lo tanto no era logico quitarlos. No era un problema de muestra.
# La distribucion de la reclasificacion sigue una distribucion normal



# i) table: anchura de la parte superior de diamante con relaci?n al punto m?s ancho  

str(table) # Variable cuantitativa discreta
summary(table) 
# Valores entre 43 y 95, pero realmente la mayoria estan entre 56 y 59
# Valores maximos y minimos muy altos/bajos con respecto a la media
sd(table) # Tiene una dispersion alta
boxplot(table) # Tiene muchisimos valores atipicos fuera del bigote superior e inferior

#¿Cuantos?
maximo<-quantile(table,.75)+1.5*IQR(table)
minimo<-quantile(table,.25)-1.5*IQR(table)
num_val_atipicos<-length(depth[depth>maximo|depth<minimo]) #3746 valores atipicos
prop_val_atipicos<-num_val_atipicos/num_diamonds*100 # Un 6.94% de la muestra
# Es un porcentaje muy alto como para quitarlos y estan la mayoria muy cerca del maximo y del minimo

# Vemos la distribucion de los quantiles con price normalizado
qqnorm(table) # Confirmamos los valores atipicos altos

# Revisamos su distribucion
hist(table)
plot(density(table)) # Es una distribucion normal pero tiene muchos outlayers

# Es el mismo caso que el anterior. Se podria hacer otra reclasificacion. Tambien sabiendo que
# sigue una normal se pueden optar por varias opciones:
# 1. Dejarlo como esta, quitando solamente los valores atipicos mas estremos
# 2. Sustituirlos por la media (NO SOY PARTIDARIO)
# 3. Sustituirlos por max y min respectivamente.

# Nosotros en esta ocasion (para cambiar con respecto a depth) vamos a dejarlos 
# y vamos a quitar solamente los muy extremos que se ven claramente en qqnorm

# Lo eliminamos de la muestra
length(which(table>75|table<50)) # 7 valores atipicos
df<-subset(df,table<75&table>50) # Los eliminamos
num_diamonds<-dim(df)[1] # Y ahora tenemos 53908 diamantes
attach(df)

#Revisamos
summary(table) 
sd(table) # Disminuye la dispersion
boxplot(table)
qqnorm(table)

# Distribucion normal
hist(table)
plot(density(table)) 

# CONCLUSION: Variable cuantitativa discreta, con distribucion normal.
# Se le ha aplicado un tratamiento a los outlayers solo quitando los mas extremos (seguramente problema de dato)
# pero se han dajado el respo porque eran muchos. Se podia haber hecho una reclasificacion.
# De hecho si se tiene encuenta en la regresion habra que hacerla

##############Inferencia###############
# 4. Calcula un intervalo de confianza para la media de "carat" y "depth"

# Si tomamos como muestra todo el df respecto a toda la poblacion de diamantes.
# Podemos inferir por intervalo de confianza la media de toda la poblacion
# Por el TCL sabemos que la media de una muestra cuya poblacion sea normal, sigue tambien
# una distribucion normal, como es el caso de carat y depth.
# Ademas sabemos por el teorema de Chebyshev la probabilidad de la media de la poblacion
# este entre media_muestral+-Z*desv_poblacional/sqrt(n)=1-alfa
# En nuestro caso:
# n>30 y desv_poblacional desconocida--> Utilizaremos desv_muestral y Z(ya que n>30)

# Para carat (Niv.conf:95%-->alfa=0.05)
n<-num_diamonds #=53908
media_muestral<-mean(carat) #=0.797648
desv_muestral<-sd(carat) #=0.4737466
z.teorica<-qnorm(c(0.025),mean = 0, sd = 1, lower.tail = FALSE) #=1.959964

media_poblacional_izq=media_muestral-z.teorica*(desv_muestral/sqrt(n))
media_poblacional_der=media_muestral+z.teorica*(desv_muestral/sqrt(n))

media_poblacional_izq #=0.7936489
media_poblacional_der #=0.8016472
#Conclusion: media_poblacional_carat pertenece a [0.7936489,0.8016472] al 95% confianza

# Para depth (Niv.conf:99%-->alfa=0.01)
n<-num_diamonds #=53908
media_muestral<-mean(depth)  #=61.7493
desv_muestral<-sd(depth)  #=1.431871
z.teorica<-qnorm(c(0.005),mean = 0, sd = 1, lower.tail = FALSE)  #=2.575829

media_poblacional_izq=media_muestral-z.teorica*(desv_muestral/sqrt(n))
media_poblacional_der=media_muestral+z.teorica*(desv_muestral/sqrt(n))

media_poblacional_izq #=61.73342
media_poblacional_der #=61.76519
#Conclusion: media_poblacional_depth pertenece a [61.73342,61.76519] al 99% confianza.
# OBS: Obviamente tenemos mayor confianza en este segundo intervalo.


# 5. Formula un test de hip?tesis

# Se sabe por otros años que la media de precios de diamantes suele sear alrededor de 3800 dollars
# Queremos ver si con esta muestra la media poblacional de este año seria mayor o menor 
# a un nivel de confianza del 95%
# H0: mu<=3800, H1: mu>3800
# Como no conocemos la sd del año pasado utilzamos la sd de este año, 
# y como el n>=30 podemos utilizar la Z (N(0,1)), sino tendriamos que utilizar la t de student

# Calculamos el estadistico Z
z_estimado<-(mean(price)-3800)/(sd(price)/sqrt(num_diamonds)) #7.611842
z_estimado

# Calculamos el valor teorico de la distribucion Z
z.teorica<-qnorm(0.05,0,1,FALSE) #1.644854
# o qnorm(0.95,0,1,TRUE)
z.teorica

# Conclusion: El estadistico es muy alto y cae dentro de la region de rechazo
# => Tengo que rechazar H0 => H1:mu>3800 es cierta con un CI 95%



# Otra posibilidad podria ser:
# Se sabe que la calidad del corte "Ideal" de un diamante es una binomial de p=0.4 aproximadamente
# ¿Con esta muestra podemos inferir en la poblacion al 95% que la calidad del diamante "Ideal"
# sigue esa proporcion?
# H0:p=0.4,H1:p<>0.4. Utilizamos el prop.test de dos colas

prop.test(x=length(cut[cut=="Ideal"]),n=num_diamonds,p=0.4, alternative = "two", conf.level = 0.99)

# Conclusion: Nos da un valor muy alto de p-value=0.8833>0.001 --> Entrariamos en la region de aceptacion
# => Tenemos que aceptar H0 => p=0.4 para el corte "Ideal" dentro de la poblacion




####Relaciones entre las variables####
# Muestra las relaciones que existen entre variables  (al menos dos)
# Tal y como hemos analizado las variables parece claro que algunas estan relacionadas
# como es el caso de price con carat y con x, y, z.
# Revisamos correlacion
head(df)
cor(df[,c("price","carat","x","y","z","depth","table")])
# Se ve claramente la correlacion lineal que existe entre ellos

cor(price,carat)# 0.9215722
cor(price,x)# 0.887206
cor(price,y)# 0.8888018
cor(price,z)# 0.8820979
# Con depth y con table no hay practicamente correlacion
cor(price,depth)# -0.01045122
cor(price,table)# 0.1273655

# Si lo representamos vemos que hay una cierta linealidad:
qplot(carat,price,data=df)
qplot(x,price,data=df)
qplot(y,price,data=df)
qplot(z,price,data=df)

# Podemos incluso intentar mostrar todas
# Aqui se puede ver como a medida que carat y price aumentan tambien lo hace el volumen=x*y*z
qplot(carat,price,data=df,color=x*y*z)

# O relacionarlas con las variables cualitativas:

# Aqui vemos como en funcion del tipo de claridad que tenga el diamante, el peso incrementa
# en mayor o menor medida el precio
qplot(carat,price,data=df,color=clarity)# Se piensa que la claridad tambien influye en el precio

# Aqui podemos ver como en funcion del tipo de corte, no aumenta el precio cuando aumenta
# el peso. Es independiente.
qplot(carat,price,data=diamonds,facets=cut~.)
qplot(carat,price,data=diamonds,color=cut)

# Aqui podemos ver que el color tambien parece variar la relacion del peso con 
# el precio. Aunque no de forma tan evidente como la claridad
qplot(carat,price,data=diamonds,color=color)

# CONCLUSION: El precio, el peso y el volumen(x,y,z) estan altamente correlacionados lineamente
# Aunque tambien caracteristicas cuantitativas lo estan--> Habria que codificarlas y ver su correlacion
# Nosotros de momento nos quedaremos con las anteriores.

# De hecho se puede encontrar una correlacion lineal aun mayor si transformamos:
qplot(log(carat),log(price),data=df)
cor(log(price),log(carat)) #0.9659173 cuando antes rondaba 0.92

qplot(log(price),log(x*y*z),data=df)
cor(log(price),log(x*y*z)) #0.96657



#######An?lisis de regresi?n##########
# a) Formular un modelo de regresi?n y analiza los resultados

# Primero vamos a crear una nueva columna volumen
df$volumen<-x*y*z
#Luego vamos a categorizar las variables cuantitativas que hallamos 
# visto tienen relacion:claridad, color
levels(clarity)
df$clarity_cat<-ifelse(clarity=="I1", 1, 
                         ifelse(clarity=="SI2", 2,
                                ifelse(clarity=="SI1", 3,
                                       ifelse(clarity=="VS2", 4,
                                              ifelse(clarity=="VS1", 5,
                                                     ifelse(clarity=="VVS2", 6,
                                                            ifelse(clarity=="VVS1", 7,
                                                              8)))))))

levels(color)
df$color_cat<-ifelse(color=="D", 1, 
                       ifelse(color=="E", 2,
                              ifelse(color=="F", 3,
                                     ifelse(color=="G", 4,
                                            ifelse(color=="H", 5,
                                                   ifelse(color=="I", 6,
                                                                 7))))))

attach(df)


# OJO: Para formular un modelo hay que revisar las correlaciones parciales lo primero
cor(df[,c("price","carat","volumen","clarity_cat","color_cat")])
#Como ya veiamos tenemos una correlacion altisima del precio con carat y con volumen, 
# vemos que tenemos algo tambien de correlacion con claridad y color.

# 1. Vamos a ver la correlacion parcial que tiene carat con volumen
parcor(cov(df[,c("carat","volumen")])) #0.9989449
# De hecho:
qplot(log(carat),log(volumen),data=df)

# Por lo tanto nos vamos a quedar con carat que es la que mas correlacion tien con price

#2. Vemos si carat tien correlacion con claridad o con color
parcor(cov(df[,c("carat","clarity_cat")])) # -0.3527278 
parcor(cov(df[,c("carat","color_cat")])) # 0.2913072
# Parece que asumible, luego lo veremos segun vayamos incluyendo en el modelo (por stepwise)

parcor(cov(df[,c("clarity_cat","color_cat")]))# Tampoco:0.02579131

rm1 <- lm(price~carat)
summary(rm1)
# Tenemos un p-value bajisimo(<2e-16) tanto para el b0 como para b1 (con una t de student muy alta).
# Entonces tenemos que rechazar H0: b0=0 y b1=0, es decir tenemos mucha seguridad en los coeficientes
# Sin embargo el coeficiente de determinacion es del 85% y eso se debe a que la SC residuaos es
# muy alta
# Residuals:
# Min       1Q   Median       3Q      Max 
# -18583.5   -804.3    -19.0    537.0  12731.6

# Se ve claramente en la regresion
plot(carat,price)
abline(rm1,col="blue",lwd=2)




# b) Muestra los residuos y analiza los resultados

summary(rm1$residuals)
plot(rm1$residuals)
abline(h=0,col="red",lwd=3)

# Vemos claramente que hay un patron, cuando el precio estimado esta entre 2000 y 3000.
# No tenemos una dispersion vertical como la horizontal
# => En este caso es claro aplicar una transformacion logaritmica como hemos estado viendo
# al analizar las variables y al relacionarlas



# c) Aplica una transformaci?n a la regresi?n y analiza los resultados

rm2 <- lm(log(price)~log(carat))
summary(rm2)

# Seguimos teniendo un un p-value bajisimo(<2e-16), es decir seguimos confiando en nuestros coeff
# Sin embargo ahora el coeficiente de determinacion es del 93% porque los residuos estan mucho
# mas ajustados
# Residuals:
# Min       1Q   Median       3Q      Max 
# -1.50859 -0.16950 -0.00594  0.16632  1.3379
sd(rm1$residuals)# Antes 1547.8
sd(rm2$residuals)# Ahora 0.2626104

# Lo podemos ver en la regresion
plot(log(price)~log(carat))
abline(rm2,col="blue",lwd=2)

#Analizando los residuos
summary(rm2$residuals)
plot(log(price),rm2$residuals)
# Tienen una dispersion perfecta alrededor de 0

# En este punto tenemos una regresion lineal simple bastante ajustada.
# log(precio) = 1.675944+log(carat)+8.448780

# Pero nos podemos plantear incluir alguna variable mas como claridad o color
# para ver si se ajusta algo mejor nuestro modelo.
rm3 <- lm(log(price)~log(carat)+clarity_cat)
summary(rm3)
# Parece que si, R ajust=96% y ademas nuestra F teorica es muy alta con su p-value bajo
# F-statistic: 6.903e+05 on 2 and 53905 DF,  p-value: < 2.2e-16
# Con lo cual no existe ninguna combinacion entre ellas que se deba quitar.

sd(rm3$residuals)#0.1966681
#Analizando los residuos
summary(rm3$residuals)
plot(1.8000229*log(carat)+0.1144724*clarity_cat+8.0340296,rm3$residuals)
abline(h=0,col="red",lwd=3)


#Añadimos el color_cat
rm4 <- lm(log(price)~log(carat)+clarity_cat+color_cat)
summary(rm4)
# Perfecto ajustamos un poco mas: R ajust=97.8%, sin dañar el resto:
#  Residual standard error: 0.1503 on 53904 degrees of freedom
#  Multiple R-squared:  0.978,	Adjusted R-squared:  0.978 
#  F-statistic: 8.005e+05 on 3 and 53904 DF,  p-value: < 2.2e-16

sd(rm4$residuals)#0.1503166
#Analizando los residuos
summary(rm4$residuals)
plot(1.8718461*log(carat)+0.1263607*clarity_cat-0.0779723*color_cat+8.2944877,rm3$residuals)
abline(h=0,col="red",lwd=3)
# Y seguimos teniendo una distribucion correcta de los errores.

# Vamos a probar por ultimo con las interacciones de las variables
cor(log(price),log(carat)*clarity_cat)#0.8451771
cor(log(price),log(carat)*color_cat)#0.8667773
cor(log(price),clarity_cat*color_cat)#-0.04731248
cor(log(price),log(carat)**2)#-0.7670203
cor(log(price),clarity_cat**2)#-0.2041066
cor(log(price),color_cat**2)#-0.1557188


# Son bastante bajas, probamos con las dos mas altas:
inter_carat_clarity<-log(carat)*clarity_cat
inter_color_clarity<-log(carat)*color_cat
rm5 <- lm(log(price)~log(carat)+clarity_cat+color_cat+inter_carat_clarity+inter_color_clarity)
summary(rm5)

# NO AÑADEN MAS INFORMACION AL MODELO CON LO CUAL EL MODELO MAS AJUSTADO SERIA:
# log(price)=1.8718461*log(carat)+0.1263607*clarity_cat-0.0779723*color_cat+8.2944877


# Interpreta los coeficientes estandarizados de la regresi?n
# Si tomamos un valor al azar:
val_azar<-df[25340,]
val_azar
precio.estimado=exp(1.8718461*log(val_azar$carat)+0.1263607*val_azar$clarity_cat-0.0779723*val_azar$color_cat+8.2944877)
precio.estimado # 729.1531 frente al real 642

# Para saber realmente que variable esta aportando mas deberiamos tipificar:
media_carat<-mean(log(carat))
media_clarity<-mean(clarity_cat)
media_color<-mean(color_cat)
sd_carat<-sd(log(carat))
sd_clarity<-sd(clarity_cat)
sd_color<-sd(color_cat)

carat_tip<-(log(carat)-media_carat)/sd_carat
clarity_tip<-(clarity_cat-media_clarity)/sd_clarity
color_tip<-(color_cat-media_color)/sd_color

rm_tip <- lm(log(price)~carat_tip+clarity_tip+color_tip)
summary(rm_tip)

#Quedando de la siguiente forma:
# log(price)=1.0944921*log(carat)+0.2081267*clarity-0.1326555*color_tip+0
# Donde B0=0, y la que mas aporta es carat, seguido de claridad y por ultimo color
# Si tuvieramos 2 diamantes de la misma claridad y mismo color y diferentes pesos
# price = exp (1.0944*dif_pesos)
# Idem se puede aplicar al resto

# Por ultimo comentar que los estimados del precio que generan la recata de regresion tienen una incertidumbre
# Estos coeficiones no son exactamente los reales, son los mejores estimadores. Siempre hai una incertidumbre:
# Esa incertidumbre queda definida por los siguientes intevalos de confianza:
confint(rm_tip)

#                  2.5 %     97.5 %
# (Intercept)  7.7850590  7.7875970
# carat_tip    1.0930545  1.0959297
# clarity_tip  0.2067380  0.2095154
# color_tip   -0.1339828 -0.1313281


#############FIN####################
detach(df)
