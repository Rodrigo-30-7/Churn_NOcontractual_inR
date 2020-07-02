### Cargar las bibliotecas y lea los datos

library(data.table)
library(tidyverse)
library(lubridate)
library(XLConnect)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())
setwd("C:/Users/FelipeHernandez/Desktop")

raw.data = fread("Online_Retail.csv", sep=",", header = T)
data <- raw.data

### Borrar los datos implicitos en la columna CostumerID

sapply(data, function(x) sum(is.na(x)))
dim(data)
data <- subset(data, CustomerID != "")
dim(data)


### Crear una columna "Total" para mostrar cuánto gastó cada cliente en cada compra.
### Luego crear un nuevo marco de datos para el gasto total de cada cliente por día.
### Especificarle a InvoiceDate que debe tener formato Fecha, para no tener problemas en la resta de fechas

data$Total <- data$Quantity * data$UnitPrice
txns <- data %>% 
  mutate(CustomerID = as.factor(CustomerID),
         InvoiceDate = as.Date(InvoiceDate)) %>%
  group_by(CustomerID, InvoiceNo, InvoiceDate) %>% 
  summarise(Spend = sum(Total)) %>%
  ungroup() %>% 
  filter(Spend>0)

### A continuación, Calcular el tiempo entre compras para cada cliente.

time_between <- txns %>% 
  arrange(CustomerID, InvoiceDate) %>% 
  group_by(CustomerID) %>% 
  mutate(dt = as.numeric(InvoiceDate - lag(InvoiceDate), unit=  'days')) %>% 
  ungroup() %>% 
  na.omit()

View(data)

### En este momento, solo estamos interesados en clientes que hayan realizado al menos 20 compras en los datos.

Ntrans = txns %>% 
  group_by(CustomerID) %>% 
  summarise(N = n()) %>%
  filter(N>20)

### Crear una pequeña función para muestrear aleatoriamente a los clientes.

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

### Después de una serie de disputas de datos, ahora podemos visualizar la distribución entre días de 
### compra para 20 clientes seleccionados al azar.

ecdf_df <- time_between %>% group_by(CustomerID) %>% arrange(dt) %>% mutate(e_cdf = 1:length(dt)/length(dt))
sample_users <- ecdf_df %>% inner_join(Ntrans) %>% sample_n_groups(20)
ggplot(data = time_between %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 15) + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)',y = 'Frequency')

### Interpretación:
###  La mayoría de los CustomerID 12748 entre los días de compra fueron menos de 5 días, ocasionalmente, 
### su (entre ellos) días entre compras excedió 5 o incluso 10 días.

### CustomerID 13102 es un cliente poco frecuente y la mayor parte de su (o ella) entre días de compra 
### varía de 5 a 30 días.

### Después de calcular el ECDF para cada cliente, estamos visualizando el ECDF de los clientes anteriores. 
### La línea roja representa aproximadamente el percentil 90. Entonces, si el ECDF cruza la línea roja a los 
### 20 días, esto significa 9 de cada 10 veces que el cliente realizará otra compra dentro de los 20 días.

ggplot(data = ecdf_df %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt,e_cdf) ) + 
  geom_point(size =0.5) +
  geom_line() + 
  geom_hline(yintercept = 0.9, color = 'red') + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)')

### Cree una función para calcular el percentil 90.

getq <- function(x,a = 0.9){
  if(a>1|a<0){
    print('Check your quantile')
  }
  X <- sort(x)
  e_cdf <- 1:length(X) / length(X)
  aprx = approx(e_cdf, X, xout = c(0.9))
  return(aprx$y)
}
percentiles = time_between %>% 
  inner_join(Ntrans) %>% 
  filter(N>5) %>% 
  group_by(CustomerID) %>% 
  summarise(percentile.90= getq(dt)) %>% 
  arrange(percentile.90)

### Mirando el CustomerID 12748: 

percentiles[ which(percentiles$CustomerID==12748), ]

### El modelo nos dice: 9 de cada 10 veces, CustomerID 12748 realizará otra compra dentro de 4.74 días, 
### si CustomerID 12748 no realiza otra compra dentro de 4.74 días, sabemos que solo hay una probabilidad 
### de 1 en 10 de que esto suceda, y que Este comportamiento es anómalo. En este punto, sabemos que 
### CustomerID 12748 comienza a actuar de forma "anómala".

### Veamos rápidamente el historial de compras del CustomerID 12748 para ver si nuestro modelo tiene sentido:

txns[ which(txns$CustomerID==12748), ]

### La mayoría de las compras de CustomerID 12748 ocurrieron en 1 a 4 días. Tiene sentido que debamos 
### preocuparnos si él (o ella) no realiza otra compra en 4.74 días.

### Mirando CustomerID 13102:

percentiles[ which(percentiles$CustomerID==13102), ]

### El modelo nos dice: 9 de cada 10 veces, CustomerID 13102 realizará otra compra dentro de los 31.6 días, 
### si CustomerID 13102 no realiza otra compra dentro de los 31.6 días, sabemos que solo hay una probabilidad 
### de 1 en 10 de que esto suceda, y que Este comportamiento es anómalo. En este punto, sabemos que 
### CustomerID 13102 comienza a actuar de forma "anómala".

### Nuevamente, tenemos una instantánea rápida del historial de compras de CustomerID 13102 para ver si 
### nuestro modelo tiene sentido:

txns[ which(txns$CustomerID==13102), ]

### Al mirar el historial de compras de CustomerID 13102, ¡estamos de acuerdo con el modelo!

### ¡Aqui lo tienes! Ahora sabemos el punto en el que cada cliente comenzará a actuar de forma "anómala".

### --- ###

### La rotación es muy diferente para las empresas no contractuales. El desafío radica en definir un 
### evento de abandono claro que significa adoptar un enfoque diferente para modelar el abandono. 
### Cuando un cliente está agitado, su tiempo (entre compras) es anormalmente grande, por lo que 
### debemos tener una idea de lo que significa "anómalamente" para cada cliente. Usando el ECDF, 
### hemos estimado el percentil 90 de cada cliente entre la distribución del tiempo de compra de una 
### manera no paramétrica. Al examinar la última vez que un cliente ha realizado una compra, y si el 
### tiempo entre entonces y ahora está cerca del percentil 90, podemos llamarlos "en riesgo de abandono" 
### y tomar las medidas adecuadas para evitar que se agiten. Lo mejor de todo, con más datos, nuestro 
### enfoque será cada vez mejor desdeel ECDF convergería en la función de distribución acumulativa 
### subyacente (CDF) de la población .

### Además, cuando implementamos el modelo anterior, es posible que deseemos tener en cuenta la estacionalidad.











