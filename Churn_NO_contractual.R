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


### Crear una columna "Total" para mostrar cu�nto gast� cada cliente en cada compra.
### Luego crear un nuevo marco de datos para el gasto total de cada cliente por d�a.
### Especificarle a InvoiceDate que debe tener formato Fecha, para no tener problemas en la resta de fechas

data$Total <- data$Quantity * data$UnitPrice
txns <- data %>% 
  mutate(CustomerID = as.factor(CustomerID),
         InvoiceDate = as.Date(InvoiceDate)) %>%
  group_by(CustomerID, InvoiceNo, InvoiceDate) %>% 
  summarise(Spend = sum(Total)) %>%
  ungroup() %>% 
  filter(Spend>0)

### A continuaci�n, Calcular el tiempo entre compras para cada cliente.

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

### Crear una peque�a funci�n para muestrear aleatoriamente a los clientes.

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

### Despu�s de una serie de disputas de datos, ahora podemos visualizar la distribuci�n entre d�as de 
### compra para 20 clientes seleccionados al azar.

ecdf_df <- time_between %>% group_by(CustomerID) %>% arrange(dt) %>% mutate(e_cdf = 1:length(dt)/length(dt))
sample_users <- ecdf_df %>% inner_join(Ntrans) %>% sample_n_groups(20)
ggplot(data = time_between %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 15) + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)',y = 'Frequency')

### Interpretaci�n:
###  La mayor�a de los CustomerID 12748 entre los d�as de compra fueron menos de 5 d�as, ocasionalmente, 
### su (entre ellos) d�as entre compras excedi� 5 o incluso 10 d�as.

### CustomerID 13102 es un cliente poco frecuente y la mayor parte de su (o ella) entre d�as de compra 
### var�a de 5 a 30 d�as.

### Despu�s de calcular el ECDF para cada cliente, estamos visualizando el ECDF de los clientes anteriores. 
### La l�nea roja representa aproximadamente el percentil 90. Entonces, si el ECDF cruza la l�nea roja a los 
### 20 d�as, esto significa 9 de cada 10 veces que el cliente realizar� otra compra dentro de los 20 d�as.

ggplot(data = ecdf_df %>% inner_join(Ntrans) %>% filter(CustomerID %in% sample_users$CustomerID), aes(dt,e_cdf) ) + 
  geom_point(size =0.5) +
  geom_line() + 
  geom_hline(yintercept = 0.9, color = 'red') + 
  facet_wrap(~CustomerID) +
  labs(x = 'Time Since Last Purchase (Days)')

### Cree una funci�n para calcular el percentil 90.

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

### El modelo nos dice: 9 de cada 10 veces, CustomerID 12748 realizar� otra compra dentro de 4.74 d�as, 
### si CustomerID 12748 no realiza otra compra dentro de 4.74 d�as, sabemos que solo hay una probabilidad 
### de 1 en 10 de que esto suceda, y que Este comportamiento es an�malo. En este punto, sabemos que 
### CustomerID 12748 comienza a actuar de forma "an�mala".

### Veamos r�pidamente el historial de compras del CustomerID 12748 para ver si nuestro modelo tiene sentido:

txns[ which(txns$CustomerID==12748), ]

### La mayor�a de las compras de CustomerID 12748 ocurrieron en 1 a 4 d�as. Tiene sentido que debamos 
### preocuparnos si �l (o ella) no realiza otra compra en 4.74 d�as.

### Mirando CustomerID 13102:

percentiles[ which(percentiles$CustomerID==13102), ]

### El modelo nos dice: 9 de cada 10 veces, CustomerID 13102 realizar� otra compra dentro de los 31.6 d�as, 
### si CustomerID 13102 no realiza otra compra dentro de los 31.6 d�as, sabemos que solo hay una probabilidad 
### de 1 en 10 de que esto suceda, y que Este comportamiento es an�malo. En este punto, sabemos que 
### CustomerID 13102 comienza a actuar de forma "an�mala".

### Nuevamente, tenemos una instant�nea r�pida del historial de compras de CustomerID 13102 para ver si 
### nuestro modelo tiene sentido:

txns[ which(txns$CustomerID==13102), ]

### Al mirar el historial de compras de CustomerID 13102, �estamos de acuerdo con el modelo!

### �Aqui lo tienes! Ahora sabemos el punto en el que cada cliente comenzar� a actuar de forma "an�mala".

### --- ###

### La rotaci�n es muy diferente para las empresas no contractuales. El desaf�o radica en definir un 
### evento de abandono claro que significa adoptar un enfoque diferente para modelar el abandono. 
### Cuando un cliente est� agitado, su tiempo (entre compras) es anormalmente grande, por lo que 
### debemos tener una idea de lo que significa "an�malamente" para cada cliente. Usando el ECDF, 
### hemos estimado el percentil 90 de cada cliente entre la distribuci�n del tiempo de compra de una 
### manera no param�trica. Al examinar la �ltima vez que un cliente ha realizado una compra, y si el 
### tiempo entre entonces y ahora est� cerca del percentil 90, podemos llamarlos "en riesgo de abandono" 
### y tomar las medidas adecuadas para evitar que se agiten. Lo mejor de todo, con m�s datos, nuestro 
### enfoque ser� cada vez mejor desdeel ECDF converger�a en la funci�n de distribuci�n acumulativa 
### subyacente (CDF) de la poblaci�n .

### Adem�s, cuando implementamos el modelo anterior, es posible que deseemos tener en cuenta la estacionalidad.











