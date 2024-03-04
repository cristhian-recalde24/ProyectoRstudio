![Logo](https://itq.edu.ec/wp-content/uploads/2023/02/Recurso-6.png)

## PRESENTACIÓN  DE PROYECTO 

### CARGO: DOCENTE
### NOMBRE: ING.SEBASTIAN LANDAZURI
### ASIGNATURA: ESTADISTICA DESCRIPTIVA
### CARRERA: DESARROLLO DE SOFTWARE
### NIVEL: TERCER NIVEL
### ESTUDIANTES: MAYURI RIVADENEIRA, CRISTIAN RECALDE, DARLYN VAZQUE

### INTRODUCCIÓN
Analizaremos un conjunto de datos del Banco que contiene información sobre diversos gastos de salud per cápita por país, así como el número de médicos, enfermeras, parteras y personal quirúrgico especializado per cápita. Se utilizarán diferentes herramientas de análisis estadístico para comprender mejor la distribución y variabilidad de estos datos.

### OBJETIVO GENERAL

Analisar y comprender mejor la distribución y variabilidad de los gastos de salud per cápital y la cantidad de profesionales de salud per cápital en diferentes países, con el fin de identificar posibles asociaciones entre estas variables y la calidad y accesibilidad del sistema de salud en cada país.

### OBJETIVO ESPECIFICO

1. Realizamos  una tabla de frecuencias para los gastos de salud per cápital y el personal de salud por país.
2. Creamos un histograma para visualizar la distribución de los gastos de salud per cápital y el personal de salud.
3. Construimos un polígono de frecuencia para analizar la tendencia de los gastos de salud per cápital y el personal de salud.
4. Generamos  un gráfico circular para comparar la proporción de gastos de salud y personal de salud entre países.
5. Calculamos la media, mediana, moda, varianza, desviación estándar y coeficiente de variación para los gastos de salud per cápita y el personal de salud.
6. Calculamos cuartiles, deciles y percentiles para los gastos de salud per cápita y el personal de salud.
7. Creamos un diagrama de cajas para visualizar la distribución y variabilidad de los gastos de salud per cápita y el personal de salud.
8. Realizamos un diagrama de Pareto para identificar los países con mayores gastos de salud per cápita y mayor personal de salud.
Desarrollo.

## MARCO TEORICO
### DESARROLLO 

### Version : 2023.1.2.1+402
### libreria: gtools, qcc, agricolae

### Tabla de Frecuencias

Tabla de frecuencias es una forma de resumir y organizar datos para que sea más fácil analizarlos. En este caso, creamos una tabla de frecuencias para los gastos de salud per cápital y el personal de salud por país. La tabla muestra el número de países en diferentes rangos de gastos de salud per cápita y el número de países con diferentes cantidades de personal de salud.

### Histograma

Histograma es una representación gráfica de la distribución de los datos. En este caso, creamos un histograma para mostrar la distribución de los gastos de salud per cápita y el personal de salud por país. El histograma mostrará la frecuencia de diferentes rangos de gastos de salud per cápita y diferentes cantidades de personal de salud.

### Polígono de Frecuencia

Polígono de frecuencia es una forma de visualizar la distribución de los datos y la tendencia central. En este caso, creamos un polígono de frecuencia para los gastos de salud per cápita y el personal de salud por país. este polígono de frecuencia muestra  la tendencia central de los datos y la distribución de los gastos de salud per cápita y el personal de salud.

### Gráfico Circular

Gráfico circular es una forma de visualizar la proporción de diferentes categorías en un conjunto de datos. En este caso, creamos un gráfico circular para mostrar la proporción de gastos de salud y personal de salud entre países. El gráfico circular mostrará qué países tienen mayores gastos de salud per cápita y mayor personal de salud.

### Media, Mediana, Moda, Varianza, Desviación Estándar y Coeficiente de Variación

La media, mediana, moda, varianza, desviación estándar y coeficiente de variación son medidas de tendencia central y dispersión. En este caso, se puede calcular la media, mediana, moda, varianza, desviación estándar y coeficiente de variación para los gastos de salud per cápita y el personal de salud. Estas medidas proporcionarán información sobre la tendencia central y la variabilidad de los datos.

### Cuartiles, Deciles y Percentiles

Cuartiles, deciles y percentiles son medidas de posición que dividen un conjunto de datos en partes iguales. ya que se  puede calcular los cuartiles, deciles y percentiles para los gastos de salud per cápita y el personal de salud. Estas medidas proporcionarán información sobre la distribución de los datos y la posición de los países en relación con los demás.

### Diagrama de Cajas

Diagrama de cajas es una forma de visualizar la distribución y variabilidad de los datos. En este caso, creamos un  diagrama de cajas para los gastos de salud per cápital y el personal de salud por país. El diagrama de cajas muestra la distribución de los datos, los valores y la variabilidad de los gastos de salud per cápita y el personal de salud.

### Diagrama de Pareto

Diagrama de Pareto es una forma de visualizar los datos en orden descendente de importancia. En este caso, creamos diagrama de Pareto para identificar los países con mayores gastos de salud per cápita y mayor personal de salud. El diagrama de Pareto ya que muestra  los países más importantes en términos de gastos de salud per cápita y personal de salud.

## CÓDIGO

data<-read.csv("2.12_Health_systems.csv")
print(data)
n<-data$Nurse_midwife_per_1000_2009.18
print(n)
print(length(n))

### TABLA DE FRECUENCIA 
TablaFrecNurse<-function(a){
  list<-hist(a, plot=FALSE)
  Health<-table.freq(list)
  View(n)
  print("La tabla de frecuencia es una herramienta estadística que organiza y resume la información 
        sobre la distribución de datos.")
}
TablaFrecNurse(n)

### HISTOGRAMA
HistogramaNurse<-function(a){
  hg<-hist(a)
  plot(hg, main = "Histograma Enfermeras Parteras",xlab="eje x", ylab="eje y", col="blue")
  print("El Histograma ofrecen una buena forma de evaluar los datos. Se pueden usar para comprobar valores extremos o atípicos y ayudar a comprender la distribución de sus datos.")  
}
HistogramaNurse(n)

### POLIGONO DE FRECUENCIA 

PoligonoFreNurse<-function(a){
  hg<-hist(a)
  polygon.freq(hg,main="Poligono de Frecuencia Health", lwd=3,col="red")
  print("El poligono de frencuencia se utilizan cuando se busca comparar o cruzar los datos de una variable en la misma gráfica.")
}
PoligonoFreNurse(n)

### Diagrama Circular
DiagramaCircularNurse<-function(a){
  rang<-a[1:5]
  colors<-c("yellow","red","blue","green","pink")
  pie(rang,main = "Diagrama Circular Enfermeras parteras",col = colors)
  print("Un diagrama circular de un rango de 1 a 5 de la varible seleccionada")
}
DiagramaCircularNurse(n)

### Media
mediaNurse<-function(vector){
  n<-na.omit(n)
  mean(n)
  paste("La media es :" ,mean(n))  
}
mediaNurse(n)

### Mediana
medianaNurse<-function(vector){
  n<-na.omit(n)
  return(median(vector))
}
paste("La mediana es:",medianaNurse(n))

### Moda
modaNurse<-function(vector){
  n <- na.omit(n)
  return(as.numeric(
    names(which.max(table(n)))))   
} 
paste("La moda es:", modaNurse(n))

### Varianza
varianzaNurse<- function(vector){
  n <- na.omit(n)
  return(var(vector,na.rm=FALSE))
}
paste("La varianza es:",varianzaNurse(n))

### Desviacion
desviacionNurse<-function(x){
  return(sd(n))
}
paste("La desviacion es:", desviacionNurse(n))

### CUARTILES

cuartilesNurse <- function(vector){
  q <- quantile(vector,probs = c(0.25, 0.50, 0.75))
  return(q)
}
cuartilesNurse(n)
### DECILES
decilesNurse <- function(vector){
  d <- quantile(vector, probs = seq(0.1, 0.9, by=0.1))
  return(d)
}
decilesNurse(n)

### PERCENTILES
percentilesNurse <- function(vector){
  p <- quantile(vector, probs = seq(0.01, 0.99, by=0.01))
  return(p)
}
percentilesNurse(n)

### DIAGRAMA DE CAJA


diagramaCajaNurse <- function(vector){
  boxplot(vector, col = "purple",horizontal = TRUE)
}
diagramaCajaNurse(n)
print("Muestra la distribución de los datos, incluyendo la mediana, el rango  y los valores.
Esto puede ayudar a identificar la dispersión y la presencia de valores extremos en los datos.")


### DIAGRAMA DE PARETO

pareto.chart(
  n,
  ylab = "Frecuencias", 
  col = heat.colors(length(vector)),
  cumperc = seq(0, 80, by = 10),
  ylab2 = "porcentaje acumulado",
  main = "Comentarios"
)

# Mensaje para imprimir junto con el gráfico
print("El gráfico de Pareto muestra la distribución de datos ordenados de mayor a menor, con la línea azul que representa el porcentaje acumulado. Esto puede ayudar a identificar las áreas de mayor impacto o los elementos más importantes."
