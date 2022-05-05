#Establecemos el entorno de trabajo
setwd(dir = "E:/Proyectos/15. Proyecto BVL/Rscripts/") #Cambia tu work direction a tu carpeta de trabajo
getwd()

#Cargamos las librerías principales
library(readxl)
library(lpSolve)
library(fPortfolio)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(readxl)
#Importar datos del excel
port=read_excel(path = "E:/Proyectos/15. Proyecto BVL/Base de datos/Precios y Markowitz 15 acciones.xlsx",sheet = "rprecios",col_names = T)
#Convertimos a tipo xts
port=xts(x=port[,-1],order.by = as.Date(port$FECHA))

#Dibujamos los precios
socieda=names(port)
chart.TimeSeries(port,main="Precios de la cartera",ylab="Precio Final")

#Dibujamos los retornos
retport=Return.calculate(port)
retport=retport[-1,]
plot(retport,main="Retornos de la cartera")

#Construimos una tabla para ver los promedios y desvest
promedio=sapply(retport[,colnames(retport)], mean)*100
volatilidad=sapply(retport[,colnames(retport)],sd)*100
tabla1=data.frame(rbind(promedio,volatilidad))

#Contruimos la matriz de varianzas y covarianzas
maCova=cov(retport)*100
maCorr=cor(retport)*100

#Creamos una función y Graficamos la matriz de correlaciones Heat Matrix
require(gplots)
mapa_calor=function(matrizCorrelacion,titulo){
  heatmap.2(x=matrizCorrelacion,
            cellnote = matrizCorrelacion,
            main=titulo,
            symm = T,
            dendrogram = "none",
            Rowv = F,
            trace = "none",
            density.info = "none",
            notecol = "black")
}
corr1=round(maCorr,2)
mapa_calor(corr1,"Mapa de calor: correlaciones")

#Calculemos la frontera eficiente

setRiskFreeRate(espCartera)=0.000 #Rentabilidad Activo libre de riesgo
setNFrontierPoints(espCartera)= 25


frontera<-portfolioFrontier(data=as.timeSeries(retport),spec=portfolioSpec(),constraints = "LongOnly")
frontera

#Dibujemos la frontera eficiente
frontierPlot(frontera)
grid()
tangencyPoints(frontera,pch=19,col="red",cex=2)
tangencyLines(frontera,col="darkgreen",pch=19,cex=2)
minvariancePoints(frontera,col="blue",pch=19,cex=2)
monteCarloPoints(frontera,mcSteps = 5000,col="green",cex=0.5)

#Dibujamos los pesos de los portafolios de la forntera eficiente
weightsPlot(frontera,col=qualiPalette(ncol(retport),"Dark2"))

#Calculamos un portafolio eficiente, es decir un portafolio que se encuentra en la frontera eficiente
efport=efficientPortfolio(as.timeSeries(retport),spec = portfolioSpec(),constraints = "LongOnly")
efport

#Calculamos el portafolio tangente, portafolio con mejor rentabilidad/riesgo
tgport=tangencyPortfolio(as.timeSeries(retport),spec=portfolioSpec(),constraints = "LongOnly")
tgport

#Graficamos el portafolio eficinete (minRisk)
weightsPie(efport,col = qualiPalette(ncol(retport),"Dark2"))
mtext(text = "Portafolio eficiente (minRisk)",side = 3,line = 1.5,font = 2,cex = 0.7,adj = 0)

#Graficamos el portafolio tangente
weightsPie(tgport,col=qualiPalette(ncol(retport),"Dark2"))
mtext(text = "Portafolio tangente",side = 3,line = 1.5,font = 2,cex = 0.7,adj = 0)
