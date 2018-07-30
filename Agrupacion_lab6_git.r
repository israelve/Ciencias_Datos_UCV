setwd ("/root/Escritorio/Laboratorio_06/")

Datos = read.table("EjemploEstudiantes.csv", header = TRUE, sep= ";",dec=",",row.names =1)
Datos

modelo =hclust(dist(Datos), method ="complete")

plot(modelo)
rect.hclust(modelo,k=3,border="red")

Grupo <- cutree (modelo,k=3)
NDatos <- cbind(Datos, Grupo)
NDatos

library(rattle) #para usar center.hclust
centros <- centers.hclust(Datos, modelo, nclust = 3 ,use.median= FALSE)
centros

rownames(centros) <- c("Cluster 1","Cluster 2", "Cluster 3")
barplot(centros [1,], col = c(2,3,4,5,6,7),las = 2)
barplot(centros [2,], col = c(2,3,4,5,6,7),las = 2)
barplot(centros [3,], col = c(2,3,4,5,6,7),las = 2)

barplot (t(centros),beside = TRUE, col = c(2,3,4,5,6))

Datos2 = read.table("kmedias-2.csv", header =  TRUE, sep = ";", dec = ".", row.names = 1)

plot(Datos2,pch= 19)

grupos <- kmeans(Datos2,2,iter.max = 100) 
#hay que correr la  linea de arriba cada vez que quiera cambiar el numero de grupos
#itermax es por defecto 10
grupos
grupos$cluster


plot(Datos2, pch=19)
points(grupos$centers, pch=19, col="blue", cex=2)
points(Datos2, col= grupos$cluster +1 ,pch=19)


rownames(grupos$centers) <- c("cluster1","cluster 2")
barplot(t(grupos$centers),beside = TRUE, col = heat.colors(5))



grupos <- kmeans(Datos2, 4 ,iter.max = 50) 
plot(Datos2, pch=19, xlab = expression(x[1]), ylab= expression(x[2]))
points(grupos$centers, pch=19, col="blue", cex=2)
points(Datos2, col= grupos$cluster +1 ,pch=19)



InerciaIC = rep (0,30)
for (k in 1:30) {
  grupo = kmeans(Datos2,k)
  InerciaIC[k] = grupo$tot.withinss  
}
#esto es un codo de yianbu
plot(InerciaIC, col= "blue", type = "b")


Datos = read.table("kmedias-3.csv", header = TRUE, sep= ";", dec=".", row.names =1)
plot(Datos,pch= 19)
InerciaIC = rep (0,30)
for (k in 1:30) {
  grupo = kmeans(Datos,k)
  InerciaIC[k] = grupo$tot.withinss  
}
#esto es un codo de yianbu
plot(InerciaIC, col= "blue", type = "b")

#hue hue hue



Datos3 <- read.table("Servicio_al_cliente.csv", 
                     header= TRUE, 
                     sep= ";",
                     dec=",",
                     row.names= 1,
                     encoding= "UTF-8")

grupos<-kmeans(Datos3, 5,iter.max=100, nstart=100)#el metodo se ejecuta n veces cada iteracion
grupos
res1<-list()
res25<-list()
inercias1<-rep(0,10)
inercias25<-rep(0,10)
for(i in 1:10){
  res1[[i]]<- kmeans(Datos3, 4, nstart= 1)
  res25[[i]]<- kmeans(Datos3, 4, nstart= 25)
  inercias1[i]<- res1[[i]]$tot.withinss
  inercias25[i]<- res25[[i]]$tot.withinss
}
inercias1
inercias25

system.time(res<-kmeans(Datos3,4,nstart=1))#Para saber cuanto se tarda
system.time(res<-kmeans(Datos3,4,nstart=250))#Para saber cuanto se tarda
#?mclapply
#?parapply
library(parallel)
mc<-detectCores()
mc
cl<- makeCluster(mc, type="SOCK")# si el 1er parametro es = a mc solo hace una distribucion a cada core de cada proceso si no, es virtual
ignore <- clusterEvalQ(cl{
  setwd("/media/3562-6165/Datos/")
  #Buscar codigo de la presentación debe haber dif entre parapply y lapply con system.time()
  Datos = read.tableread.table("Servicio_al_cliente.csv", 
                               header= TRUE, 
                               sep= ";",
                               dec=",",
                               row.names= 1,
                               encoding= "UTF-8")
  grupos <- function(n){kmeans(Datos, 4, nstart = n)}
})

resul<- lapply(cl, c(2,2,2,2),grupos)#probar con system.time$elapsed para benchmark de rendimiento
inercia_intra<- function(r){
  r$tot.withinss
}
cl.close


grupos<-function(n){kmeans(Datos3, 4, nstart=n)}
resul<- lapply(c(2,2,2,2))#probar con system.time$elapsed para benchmark de rendimiento
inercia_intra<- function(r){
  r$tot.withinss
}

