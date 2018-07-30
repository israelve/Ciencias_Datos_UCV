#Integrantes:
#Alberto.Cavadia 
#Juan. Gómez 
#José Israel. Rodríguez

##############################

#Aqui estan los circulos de relaciones y los arboles

###################################

#setwd ("ciencias de datos/tarea 3/tarea_03")
library(FactoMineR)

Datos = read.table("Servicio_al_cliente.csv", header = TRUE, sep= ";",dec=",",row.names =1)
#1 Analisis exploratorio
Datos

#2.1 Grafique el mapa el círculo de correlaciones y realize un análisis de las relaciones entre las variables involucradas
#install.packages("FactoMineR")
#2.2 Grafique el plano principal e identifique la cantidad de posibles grupos.


modelo <- PCA(Datos,
              scale.unit = TRUE,
              ncp = 5,
              graph = FALSE)
modelo
par(mfrow = c(1,2))
plot(modelo, axes = c(1,2), choix = "ind", col.ind = "red", new.plot = TRUE)
plot(modelo, axes = c(1,2), choix = "var", col.var = "blue", new.plot = TRUE)
modelo$ind

par(mfrow = c(1,2))
plot(modelo, axes = c(1,2), choix = "ind", col.ind = "red", new.plot = TRUE, select = "cos2 0.9")
plot(modelo, axes = c(1,2), choix = "var", col.var = "blue", new.plot = TRUE,select = "cos2 0.9")




#2.3 Por cada grupo determine: Cantidad de clientes que los conforman y que porcentaje representa del total. 

#transform(as.data.frame(table(Salida[13])),percentage_column=Freq/nrow(Salida[13])*100)
#con esta consulta, mas adelante podremos determinar esto

#Características más resaltantes del grupo

#Hay 4 posibles grupos formados por
# a-espacios parqueo, calidad instalaciones
# b-variedad productos, prestigio empresa, limpieza, calidad de servicio, distribucion productos, calidad de servicio, velocidad de caja
# c-ubicacion
# d-Edad, antiguedad, atencion empliados



#2.4Determine el grado de representación de los individuos y de las variables y razone sobre si es necesario estudiar otras conbinaciones de componentes (por ejemplo 1 y 3 ó 2 y 3) 
modelo$ind$cos2
modelo$var$cos2

#No se debiera utilizar otras componentes,puesto que PCA ordena los resultados por capacidad de una dimension para representar los elementos, por lo tanto, la dimension 1 y despues la 2 son las 2 mejores dimensiones apra representar al promedio. 

#3. Sobre las dos primeras componentes realice un método de clustering para determinar los grupos
#identificados antes

res.hcpc <- HCPC(modelo, nb.clust = -1, consol = TRUE, min = 4, max = 4, graph = FALSE)
res.hcpc
res.hcpc$data.clust



#4.Agregue el grupo correspondiente a cada cliente como una nueva columna en los datos de entrada y
#almacene este nuevo data-frame en un archivo llamado Servicio_al_cliente_sal.csv.

Salida <- cbind(Datos, res.hcpc$data.clust[13])
write.csv(file="Servicio_al_cliente_sal.csv", x=Salida)


# Respuesta 2.3 
transform(as.data.frame(table(Salida[13])),percentage_column=Freq/nrow(Salida[13])*100)

#5.Realice una gráfica de los grupos generados según el método que haya sido empleado para su
#generación.

plot(res.hcpc, axes=c(1,2), choice="tree", rect=TRUE,
     draw.tree=TRUE, ind.names=TRUE, t.level="all", title=NULL,
     new.plot=FALSE, max.plot=15, tree.barplot=TRUE,
     centers.plot=FALSE)


