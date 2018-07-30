#data: https://www.kaggle.com/c/digit-recognizer

#Integrantes:
#Alberto.Cavadia 
#Juan. Gómez 
#José Israel. Rodríguez

#setwd("C:/Users/Owner/Desktop/tarea 3/CD Tarea2")

var <- read.csv("cd_digits_train_01.csv", header=TRUE)   #lee la data original

print_number<-function(i){

  cat("\014")   #limpia la consola de escritos
  par(mfcol=c(1,1))

  #var[,1] <- as.factor(var[,1])
  if(i>0 && i<=nrow(var)){ 
  var <- var[, -1]  # le quitamos la primera columna, que corresponde a label 
  
  m <- matrix(unlist(var[i,]), nrow=28, ncol=28, byrow = TRUE)  #le da formato de matrix a la lista
  #output <- matrix(unlist(munlis), ncol = 28, byrow = TRUE)\
  rotate <- function(x) t(apply(x, 2, rev))  #funcion para rotar la imagen 90 grados
  m <- rotate(m)   #rotamos la imagen
  image(m,col  = gray((32:0)/32))  #graficamos en escala de grises 
  }else{
    print("numero fuera del rango permitido")
  }
  
}



print_number_2<-function(i){
  cat("\014")   #limpia la consola de escritos

  
  par(mfcol=c(2,3))
  if(i>=0 && i<=9){
    var<-  head(var[var[,1]==i,],6) #obtenemos los primeros 6 registros con el valor dado
    var <- var[, -1]  # le quitamos la primera columna, que corresponde a label 
    for(i in 1:nrow(var)){
      m <- matrix(unlist(var[i,]), nrow=28, ncol=28, byrow = TRUE)  #le da formato de matrix a la lista
      #output <- matrix(unlist(munlis), ncol = 28, byrow = TRUE)\
      rotate <- function(x) t(apply(x, 2, rev))  #funcion para rotar la imagen 90 grados
      m <- rotate(m)   #rotamos la imagen
      image(m,col  = gray((32:0)/32))  #graficamos en escala de grises 
    }
  }else{
    print("numero fuera del rango permitido")
  }
}

