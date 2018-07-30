#Sys.setenv("HADOOP_PREFIX"="/opt/hadoop")
#Sys.setenv("HADOOP_CMD"="/opt/hadoop/bin/hadoop")
#Sys.setenv("HADOOP_STREAMING"="/opt/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.4.0.jar")
library(twitteR)
library(RCurl)
#library(rhdfs)
#library(rmr2) 

#hdfs.init()
#rmr.options(backend="local")
#NULL

Consumer_Key <- "eee"
Consumer_Secret <- "www"
Access_Token <- "qqqq"
Access_Token_Secret <- "aaaa"

setup_twitter_oauth(Consumer_Key, Consumer_Secret, Access_Token, Access_Token_Secret)
Yes
#Actividad 1
user <- getUser("IsraelRodirguez")
userFriends <- user$getFriends()
userFriends<-twListToDF(userFriends)
userFriends['type']<-1
userFollowers <- user$getFollowers()
userFollowers<-twListToDF(userFollowers)
userFollowers['type']<-2
userNeighbors <- rbind(userFriends, userFollowers)
userNeighbors$type[270]

#Actividad 2
userNeighbors <- subset(userNeighbors, select=c("statusesCount","followersCount","favoritesCount","friendsCount","type"))

#Actividad 3
write.csv(userNeighbors, file = "uuu_twiter_usuario.csv")
          
#Actividad 4

kmeans.mr = function( P,num.clusters,num.iter,combine,in.memory.combine ) {
  
  dist.fun =  function(C,P) { apply( C,1,function(x) colSums((t(P) - x)^2)) }
  
  kmeans.map = function(., P) {
    nearest = {
      if(is.null(C)) 
        sample( 1:num.clusters, nrow(P), replace = TRUE)
      else {
        D = dist.fun(C, P)
        nearest = max.col(-D)
        print(nearest)
      }
    }
    if(!(combine || in.memory.combine))
      keyval(nearest, P) 
    else 
      keyval(nearest, cbind(1, P))
  }
  
  kmeans.reduce = {
    if (!(combine || in.memory.combine) ) 
      function(., P) t(as.matrix(apply(P, 2, mean)))
    else 
      function(k, P) keyval( k, t(as.matrix(apply(P, 2, sum))))
  }
  C = NULL
  for(i in 1:num.iter ) {
    calc <- mapreduce( P, 
                       map = kmeans.map,
                       reduce = kmeans.reduce,
                       verbose = FALSE )
    C = values( from.dfs( calc ) )
    if(combine || in.memory.combine)
      C = C[, -1]/C[, 1]
    
    if(nrow(C) < num.clusters) {
      C = rbind( C, 
                 matrix( rnorm( (num.clusters - nrow(C)) * nrow(C)), ncol = nrow(C)) %*% C) 
    }
  }   
  C
}


#Actividad 5
Datos = read.table("uuu_twiter_usuario.csv", header = TRUE, sep= ",",row.names =1)
plot(Datos, pch = 19)
InerciaIC = rep(0, 30)
for (k in 1:30) {
  grupos = kmeans(Datos, k)
  InerciaIC[k] = grupos$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b")
#Observado el gráfico, consideramos que 2 y 3 son K válidos

grupos <- kmeans(Datos, 3, iter.max = 100)
plot(Datos[,1],Datos[,3] ,pch = 19)
points(grupos$centers, pch = 10, col = "blue", cex = 2)
points(Datos[,1],Datos[,3], col = grupos$cluster + 1, pch = 19)

plot(Datos[,3],Datos[,4] ,pch = 19)
points(grupos$centers, pch = 10, col = "blue", cex = 2)
points(Datos[,3],Datos[,4], col = grupos$cluster + 1, pch = 19)

plot(Datos[,2],Datos[,4] ,pch = 19)
points(grupos$centers, pch = 10, col = "blue", cex = 2)
points(Datos[,2],Datos[,4], col = grupos$cluster + 1, pch = 19)
#Actividad 6
ignore <- rmr.options(backend = "local")
set.seed(0)
grupos2 <- kmeans.mr( to.dfs(Datos), 
                  num.clusters = 3, 
                  num.iter = 15,
                  combine = FALSE,
                  in.memory.combine = FALSE)

Datos['cluster']<-grupos2$cluster
write.csv(Datos, file="uuu1_twiter_usuario_grupos.csv")


