# Comparación entre los estimadores de la media poblacional.

# Para poder ejecutar rpareto es necesario intalar el paquete EnvStats u otro equivalente
# con el comando install.packages('EnvStats') y después marcar su casilla en la pestaña
# de paquetes en la ventana inferior derecha de RStudio.
# Habrá que repetir este último paso cada vez que se quiera hacer uso de éste u otro paquete.

# Puede realizarse la comparación entre estadísticos para este trabajo cargando todas las funciones
# de abajo y ejecutando comparar(40) para el tamaño muestral del trabajo.


muestra<-function(n, k) {                   # Obtenemos n muestras de 10 variables aleatorias Pareto cada una.
  samp= (replicate(n,rpareto(k ,1,3)))      # Nótese que los argumentos de rpareto son (número de variables, beta, alfa).
  return(samp)
}
muestra(40)
media_por_parametros<-function(M) {      # Obtenemos los estadísticos de la media poblacional usando los estimadores de
  res=c()                                # parámetros por máxima verosimilitud, agrupándolos en un vector.
  for (j in 1:ncol(M)) {
    beta=min(M[,j])
    product=prod(M[,j])
    geom=product^(1/nrow(M))
    alpha=1/log(geom/beta)
    mu=alpha*beta/(alpha-1)
    res=c(res,mu)
  }
  return(res)
}

media_armonica<-function(M) {            # Obtenemos la media armónica de cada muestra, agrupándolas en un vector.
  res=c()
  for (j in 1:ncol(M)) {
    sum=0
    for (i in 1:nrow(M)) {
      sum=sum+(1/M[i,j])
    }
    mu=nrow(M)/sum
    res=c(res,mu)
  }
  return(res)
}

media_muestral<-function(M) {            # Obtenemos la media muestral de cada muestra, agrupándolas en un vector
  res=c()
  for (j in 1:ncol(M)) {
    mu=mean(M[,j])
    res=c(res,mu)
  }
  return(res)
}

comparar<-function(n) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviación típica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=3,ncol=2)
  rownames(R)=c("Media muestral","Media por parámetros","Media armónica")
  colnames(R)=c("Media del estimador","Desviación típica del estimador")
  M=muestra(n)
  m=media_muestral(M)
  p=media_por_parametros(M)
  a=media_armonica(M)
  R[1,]=c(mean(m),sd(m))
  R[2,]=c(mean(p),sd(p))
  R[3,]=c(mean(a),sd(a))
  return(R)
}

comparar(40)

TendenciaEsp <-function(n, k) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviación típica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=1,ncol=3)
  rownames(R)=c("Media por parámetros")
  colnames(R)=c("Media del estimador","Varianza del estimador", "Numero de muestras")
  M=muestra(n,k)
  p=media_por_parametros(M)
  R[1,]=c(mean(p),(sd(p)^2), k)
  H <- mean(p)
  return(H)
}
TendenciaEsp(40, 1000)

TendenciaVar <-function(n, k) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviación típica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=1,ncol=3)
  rownames(R)=c("Media por parámetros")
  colnames(R)=c("Media del estimador","Varianza del estimador", "Numero de muestras")
  M=muestra(n,k)
  p=media_por_parametros(M)
  R[1,]=c(mean(p),(sd(p)^2), k)
  H <- sd(p)^2
  return(H)
} 

#Esta funcion sirve para estimar el valor de la Esperanza, el parámetro nos la cantidad de muestras que queremos estudiar
EstimacionEsp <- function(Z){
  Esp <- c()
  for (i in 10:Z){
    Esp <- c(Esp,TendenciaEsp(40, i))
  }
  print(mean(Esp))
  plot(Esp, type="l",ylab="Valor de la Esperanza",xlab="Número de muestras")
  abline(h=1.50)
  
}
EstimacionEsp(10000)
#Igual que la anterior pero para la varianza
EstimacionVar <- function(Z){
  Var <- c()
  for (i in 10:Z){
    Var <- c(Var ,TendenciaVar(40, i))
  }
  print(mean(Var))
  plot(Var, type="l",ylab="Valor de la varianza",xlab="Número de muestras")
  abline(h=0)
  
}
EstimacionVar(50)


muestraInv<-function(n, k) {                  
  samp= (replicate(n, k + rpareto(10,1,3))))      
  return(samp)
}
muestraEsc<-function(n, k) {                  
  samp= (replicate(n, k* rpareto(10,1,3)))      
return(samp)
}

#Estudia la invarianza del estimador, n es el temaño de la muestra, tras el valor de la transformación y esc el valor del cambio de n
Invariancia <-function(n,tras,esc) {                 
  R=matrix(NA,nrow=2,ncol=3)
  rownames(R)=c("Traslación","Escala")
  colnames(R)=c("Media del estimador","Valor teórico","Valor de la transformación")
  M=muestraInv(n,tras)
  K=muestraEsc(n,esc)
  p=media_por_parametros(M)
  j=media_por_parametros(K)
  
  R[1,]=c(mean(p),tras + 1.50, tras)
  R[2,]=c(mean(j),esc * 1.50, esc)
  return(R)
} 
Invariancia(1000,7,6)
 dchisq(12,15)
 
 data = choose.files()