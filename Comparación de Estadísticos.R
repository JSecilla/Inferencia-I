# Comparación entre los estimadores de la media poblacional.

# Para poder ejecutar rpareto es necesario intalar el paquete EnvStats u otro equivalente
# con el comando install.packages('EnvStats') y después marcar su casilla en la pestaña
# de paquetes en la ventana inferior derecha de RStudio.
# Habrá que repetir este último paso cada vez que se quiera hacer uso de éste u otro paquete.

# Puede realizarse la comparación entre estadísticos para este trabajo cargando todas las funciones
# de abajo y ejecutando comparar(40) para el tamaño muestral del trabajo.


muestra<-function(n) {                   # Obtenemos n muestras de 10 variables aleatorias Pareto cada una.
  samp=replicate(n,rpareto(1000,1,3))      # Nótese que los argumentos de rpareto son (número de variables, beta, alfa).
  return(samp)
}

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

Esperanza <-function(n) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviación típica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=2,ncol=2)
  rownames(R)=c("Media muestral","Media por parámetros")
  colnames(R)=c("Media del estimador","Varianza del estimador")
  M=muestra(n)
  m=media_muestral(M)
  p=media_por_parametros(M)
  R[1,]=c(mean(m),(sd(m)^2))
  R[2,]=c(mean(p),(sd(p)^2))
  return(R)
}
Esperanza(40)
