# Comparaci�n entre los estimadores de la media poblacional.

# Para poder ejecutar rpareto es necesario intalar el paquete EnvStats u otro equivalente
# con el comando install.packages('EnvStats') y despu�s marcar su casilla en la pesta�a
# de paquetes en la ventana inferior derecha de RStudio.
# Habr� que repetir este �ltimo paso cada vez que se quiera hacer uso de �ste u otro paquete.

# Puede realizarse la comparaci�n entre estad�sticos para este trabajo cargando todas las funciones
# de abajo y ejecutando comparar(40) para el tama�o muestral del trabajo.


muestra<-function(n) {                   # Obtenemos n muestras de 10 variables aleatorias Pareto cada una.
  samp=replicate(n,rpareto(1000,1,3))      # N�tese que los argumentos de rpareto son (n�mero de variables, beta, alfa).
  return(samp)
}

media_por_parametros<-function(M) {      # Obtenemos los estad�sticos de la media poblacional usando los estimadores de
  res=c()                                # par�metros por m�xima verosimilitud, agrup�ndolos en un vector.
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

media_armonica<-function(M) {            # Obtenemos la media arm�nica de cada muestra, agrup�ndolas en un vector.
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

media_muestral<-function(M) {            # Obtenemos la media muestral de cada muestra, agrup�ndolas en un vector
  res=c()
  for (j in 1:ncol(M)) {
    mu=mean(M[,j])
    res=c(res,mu)
  }
  return(res)
}

comparar<-function(n) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviaci�n t�pica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=3,ncol=2)
  rownames(R)=c("Media muestral","Media por par�metros","Media arm�nica")
  colnames(R)=c("Media del estimador","Desviaci�n t�pica del estimador")
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

Esperanza <-function(n) {                  # Generamos n muestras y las pasamos por cada estimador, calculamos la media y desviaci�n t�pica del estimador y la resumimos en una matriz.
  R=matrix(NA,nrow=2,ncol=2)
  rownames(R)=c("Media muestral","Media por par�metros")
  colnames(R)=c("Media del estimador","Varianza del estimador")
  M=muestra(n)
  m=media_muestral(M)
  p=media_por_parametros(M)
  R[1,]=c(mean(m),(sd(m)^2))
  R[2,]=c(mean(p),(sd(p)^2))
  return(R)
}
Esperanza(40)
