# INTERVALOS DE CONFIANZA - DIABETES

# Los datos vienen de https://www.kaggle.com/alexteboul/diabetes-health-indicators-dataset?select=diabetes_binary_health_indicators_BRFSS2015.csv

# Se deber� descargar el archivo .csv e introducir a continuaci�n la ruta del archivo.

# Todas las funciones de este c�digo no requieren de argumentos, basta con ejecutar intervalos_proporciones() tal cual.

# Las funciones est�n ordenadas seg�n se ofrecen los distintos intervalos en el documento pdf.



ruta="C:/Users/jsm01/OneDrive/Escritorio/Segundo/1� Cuatrimestre/inferencia I/datos diabetes.csv"      # Introducir ruta del archivo entre comillas.

"C:/Users/jsm01/OneDrive/Escritorio/Segundo/1� Cuatrimestre/inferencia I/datos diabetes.csv"
Datos= read.csv(ruta)        # Una vez introducida la ruta, cargar TODAS las sentencias y funciones de abajo.

var_categoricas=c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,18,19,21,22)

var_categoricas2=c(2,3,4,6,7,8,9,10,11,12,13,14,18,19)

var_discretas=c(5,16,17)

nombre_var_categoricas=c("Diabetes","Presi�n sangu�nea alta","Colesterol alto","Prueba de colesterol �ltimos 5 a�os",
                         "Fumador","Derrame cerebral","Problemas card�acos graves","Actividad f�sica","Consumo diario de fruta",
                         "Consumo diario de verdura","Consumo habitual de alcohol","Seguro m�dico/cobertura sanitaria","No pudo costearse m�dico",
                         "Autopercepci�n de salud pobre","Dificultades subiendo escaleras","Sexo (0 = Mujer, 1 = Hombre)","Nivel educativo bajo","Nivel de renta bajo")

nombre_var_categoricas2=c("Presi�n sangu�nea alta","Presi�n sangu�nea baja","Colesterol alto","Colesterol bajo",
                          "Prueba de colesterol �ltimos 5 a�os","Sin Prueba de colesterol �ltimos 5 a�os","Fumador","No Fumador",
                          "Derrame cerebral","Sin Derrame cerebral","Problemas card�acos graves","Sin Problemas card�acos graves",
                          "Actividad f�sica","Sin Actividad f�sica","Consumo diario de fruta","No Consumo diario de fruta",
                          "Consumo diario de verdura","No Consumo dirio de verdura","Consumo habitual de alcohol","No Consumo habitual de alcohol",
                          "Seguro m�dico/cobertura sanitaria","Sin Seguro m�dico/cobertura sanitaria","No pudo costearse m�dico","S� pudo costearse m�dico",
                          "Dificultades subiendo escaleras","Sin dificultades subiendo escaleras","Sexo masculino","Sexo femenino")

nombre_var_ordinales=c("Autopercepci�n de salud 1","Autopercepci�n de salud 2","Autopercepci�n de salud 3","Autopercepci�n de salud 4",
                       "Autopercepci�n de salud 5","18-24 a�os","25-29 a�os","30-34 a�os","35-39 a�os","40-44 a�os","45-49 a�os","50-54 a�os",
                       "55-59 a�os","60-64 a�os","65-69 a�os","70-74 a�os","75-79 a�os","80 o m�s a�os","Nunca fue al colegio","Primaria","Secundaria",
                       "Equivalente a FP/Bachillerato","Menos de 4 a�os de universidad","Licenciado o superior","Menos de 10.000 $","10.000-15.000 $",
                       "15.000-20.000 $","20.000-25.000 $","25.000-35.000 $","35.000-50.000 $","50.000-75.000 $","M�s de 75.000 $")

nombre_var_discretas=c("BMI","D�as autorreportados de salud mental pobre �ltimo mes","D�as herido/lesionado �ltimo mes")

corregir_csv<-function(M) {
  a=M[,15]
  b=M[,21]
  c=M[,22]
  a[a<5]=0
  a[a==5]=1
  b[b<3]=1
  b[b>2]=0
  c[c<3]=1
  c[c>2]=0
  M[,15]=a
  M[,21]=b
  M[,22]=c
  return(M)
}

intervalos_proporciones<-function() {
  M=Datos
  v=var_categoricas2
  matriz=matrix(NA,nrow=length(v)*2,ncol=2)
  rownames(matriz)=nombre_var_categoricas2
  j=1
  for (i in v) {
    A=M[M[,i]==1,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+2
  }
  j=2
  for (i in v) {
    A=M[M[,i]==0,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+2
  }
  return(matriz)
}

intervalos_proporciones_ordinales<-function() {
  M=Datos
  matriz=matrix(NA,nrow=32,ncol=2)
  rownames(matriz)=nombre_var_ordinales
  j=1
  for (i in 1:5) {
    A=M[M[,15]==i,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  for (i in 1:13) {
    A=M[M[,20]==i,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  for (i in 1:6) {
    A=M[M[,21]==i,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  for (i in 1:8) {
    A=M[M[,22]==i,]
    media=mean(A[,1])
    desviacion=sqrt(media*(1-media)/nrow(A))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_diferencia_proporciones<-function() {
  M=corregir_csv(Datos)
  v=var_categoricas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_categoricas
  j=1
  for (i in v) {
    A=M[M[,i]==1,]
    B=M[M[,i]==0,]
    media_A=mean(A[,1])
    media_B=mean(B[,1])
    desviacion=sqrt((media_A*(1-media_A)/nrow(A))+(media_B*(1-media_B)/nrow(B)))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media_A-media_B,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_proporcion_diabeticos<-function() {
  M=corregir_csv(Datos)
  M=M[M[,"Diabetes_binary"]==1,]
  v=var_categoricas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_categoricas
  j=1
  for (i in v) {
    media=mean(M[,i])
    desviacion=sqrt(media*(1-media)/nrow(M))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_proporcion_no_diabeticos<-function() {
  M=corregir_csv(Datos)
  M=M[M[,"Diabetes_binary"]==0,]
  v=var_categoricas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_categoricas
  j=1
  for (i in v) {
    media=mean(M[,i])
    desviacion=sqrt(media*(1-media)/nrow(M))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_media_diabeticos<-function() {
  M=Datos
  M=M[M[,"Diabetes_binary"]==1,]
  v=var_discretas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_discretas
  j=1
  for (i in v) {
    media=mean(M[,i])
    estadistico=qt(0.025,df=nrow(M)-1,lower.tail=FALSE)*sd(M[,i])/sqrt(nrow(M))
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_media_no_diabeticos<-function() {
  M=Datos
  M=M[M[,"Diabetes_binary"]==0,]
  v=var_discretas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_discretas
  j=1
  for (i in v) {
    media=mean(M[,i])
    estadistico=qt(0.025,df=nrow(M)-1,lower.tail=FALSE)*sd(M[,i])/sqrt(nrow(M))
    matriz[j,]=c(media,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_diferencia_proporciones_diabeticos<-function() {
  M=corregir_csv(Datos)
  A=M[M[,"Diabetes_binary"]==1,]
  B=M[M[,"Diabetes_binary"]==0,]
  v=var_categoricas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_categoricas
  j=1
  for (i in v) {
    media_A=mean(A[,i])
    media_B=mean(B[,i])
    desviacion=sqrt((media_A*(1-media_A)/nrow(A))+(media_B*(1-media_B)/nrow(B)))
    estadistico=qnorm(0.025,0,1,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media_A-media_B,estadistico)
    j=j+1
  }
  return(matriz)
}

intervalos_diferencia_medias<-function() {
  A=Datos[Datos[,"Diabetes_binary"]==1,]
  B=Datos[Datos[,"Diabetes_binary"]==0,]
  v=var_discretas
  matriz=matrix(NA,nrow=length(v),ncol=2)
  rownames(matriz)=nombre_var_discretas
  j=1
  for (i in v) {
    media_A=mean(A[,i])
    media_B=mean(B[,i])
    desviacion=sqrt(var(A[,i])/nrow(A)+var(B[,i])/nrow(B))
    grados=(var(A[,i])/nrow(A)+var(B[,i])/nrow(B))^2/((var(A[,i])/nrow(A))^2/(nrow(A)-1)+(var(B[,i])/nrow(B))^2/(nrow(B)-1))
    estadistico=qt(0.025,df=grados,lower.tail=FALSE)*desviacion
    matriz[j,]=c(media_A-media_B,estadistico)
    j=j+1
  }
  return(matriz)
}


