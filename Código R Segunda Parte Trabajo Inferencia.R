parameters<-function(n) {
  sample=rpareto(n,1,3)
  beta=min(sample)
  div=sample/beta
  lg=log(div)
  S=sum(lg)
  alpha=(n-1)/S
  return(alpha)
}

plotalpha<-function(n) {
  v=c()
  for (i in 1:n) {
    v=c(v,parameters(i))
    if (i%%1000==0) {print(i)}
  }
  res=v/(v-1)
  plot(1:n,res,type="l")
}

work<-function() {                                    # Esta es la función usada en la segunda parte del trabajo. Se ejecuta tal cual work(). 
  g=replicate(40,parameters(10))
  res=g/(g-min(rpareto(10,1,3)))
  hist(res)
  m=mean(res)
  s=sd(res)
  samp=replicate(40,mean(rpareto(10,1,3)))
  hist(samp)
  m2=mean(samp)
  s2=sd(samp)
  return(matrix(c(m,s,m2,s2),nrow=2,byrow=TRUE))
}

alphaest<-function(n,a,b) {
  sample=rpareto(n,b,a)
  beta=min(sample)
  m=mean(sample)
  mu=a*b/(a-1)
  prod=1
  for (i in 1:n) {
    prod=prod*sample[i]^(1/n)
  }
  est=prod
  alpha=1/(log(est)-log(beta))
  meann=alpha*beta/(alpha-1)
  div=sample/beta
  lg=log(div)
  S=sum(lg)
  alph=(n)/S
  menn=alph*beta/(alph-1)
  return(c(meann,menn,m,mu))
}

alphaest2<-function(n) {
  sample=rpareto(n,1,3)
  m=mean(sample)
  med=median(sample)
  return(c(med,m))
}

work2<-function() {
  samp=replicate(40,mean(rpareto(10,1,3)))
  hist(samp)
}

graphic<-function(n,a,b) {
  sample=rpareto(n,b,a)
  beta=min(sample)
  prod=1
  for (i in 1:n) {
    prod=prod*sample[i]^(1/n)
  }
  est=prod
  alpha=1/(log(est)-log(beta))
  meann=alpha*beta/(alpha-1)
}

replicator<-function(n,a,b){
  res=c()
  for (i in 1:n) {
    res=c(res,graphic(i,a,b))
  }
  plot(1:n,res,type="l")
  mu=a*b/(a-1)
  abline(h=mu)
}

replicator2<-function(n,a,b) {
  v=c()
  for (i in 1:n) {
    v=c(v,mean(rpareto(i,b,a)))
  }
  plot(1:n,v,type="l")
  mu=a*b/(a-1)
  abline(h=mu)
}


