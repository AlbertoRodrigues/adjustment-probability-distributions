require(ggplot2)
require(fitdistrplus)
require(gridExtra)
require(VGAM)
x=rgamma(100,2,2)

ajuste=function(dados)
{
  dist1=fitdist(dados,"gamma");est1=dist1$estimate
  dist2=fitdist(dados,"lnorm");est2=dist2$estimate
  dist3=fitdist(dados,"weibull");est3=dist3$estimate
  dist4=fitdist(dados,"logis");est4=dist4$estimate
  dist5=fitdist(dados,"norm");est5=dist5$estimate
  dist6=fitdist(dados,"cauchy");est6=dist6$estimate
  dist7=fitdist(dados,"unif");est7=dist7$estimate
  
  a=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")+
     stat_function(fun = dgamma, args = list( est1[1], est1[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Gama")+theme_minimal())
  
  
  b=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
    +stat_function(fun = dweibull, args = list( est3[1], est3[2]),aes(fill="black"),size=1.2)
    +labs(title="Distribuição Weibull")+theme_minimal()
    )
  c=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = dnorm, args = list( est5[1], est5[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Normal")+theme_minimal())
  
  d=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = dlogis, args = list( est4[1], est4[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Logística")+theme_minimal())
  
  e=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = dlnorm, args = list( est2[1], est2[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Lognormal")+theme_minimal())
  
  f=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = drayleigh, args = list(sqrt(sum(dados^2)/(2*length(dados))) ),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Rayleigh")+theme_minimal())
  
  g=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = dcauchy, args = list(est6[1],est6[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Cauchy")+theme_minimal())
  
  h=(ggplot(data.frame(x=dados),aes(x))+geom_histogram(aes(y =..density..),fill="#00AFBB",color="black")
     +stat_function(fun = dunif, args = list(est7[1],est7[2]),aes(fill="black"),size=1.2)
     +labs(title="Distribuição Uniforme")+theme_minimal())
  
  a1=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qgamma, dparams = est1) +
    stat_qq_line(distribution = qgamma, dparams = est1)
  
  a2=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qnorm, dparams = est5) +
    stat_qq_line(distribution = qnorm, dparams = est5)
  
  a3=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qlnorm, dparams = est2) +
    stat_qq_line(distribution = qlnorm, dparams = est2)
  
  a4=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qweibull, dparams = est3) +
    stat_qq_line(distribution = qweibull, dparams = est3)
  
  b1=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qlogis, dparams = est4) +
    stat_qq_line(distribution = qlogis, dparams = est4)
  
  b2=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qrayleigh, dparams = sqrt(sum(x^2)/(2*length(x)))) +
    stat_qq_line(distribution = qrayleigh, dparams = sqrt(sum(x^2)/(2*length(x))))
  
  b3=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qcauchy, dparams = est6) +
    stat_qq_line(distribution = qcauchy, dparams = est6)
  
  b4=ggplot(data.frame(x=dados), aes(sample=x)) +
    stat_qq(distribution = qunif, dparams = est7) +
    stat_qq_line(distribution = qunif, dparams = est7)
  
  grafico1=grid.arrange(a,b,c,d,ncol=2)
  grafico2=grid.arrange(e,f,g,h,ncol=2)
  grafico3=grid.arrange(a1,a2,a3,a4,ncol=2)
  grafico4=grid.arrange(b1,b2,b3,b4,ncol=2)
  lista_de_graficos=list(grafico1,grafico2,grafico3,grafico4)
  return(lista_de_graficos)
}

ajuste=ajuste(x)
ajuste[[1]]
ajuste(rrayleigh(100,2))

dist1=fitdist(x,"gamma");est1=dist1$estimate
dist2=fitdist(x,"lnorm");est2=dist2$estimate
dist3=fitdist(x,"weibull");est3=dist3$estimate
dist4=fitdist(x,"logis");est4=dist4$estimate
dist5=fitdist(x,"norm");est5=dist5$estimate
dist6=fitdist(x,"cauchy");est6=dist6$estimate
dist7=fitdist(x,"unif");est7=dist7$estimate

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qgamma, dparams = est1) +
  stat_qq_line(distribution = qgamma, dparams = est1)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qnorm, dparams = est5) +
  stat_qq_line(distribution = qnorm, dparams = est5)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qlnorm, dparams = est2) +
  stat_qq_line(distribution = qlnorm, dparams = est2)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qweibull, dparams = est3) +
  stat_qq_line(distribution = qweibull, dparams = est3)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qlogis, dparams = est4) +
  stat_qq_line(distribution = qlogis, dparams = est4)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qrayleigh, dparams = sqrt(sum(x^2)/(2*length(x)))) +
  stat_qq_line(distribution = qrayleigh, dparams = sqrt(sum(x^2)/(2*length(x))))

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qcauchy, dparams = est6) +
  stat_qq_line(distribution = qcauchy, dparams = est6)

ggplot(data.frame(x=x), aes(sample=x)) +
  stat_qq(distribution = qunif, dparams = est7) +
  stat_qq_line(distribution = qunif, dparams = est7)
