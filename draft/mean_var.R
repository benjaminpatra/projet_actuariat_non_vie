Y <- freMTPLfreq$ClaimNb
E <- freMTPLfreq$Exposure
m=sum(Y)/sum(E)
v=sum((Y-m*E)^2)/sum(E)

cat("average =",m," variance =",v,"phi =",v/m,"\n")


X=as.factor(freMTPLfreq$DriverAge)
for(i in 1:length(levels(X))){
  Ei=E[X==levels(X)[i]]
  Yi=Y[X==levels(X)[i]]
  meani=mean(Yi)
  variancei=sum((Yi-meani*Ei)^2)/sum(Ei)
  cat("DriverAge , age",levels(X)[i],"average =",meani ," variance =",
      variancei ,"\n")
  plot(meani,variancei,cex=sqrt(Ei),col="grey",pch=19,
       xlab="Empirical average",ylab="Empirical variance")
  points(meani ,variancei ,cex=sqrt(Ei))
  }

