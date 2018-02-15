# rbasic-learn
learn r to handle better with econometric exercise, esp. standard linear regression model

   
############ Stock parameters

x=150
sigma=0.3/sqrt(365)
st=c(seq(140,149,1),149.99,150.01,seq(151,160,1))
r_f=0.1/365
t=15 #in days



##################


i=0  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  c=st*pnorm(d1)-x*exp(-r_f*(t-i))*pnorm(d2)  

plot(st, c, type="l", lty=1, xlab="Stock Price at time (t)", ylab="Call Value", main="Evolution of Call Value",ylim=c(0,max(c)),lwd=2)
abline(v=x,lty=2)
text(max(st)-2,max(c),"Value at t=0",cex=0.7)
text(x+5,max(c),"In the money region",cex=0.75)
text(x-5,max(c),"Out of money region",cex=0.75)
text(x,max(c)-3,"At the money",cex=0.75)
text(x,max(c)-3,"At the money",cex=0.75)
text(min(st)+2,max(c),paste0("Sigma =",round(100*sigma*sqrt(365),2),"% per year"),cex=0.75)
text(min(st)+2,max(c)-0.5,paste0("Rf =",round(100*r_f*365,2),"% per year"),cex=0.75)
text(min(st)+2,max(c)-1,paste0("Strike =",x),cex=0.75)
text(min(st)+2,max(c)-1.5,paste0("Time to maturity =",t, "days"),cex=0.75)


finpayoff <- ifelse(st-x>=0,st-x,0)
for (i in 1:t){
  if(i==t){Sys.sleep(1.8)  
    lines(st,finpayoff,col=paste0("grey",80-i*5),lwd=2)
    text(max(st)-2,max(finpayoff)-2,"Value at t=T",cex=0.7)
    }else{
    Sys.sleep(1)  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  c=st*pnorm(d1)-x*exp(-r_f*(t-i))*pnorm(d2)    
lines(st,c,col=paste0("grey",80-i*5))}
}



#### call delta over time
i=0  
d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  delta=pnorm(d1)
plot(st,delta , type="l", lty=1, xlab="Stock Price at time (t)", ylab="Delta", main="Evolution of Call option Delta",ylim=c(0,1.2))
abline(v=x,lty=2)
text(max(st)-2,max(delta)-.05,"Value at t=0",cex=0.7)
text(x+5,1.2,"In the money region",cex=0.75)
text(x-5,1.2,"Out of money region",cex=0.75)
text(x,1.1,"At the money",cex=0.75)


findelta <- ifelse(st-x>=0,1,0)
for (i in 1:t){
  if(i==t){Sys.sleep(1.8)  
    lines(st,findelta,col=paste0("grey",80-i*5))
    text(max(st)-2,max(findelta)+.05,"Value at t=T",cex=0.7)
    }else{
    Sys.sleep(1)  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
   delta=pnorm(d1)  
lines(st,delta,col=paste0("grey",80-i*5))}
}



#### call Gamma over time
i=0  
d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  gamma=dnorm(d1)/(st*sigma*sqrt(t-i))
plot(st,gamma, type="l", lty=1, xlab="Stock Price at time (t)", ylab="Gamma", main="Evolution of Call option Gamma",ylim=c(0,0.2))
abline(v=x,lty=2)
text(max(st)-2,max(gamma)-.01,"Value at t=0",cex=0.7)
text(x+5,0.2,"In the money region",cex=0.75)
text(x-5,0.2,"Out of money region",cex=0.75)
text(x,0.19,"At the money",cex=0.75)


fingamma <- ifelse(st==x,500,0)
for (i in 1:t){
  if(i==t){Sys.sleep(1.8)  
    lines(st,fingamma,col=paste0("grey",80-i*5))
    }else{
    Sys.sleep(1)  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
   gamma=dnorm(d1)/(st*sigma*sqrt(t-i))
lines(st,gamma,col=paste0("grey",80-i*5))}
}



############# Evolution of put value

i=0  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  c=st*pnorm(d1)-x*exp(-r_f*(t-i))*pnorm(d2)  
  p=c-st+x*exp(-r_f*(t-i))
  p1=x*exp(-r_f*(t-i))*pnorm(-d2) - st*pnorm(-d1)

plot(st, p, type="l", lty=1, xlab="Stock Price at time (t)", ylab="Put Value", main="Evolution of Put Value",ylim=c(0,max(c)),lwd=2)
abline(v=x,lty=2)
text(min(st)-2,max(c),"Value at t=0",cex=0.7)
text(x+5,max(p),"Out of the money region",cex=0.75)
text(x-5,max(p),"In the money region",cex=0.75)
text(x,max(p)-3,"At the money",cex=0.75)
text(max(st)-2,max(p),paste0("Sigma =",round(100*sigma*sqrt(365),2),"% per year"),cex=0.75)
text(max(st)-2,max(p)-0.5,paste0("Rf =",round(100*r_f*365,2),"% per year"),cex=0.75)
text(max(st)-2,max(p)-1,paste0("Strike =",x),cex=0.75)
text(min(st)-2,max(c)-1.5,paste0("Time to maturity =",t, "days"),cex=0.7)

finpayoff <- ifelse(st-x<=0,-st+x,0)
for (i in 1:t){
  if(i==t){Sys.sleep(1.8)  
    lines(st,finpayoff,col=paste0("grey",80-i*5),lwd=2)
    text(min(st)+2,max(finpayoff)-2,"Value at t=T",cex=0.7)
    }else{
    Sys.sleep(1)  
  d2=(log(st/x)+(r_f-sigma^2/2)*(t-i))/(sigma*sqrt(t-i))
  d1=d2+(sigma*sqrt(t-i))
  c=st*pnorm(d1)-x*exp(-r_f*(t-i))*pnorm(d2)    
  p=c-st+x*exp(-r_f*(t-i))
  p1=x*exp(-r_f*(t-i))*pnorm(-d2) - st*pnorm(-d1)
lines(st,p,col=paste0("grey",80-i*5))}
}

