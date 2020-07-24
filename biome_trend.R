rm(list = ls())
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/sf/factor")

d <- read.csv("total_gpp.csv")


# set up the model----
mod1 <- lm(Total~year,data = d)
summary(mod1)
prd1=predict(mod1,interval="confidence",level=0.95)

mod2 <- lm(X0.30.N~year,data = d)
summary(mod2)
prd2=predict(mod2,interval="confidence",level=0.95)

mod3 <- lm(X30.90.N~year,data = d)
summary(mod3)
prd3=predict(mod3,interval="confidence",level=0.95)

mod4 <- lm(X0.30.S~year,data = d)
summary(mod4)
prd4=predict(mod4,interval="confidence",level=0.95)

mod5 <- lm(X30.90.S~year,data = d)
summary(mod5)
prd5=predict(mod5,interval="confidence",level=0.95)


par(mfrow=c(3,2))
# plot----
plot(d$year,d$Total,main = "Global", pch=20 ,col="green4",xlab = "Year",
     ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(150,175))
abline(mod1,lwd=1, col="green4")
lines(d$year,prd1[,2],lty=3,col="green4")
lines(d$year,prd1[,3],lty=3,col="green4")

plot.new()

plot(d$year,d$X0.30.N,main = "0-30N", pch=20 ,col="snow4",xlab = "Year",
    ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(45,55))
abline(mod2,lwd=1, col="snow4")
lines(d$year,prd2[,2],lty=3,col="snow4")
lines(d$year,prd2[,3],lty=3,col="snow4")

plot(d$year,d$X30.90.N,main = "30-90N", pch=20 ,col="deeppink2",xlab = "Year",
     ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(40,55))
abline(mod3,lwd=1, col="deeppink2")
lines(d$year,prd3[,2],lty=3,col="deeppink2")
lines(d$year,prd3[,3],lty=3,col="deeppink2")

plot(d$year,d$X0.30.S,main = "0-30S", pch=20 ,col="slategray3",xlab = "Year",
     ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(55,65))
abline(mod4,lwd=1, col="slategray3")
lines(d$year,prd4[,2],lty=3,col="slategray3")
lines(d$year,prd4[,3],lty=3,col="slategray3")

plot(d$year,d$X30.90.S,main = "30-90S", pch=20 ,col="brown",xlab = "Year",
     ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(5.5,7.5))
abline(mod5,lwd=1, col="brown")
lines(d$year,prd5[,2],lty=3,col="brown")
lines(d$year,prd5[,3],lty=3,col="brown")

# legend.txt <- c(paste("Global - ∆GPP =",
#                       round(summary(mod1)$coefficients[2,1],4)," ± ",
#                       round(summary(mod1)$coefficients[2,2],4),", p=",
#                       round(summary(mod1)$coefficients[2,4],4),
#                       sep = ""),
#                 expression(paste(CO[2]," - ∆GPP = ","0.124 ± 0.0012, p=0",
#                                  sep = "")),
#                 paste("0-30N - ∆GPP =",
#                       round(summary(mod2)$coefficients[2,1],4)," ± ",
#                       round(summary(mod2)$coefficients[2,2],4),", p=",
#                       round(summary(mod2)$coefficients[2,4],4),
#                       sep = ""),
#                 paste("30-90N - ∆GPP =",
#                       round(summary(mod3)$coefficients[2,1],4)," ± ",
#                       round(summary(mod3)$coefficients[2,2],4),", p=",
#                       round(summary(mod3)$coefficients[2,4],4),
#                       sep = ""),
#                 paste("0-30S - ∆GPP =",
#                       round(summary(mod4)$coefficients[2,1],4)," ± ",
#                       round(summary(mod4)$coefficients[2,2],4),", p=",
#                       round(summary(mod4)$coefficients[2,4],4),
#                       sep = ""),
#                 paste("30-90S - ∆GPP =",
#                       round(summary(mod5)$coefficients[2,1],4)," ± ",
#                       round(summary(mod5)$coefficients[2,2],4),", p=",
#                       round(summary(mod5)$coefficients[2,4],4),
#                       sep = ""))
# 
# legend("topleft",legend = legend.txt,cex = 0.75,
#        col = c("green4","snow4","deeppink2","slategray3","brown"),
#        bty = "n",lty = 1)
# 





