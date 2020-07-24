rm(list = ls())
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/sf/biome")

d <- read.csv("global_sf.csv")


# set up the model----
mod1 <- lm(all_variable~year,data = d)
summary(mod1)
prd1=predict(mod1,interval="confidence",level=0.95)

mod2 <- lm(CO2_only~year,data = d)
summary(mod2)
prd2=predict(mod2,interval="confidence",level=0.95)

mod3 <- lm(Tc_only~year,data = d)
summary(mod3)
prd3=predict(mod3,interval="confidence",level=0.95)

mod4 <- lm(fAPAR_only~year,data = d)
summary(mod4)
prd4=predict(mod4,interval="confidence",level=0.95)

mod5 <- lm(ppfd_only~year,data = d)
summary(mod5)
prd5=predict(mod5,interval="confidence",level=0.95)

mod6 <- lm(soil_mois_only~year,data = d)
summary(mod6)
prd6=predict(mod6,interval="confidence",level=0.95)

# plot----
plot(d$year,d$all_variable,main = "0~30N GPP trend from 1982 to 2016", pch=20 ,col="green4",xlab = "Year",
     ylab = expression(paste("GPP (Pg C ",yr^-1,")")),ylim = c(150,175))
abline(mod1,lwd=1, col="green4")
lines(d$year,prd1[,2],lty=3,col="green4")
lines(d$year,prd1[,3],lty=3,col="green4")

points(d$year,d$CO2_only, pch=20,col="snow4")
abline(mod2,lwd=1, col="snow4")
lines(d$year,prd2[,2],lty=3,col="snow4")
lines(d$year,prd2[,3],lty=3,col="snow4")

cs <- paste(
  round(summary(mod2)$coefficients[2,1],4)," ± ",
  round(summary(mod2)$coefficients[2,2],4),", p=",
  round(summary(mod2)$coefficients[2,4],4),
  sep = "")
print(cs)

points(d$year,d$Tc_only, pch=20,col="deeppink2")
abline(mod3,lwd=1, col="deeppink2")
lines(d$year,prd3[,2],lty=3,col="deeppink2")
lines(d$year,prd3[,3],lty=3,col="deeppink2")

points(d$year,d$fAPAR_only, pch=20,col="slategray3")
abline(mod4,lwd=1, col="slategray3")
lines(d$year,prd4[,2],lty=3,col="slategray3")
lines(d$year,prd4[,3],lty=3,col="slategray3")

points(d$year,d$ppfd_only, pch=20,col="brown")
abline(mod5,lwd=1, col="brown")
lines(d$year,prd5[,2],lty=3,col="brown")
lines(d$year,prd5[,3],lty=3,col="brown")

points(d$year,d$soil_mois_only, pch=20,col="turquoise3")
abline(mod6,lwd=1, col="turquoise3")
lines(d$year,prd6[,2],lty=3,col="turquoise3")
lines(d$year,prd6[,3],lty=3,col="turquoise3")


legend.txt <- c(paste("all variable - ∆GPP =",
                      round(summary(mod1)$coefficients[2,1],4)," ± ",
                      round(summary(mod1)$coefficients[2,2],4),", p=",
                      round(summary(mod1)$coefficients[2,4],4),
                      sep = ""),
                expression(paste(CO[2]," - ∆GPP = ","0.124 ± 0.0012, p=0",
                                 sep = "")),
                paste("Temp - ∆GPP =",
                      round(summary(mod3)$coefficients[2,1],4)," ± ",
                      round(summary(mod3)$coefficients[2,2],4),", p=",
                      round(summary(mod3)$coefficients[2,4],4),
                      sep = ""),
                paste("fAPAR - ∆GPP =",
                      round(summary(mod4)$coefficients[2,1],4)," ± ",
                      round(summary(mod4)$coefficients[2,2],4),", p=",
                      round(summary(mod4)$coefficients[2,4],4),
                      sep = ""),
                paste("ppfd - ∆GPP =",
                      round(summary(mod5)$coefficients[2,1],4)," ± ",
                      round(summary(mod5)$coefficients[2,2],4),", p=",
                      round(summary(mod5)$coefficients[2,4],4),
                      sep = ""),
                paste("drought - ∆GPP =",
                      round(summary(mod6)$coefficients[2,1],4)," ± ",
                      round(summary(mod6)$coefficients[2,2],4),", p=",
                      round(summary(mod6)$coefficients[2,4],4),
                      sep = ""))

legend("topleft",legend = legend.txt,cex = 0.75,
       col = c("green4","snow4","deeppink2","slategray3","brown","turquoise3"),
       bty = "n",lty = 1)







