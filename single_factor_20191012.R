rm(list = ls())
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/sf/biome")

par(mfrow=c(5,2))
# Global----
d1 <- read.csv("global_sf.csv")
d <- d1-d1[1,2]
d$year <- d1$year

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

par(mar=c(1,2.5,1,2.5))
plot(d$year,d$all_variable, pch=20 ,col="white",xlab = "Year",
     bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i", yaxs ="i",
     ylim = c(-4,20),xlim = c(1982,2016))
axis(2,seq(-4,20,4),tcl=0.2,las=1,mgp=c(2,0.1,0))
title(ylab = expression(paste("Global ∆GPP (Pg C ",yr^-1,")")),line = 1.2)
# axis(1,seq(1982,2017,5),tcl=0.2,mgp=c(2,0.1,0))
# title(xlab = "YEAR",line = 1.3)
col2rgb("green2", alpha=TRUE) 
lines(d$year,prd1[,1],lwd=1, col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,2],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,3],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd1[,2], rev(prd1[,3])), 
        col =rgb(0, 238, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd2[,1],lty=2,col="brown")

lines(d$year,prd3[,1],lty=2,col="red2")

# lines(d$year,prd3[,1],lwd=1, col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,2],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,3],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# polygon(c(d$year,rev(d$year)), c(prd3[,2], rev(prd3[,3])), 
#         col =rgb(238,0, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd4[,1],lwd=1, col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,2],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,3],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd4[,2], rev(prd4[,3])), 
        col =rgb(0, 0,238, 127, maxColorValue=255) , border = NA)

lines(d$year,prd5[,1],lwd=1, col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,2],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,3],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd5[,2], rev(prd5[,3])), 
        col =rgb(190, 190,190, 127, maxColorValue=255) , border = NA)

lines(d$year,prd6[,1],lwd=1, col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,2],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,3],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd6[,2], rev(prd6[,3])), 
        col =rgb(0, 197,205, 127, maxColorValue=255) , border = NA)

cols <- c('green2','brown','red2','blue2','grey','turquoise3')
text.legend <- c("all variables",expression(paste(CO[2]," only")),"Temp only","fAPAR only","ppfd only","soil moisture only")
legend("topleft",legend = text.legend,lty = c(1,2,2,1,1,1),inset = c(0,-0.03),
       bty = "n",col = cols,cex=0.85, y.intersp = 0.85,ncol = 2,x.intersp = 0.6,text.width = 7.5)

chg <- d[35,2:7]
chg <- as.matrix(chg)
chg <- t(chg)

par(mar=c(1,2.5,1,2.5))
barplot(chg,bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i",horiz=TRUE,xlim = c(-4,20),space = 0.3,
        col = c('green2','brown','red2','blue2','grey','turquoise3'),beside = T)
legend("topright",fill=cols,col = cols,cex=0.9,bty = "n",inset = c(0,0),
       legend=text.legend, y.intersp = 0.85)
#axis(1,seq(-5,20,5),seq(-5,20,5),tcl=0.2,line = 0.2,mgp=c(2,0.1,0))
axis(2,tcl=0,cex.axis=0.001)
title(ylab = "Global GPP",line = 0.2)
#title(xlab = "Driver attributed change in C fluxes (Pg C)",line = 1.3)


# 0-30N----
d2 <- read.csv("N030_sf.csv")
d <- d2-d2[1,2]
d$year <- d2$year

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

par(mar=c(1,2.8,1,2))
plot(d$year,d$all_variable, pch=20 ,col="white",xlab = "Year",
     bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i", yaxs ="i",
     ylim = c(-2,6),xlim = c(1982,2016))
axis(4,seq(-2,6,2),tcl=0.2,las=1,mgp=c(2,0.1,0))
mtext(expression(paste("0-30N ∆GPP (Pg C ",yr^-1,")")),cex = 0.7, side=4, line=1.3)
# title(ylab = expression(paste("0-30N ∆GPP (Pg C ",yr^-1,")")),line = 1.3)
col2rgb("green2", alpha=TRUE) 
lines(d$year,prd1[,1],lwd=1, col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,2],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,3],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd1[,2], rev(prd1[,3])), 
        col =rgb(0, 238, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd2[,1],lty=2,col="brown")

lines(d$year,prd3[,1],lty=2,col="red2")

# lines(d$year,prd3[,1],lwd=1, col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,2],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,3],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# polygon(c(d$year,rev(d$year)), c(prd3[,2], rev(prd3[,3])), 
#         col =rgb(238,0, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd4[,1],lwd=1, col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,2],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,3],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd4[,2], rev(prd4[,3])), 
        col =rgb(0, 0,238, 127, maxColorValue=255) , border = NA)

lines(d$year,prd5[,1],lwd=1, col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,2],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,3],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd5[,2], rev(prd5[,3])), 
        col =rgb(190, 190,190, 127, maxColorValue=255) , border = NA)

lines(d$year,prd6[,1],lwd=1, col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,2],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,3],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd6[,2], rev(prd6[,3])), 
        col =rgb(0, 197,205, 127, maxColorValue=255) , border = NA)

cols <- c('green2','brown','red2','blue2','grey','turquoise3')
#text.legend <- c("all variables",expression(paste(CO[2]," only")),"Temp only","fAPAR only","ppfd only","soil moisture only")
#legend("topleft",legend = text.legend,lty = c(1,2,1,1,1,1),inset = c(0,-0.05),
      # bty = "n",col = cols,cex=0.85, y.intersp = 0.85,ncol = 2,x.intersp = 0.6,text.width = 7.5)

chg <- d[35,2:7]
chg <- as.matrix(chg)
chg <- t(chg)

par(mar=c(1,2,1,2.8))
barplot(chg,bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i",horiz=TRUE,xlim = c(-4,20),space = 0.3,
        col = c('green2','brown','red2','blue2','grey','turquoise3'),beside = T)
#legend("topright",fill=cols,col = cols,cex=0.9,bty = "n",
       #legend=text.legend, y.intersp = 0.85)
# axis(1,seq(-5,20,5),seq(-5,20,5),tcl=0.2,line = 0.5,mgp=c(2,0.1,0))
axis(4,tcl=0,cex.axis=0.001)
mtext("0-30N GPP", side=4, cex = 0.65,line=0.2)
# title(ylab = "0-30N GPP",line = 0.2)

# 30-90N----
d3 <- read.csv("N3090_sf.csv")
d <- d3-d3[1,2]
d$year <- d3$year

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

par(mar=c(1,2.5,1,2.5))
plot(d$year,d$all_variable, pch=20 ,col="white",xlab = "Year",
     bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i", yaxs ="i",
     ylim = c(-2,8),xlim = c(1982,2016))
axis(2,seq(-2,8,2),tcl=0.2,las=1,mgp=c(2,0.1,0))
title(ylab = expression(paste("30-90N ∆GPP (Pg C ",yr^-1,")")),line = 1.2)
# axis(1,seq(1982,2017,5),tcl=0.2,mgp=c(2,0.1,0))
# title(xlab = "YEAR",line = 1.3)
col2rgb("green2", alpha=TRUE) 
lines(d$year,prd1[,1],lwd=1, col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,2],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,3],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd1[,2], rev(prd1[,3])), 
        col =rgb(0, 238, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd2[,1],lty=2,col="brown")

lines(d$year,prd3[,1],lty=2,col="red2")

# lines(d$year,prd3[,1],lwd=1, col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,2],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,3],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# polygon(c(d$year,rev(d$year)), c(prd3[,2], rev(prd3[,3])), 
#         col =rgb(238,0, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd4[,1],lwd=1, col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,2],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,3],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd4[,2], rev(prd4[,3])), 
        col =rgb(0, 0,238, 127, maxColorValue=255) , border = NA)

lines(d$year,prd5[,1],lwd=1, col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,2],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,3],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd5[,2], rev(prd5[,3])), 
        col =rgb(190, 190,190, 127, maxColorValue=255) , border = NA)

lines(d$year,prd6[,1],lwd=1, col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,2],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,3],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd6[,2], rev(prd6[,3])), 
        col =rgb(0, 197,205, 127, maxColorValue=255) , border = NA)

cols <- c('green2','brown','red2','blue2','grey','turquoise3')
text.legend <- c("all variables",expression(paste(CO[2]," only")),"Temp only","fAPAR only","ppfd only","soil moisture only")
#legend("topleft",legend = text.legend,lty = c(1,2,1,1,1,1),inset = c(0,-0.05),
 #      bty = "n",col = cols,cex=0.8, y.intersp = 0.85,ncol = 2,x.intersp = 0.6,text.width = 7.5)

chg <- d[35,2:7]
chg <- as.matrix(chg)
chg <- t(chg)

par(mar=c(1,2.5,1,2.5))
barplot(chg,bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i",horiz=TRUE,xlim = c(-4,20),space = 0.3,
        col = c('green2','brown','red2','blue2','grey','turquoise3'),beside = T)
#legend("topright",fill=cols,col = cols,cex=0.9,bty = "n",inset = c(0,0),
      # legend=text.legend, y.intersp = 0.85)
#axis(1,seq(-5,20,5),seq(-5,20,5),tcl=0.2,line = 0.2,mgp=c(2,0.1,0))
axis(2,tcl=0,cex.axis=0.001)
title(ylab = "30-90N GPP",line = 0.2)
#title(xlab = "Driver attributed change in C fluxes (Pg C)",line = 1.3)


# 0-30S----
d4 <- read.csv("S030_sf.csv")
d <- d4-d4[1,2]
d$year <- d4$year

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

par(mar=c(1,2.8,1,2))
plot(d$year,d$all_variable, pch=20 ,col="white",xlab = "Year",
     bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i", yaxs ="i",
     ylim = c(-2,6),xlim = c(1982,2016))
axis(4,seq(-2,6,2),tcl=0.2,las=1,mgp=c(2,0.1,0))
mtext(expression(paste("0-30S ∆GPP (Pg C ",yr^-1,")")),cex = 0.7, side=4, line=1.3)
# title(ylab = expression(paste("0-30N ∆GPP (Pg C ",yr^-1,")")),line = 1.3)
col2rgb("green2", alpha=TRUE) 
lines(d$year,prd1[,1],lwd=1, col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,2],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,3],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd1[,2], rev(prd1[,3])), 
        col =rgb(0, 238, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd2[,1],lty=2,col="brown")

lines(d$year,prd3[,1],lty=2,col="red2")

# lines(d$year,prd3[,1],lwd=1, col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,2],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,3],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# polygon(c(d$year,rev(d$year)), c(prd3[,2], rev(prd3[,3])), 
#         col =rgb(238,0, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd4[,1],lwd=1, col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,2],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,3],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd4[,2], rev(prd4[,3])), 
        col =rgb(0, 0,238, 127, maxColorValue=255) , border = NA)

lines(d$year,prd5[,1],lwd=1, col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,2],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,3],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd5[,2], rev(prd5[,3])), 
        col =rgb(190, 190,190, 127, maxColorValue=255) , border = NA)

lines(d$year,prd6[,1],lwd=1, col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,2],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,3],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd6[,2], rev(prd6[,3])), 
        col =rgb(0, 197,205, 127, maxColorValue=255) , border = NA)

cols <- c('green2','brown','red2','blue2','grey','turquoise3')
#text.legend <- c("all variables",expression(paste(CO[2]," only")),"Temp only","fAPAR only","ppfd only","soil moisture only")
#legend("topleft",legend = text.legend,lty = c(1,2,1,1,1,1),inset = c(0,-0.05),
# bty = "n",col = cols,cex=0.85, y.intersp = 0.85,ncol = 2,x.intersp = 0.6,text.width = 7.5)

chg <- d[35,2:7]
chg <- as.matrix(chg)
chg <- t(chg)

par(mar=c(1,2,1,2.8))
barplot(chg,bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i",horiz=TRUE,xlim = c(-4,20),space = 0.3,
        col = c('green2','brown','red2','blue2','grey','turquoise3'),beside = T)
#legend("topright",fill=cols,col = cols,cex=0.9,bty = "n",
#legend=text.legend, y.intersp = 0.85)
# axis(1,seq(-5,20,5),seq(-5,20,5),tcl=0.2,line = 0.5,mgp=c(2,0.1,0))
axis(4,tcl=0,cex.axis=0.001)
mtext("0-30S GPP", side=4, cex = 0.65,line=0.2)
# title(ylab = "0-30N GPP",line = 0.2)

# 30-90S----
d5 <- read.csv("S3090_sf.csv")
d <- d5-d5[1,2]
d$year <- d5$year

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

par(mar=c(2.5,2.5,0.5,2.5))
plot(d$year,d$all_variable, pch=20 ,col="white",xlab = "Year",
     bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i", yaxs ="i",
     ylim = c(-0.5,1.5),xlim = c(1982,2016))
axis(2,seq(-0.5,1.5,1),tcl=0.2,las=1,mgp=c(2,0.1,0))
title(ylab = expression(paste("30-90S ∆GPP (Pg C ",yr^-1,")")),line = 1.2)
axis(1,seq(1982,2017,5),tcl=0.2,mgp=c(2,0.1,0))
title(xlab = "YEAR",line = 1.3)
col2rgb("green2", alpha=TRUE) 
lines(d$year,prd1[,1],lwd=1, col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,2],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
lines(d$year,prd1[,3],lty=1,col=rgb(0, 238, 0, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd1[,2], rev(prd1[,3])), 
        col =rgb(0, 238, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd2[,1],lty=2,col="brown")

lines(d$year,prd3[,1],lty=2,col="red2")

# lines(d$year,prd3[,1],lwd=1, col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,2],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# lines(d$year,prd3[,3],lty=1,col=rgb(238,0, 0, 127, maxColorValue=255))
# polygon(c(d$year,rev(d$year)), c(prd3[,2], rev(prd3[,3])), 
#         col =rgb(238,0, 0, 127, maxColorValue=255) , border = NA)

lines(d$year,prd4[,1],lwd=1, col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,2],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
lines(d$year,prd4[,3],lty=1,col=rgb(0, 0,238, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd4[,2], rev(prd4[,3])), 
        col =rgb(0, 0,238, 127, maxColorValue=255) , border = NA)

lines(d$year,prd5[,1],lwd=1, col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,2],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
lines(d$year,prd5[,3],lty=1,col=rgb(190, 190,190, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd5[,2], rev(prd5[,3])), 
        col =rgb(190, 190,190, 127, maxColorValue=255) , border = NA)

lines(d$year,prd6[,1],lwd=1, col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,2],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
lines(d$year,prd6[,3],lty=1,col=rgb(0, 197,205, 127, maxColorValue=255))
polygon(c(d$year,rev(d$year)), c(prd6[,2], rev(prd6[,3])), 
        col =rgb(0, 197,205, 127, maxColorValue=255) , border = NA)

cols <- c('green2','brown','red2','blue2','grey','turquoise3')
text.legend <- c("all variables",expression(paste(CO[2]," only")),"Temp only","fAPAR only","ppfd only","soil moisture only")
#legend("topleft",legend = text.legend,lty = c(1,2,1,1,1,1),inset = c(0,-0.05),
#      bty = "n",col = cols,cex=0.8, y.intersp = 0.85,ncol = 2,x.intersp = 0.6,text.width = 7.5)

chg <- d[35,2:7]
chg <- as.matrix(chg)
chg <- t(chg)

par(mar=c(2.5,2.5,0.5,2.5))
barplot(chg,bty= "n",ann = F,xaxt="n",yaxt="n",xaxs = "i",horiz=TRUE,xlim = c(-4,20),space = 0.3,
        col = c('green2','brown','red2','blue2','grey','turquoise3'),beside = T)
#legend("topright",fill=cols,col = cols,cex=0.9,bty = "n",inset = c(0,0),
# legend=text.legend, y.intersp = 0.85)
axis(1,seq(-5,20,5),seq(-5,20,5),tcl=0.2,line = 0.2,mgp=c(2,0.1,0))
axis(2,tcl=0,cex.axis=0.001)
title(ylab = "30-90S GPP",line = 0.2)
title(xlab = "Driver attributed change in C fluxes (Pg C)",line = 1.3)

