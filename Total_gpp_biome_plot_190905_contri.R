rm(list =ls())
setwd("/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/ann_cli_gpp")
fn <- dir()

area_grid <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/area.csv",header = F)
bio_gpp <- matrix(rep(0,210),nrow = 35,ncol = 6)
bio_gpp[1:35,1] <- seq(1982,2016)

for (f in 1:35) {
  annual_gpp <- read.csv(fn[f],header = F)
  
  #calculate total gpp by multipling gpp per m2 with the area of each 0.5 degree grid
  total_gpp <- annual_gpp * area_grid
  # save total_gpp
  # out_file <- paste("/Users/shirley/Desktop/REALM/New_run_20181106/total_gpp/",fn[f],sep = "")
  # write.table(total_gpp,out_file,sep = ",",na="NaN",row.names = F,col.names = F)
  
  # assign NaN to zero before calculating total global gpp
  total_gpp[total_gpp=="NaN"] <- 0
  bio_gpp[f,2] <- sum(total_gpp[121:180,])/(1e+15) 
  bio_gpp[f,3] <- sum(total_gpp[1:120,])/(1e+15) 
  bio_gpp[f,4] <- sum(total_gpp[181:240,])/(1e+15)
  bio_gpp[f,5] <- sum(total_gpp[241:360,])/(1e+15)
  
  global_gpp <- sum(total_gpp)/(1e+15) 
  bio_gpp[f,6] <- global_gpp
  print(paste("Global GPP in ", substr(fn[f],1,4), " is ", global_gpp, " Pg C per year",sep = ""))
}

bio_gpp <- as.data.frame(bio_gpp)
names(bio_gpp) <- c("year","0-30 N","30-90 N","0-30 S","30-90 S","Total")
# write.csv(bio_gpp,"/Users/shirley/Desktop/REALM/C4_2:3/Global_2:3_20190318/single_trend/CO2/gpp_global.csv",row.names = F)

# # get the model
# mod1 <- lm(bio_gpp$`0-30 N`~bio_gpp$year,data = bio_gpp)
# summary(mod1)
# prd1=predict(mod1,interval="confidence",level=0.95)
# 
# mod2 <- lm(bio_gpp$`30-90 N`~bio_gpp$year,data = bio_gpp)
# summary(mod2)
# prd2=predict(mod2,interval="confidence",level=0.95)
# 
# mod3 <- lm(bio_gpp$`0-30 S`~bio_gpp$year,data = bio_gpp)
# summary(mod3)
# prd3=predict(mod3,interval="confidence",level=0.95)
# 
# mod4 <- lm(bio_gpp$`30-90 S`~bio_gpp$year,data = bio_gpp)
# summary(mod4)
# prd4=predict(mod4,interval="confidence",level=0.95)
# 
# # plot
# par(mfrow=c(2,2))
# plot(bio_gpp$year,bio_gpp$`0-30 N`,main = "GPP trend in 0-30 N from 1982 to 2016", pch=20 ,col="skyblue3",xlab = "Year",
#      ylab = expression(paste("GPP (Pg C ",yr^-1,")")))
# abline(mod1,lwd=1, col="skyblue3")
# lines(bio_gpp$year,prd1[,2],lty=3,col="skyblue3")
# lines(bio_gpp$year,prd1[,3],lty=3,col="skyblue3")
# legend("topleft",bty = "n",cex=1,lty = 1, col = "skyblue3",
#        legend = c(paste("∆GPP =",round(summary(mod1)$coefficients[2,1],4)," ± ",
#                         round(summary(mod1)$coefficients[2,2],4),", p<0.01",
#                         sep = "")
#        ))
# 
# plot(bio_gpp$year,bio_gpp$`30-90 N`,main = "GPP trend in 30-90 N from 1982 to 2016", pch=20 ,col="snow4",xlab = "Year",
#      ylab = expression(paste("GPP (Pg C ",yr^-1,")")))
# abline(mod2,lwd=1, col="snow4")
# lines(bio_gpp$year,prd2[,2],lty=3,col="snow4")
# lines(bio_gpp$year,prd2[,3],lty=3,col="snow4")
# legend("topleft",bty = "n",cex=1,lty = 1, col = "skyblue3",
#        legend = c(paste("∆GPP =",round(summary(mod2)$coefficients[2,1],4)," ± ",
#                         round(summary(mod2)$coefficients[2,2],4),", p<0.01",
#                         sep = "")
#        ))
# 
# plot(bio_gpp$year,bio_gpp$`0-30 S`,main = "GPP trend in 0-30 S from 1982 to 2016", pch=20 ,col="deeppink2",xlab = "Year",
#      ylab = expression(paste("GPP (Pg C ",yr^-1,")")))
# abline(mod3,lwd=1, col="deeppink2")
# lines(bio_gpp$year,prd3[,2],lty=3,col="deeppink2")
# lines(bio_gpp$year,prd3[,3],lty=3,col="deeppink2")
# legend("topleft",bty = "n",cex=1,lty = 1, col = "skyblue3",
#        legend = c(paste("∆GPP =",round(summary(mod3)$coefficients[2,1],4)," ± ",
#                         round(summary(mod3)$coefficients[2,2],4),", p<0.01",
#                         sep = "")
#        ))
# 
# plot(bio_gpp$year,bio_gpp$`30-90 S`,main = "GPP trend in 30-90 S from 1982 to 2016", pch=20 ,col="brown",xlab = "Year",
#      ylab = expression(paste("GPP (Pg C ",yr^-1,")")))
# abline(mod4,lwd=1, col="brown")
# lines(bio_gpp$year,prd4[,2],lty=3,col="brown")
# lines(bio_gpp$year,prd4[,3],lty=3,col="brown")
# legend("topleft",bty = "n",cex=1,lty = 1, col = "skyblue3",
#        legend = c(paste("∆GPP =",round(summary(mod4)$coefficients[2,1],4)," ± ",
#                         round(summary(mod4)$coefficients[2,2],4),", p<0.01",
#                         sep = "")
#        ))


# Total Trend
mod0 <- lm(Total~year,data = bio_gpp)
summary(mod0)
# prd0=predict(mod0,interval="confidence",level=0.95)
# 
# plot(bio_gpp$year,bio_gpp$Total,main = "Total Global GPP trend from 1982 to 2016", pch=20 ,col="lavenderblush3",
#      ylim = c(139,143),
#      xlab = "Year",
#      ylab = expression(paste("GPP (Pg C ",yr^-1,")")))
# abline(mod0,lwd=1, col="lavenderblush3")
# lines(bio_gpp$year,prd0[,2],lty=3,col="lavenderblush3")
# lines(bio_gpp$year,prd0[,3],lty=3,col="lavenderblush3")
# legend("topleft",bty = "n",cex=1,lty = 1, col = "lavenderblush3",
#        legend = c(paste("∆GPP =",round(summary(mod0)$coefficients[2,1],4)," ± ",
#                         round(summary(mod0)$coefficients[2,2],4),", p<0.01",
#                         sep = "")
#        ))

bio_gpp$mod.values <- mod0$fitted.values
write.csv(bio_gpp,"/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/gpp_global_climateexclu.csv",row.names = F)


