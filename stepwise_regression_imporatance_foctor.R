library(corrplot)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(PerformanceAnalytics)
library(car)
library(leaps)
#for (r in 1:360) {
  #for (c in 1:720) {

# Senario 1
# r=20
# c=200

#Scnario 2
# r= 58
# c = 47
    # initiate matrix for multiple regression
    g_data <- matrix(rep(0,245),nrow = 35,ncol = 7)
    g_data <- as.data.frame(g_data)
    names(g_data) <- c("gpp","CO2","tc","vpd","fAPAR","ppfd","sw_stress")
    for (i in 1:35) {
      g_data$gpp[i] <- gpp[[i]][r,c]
      g_data$CO2[i] <- CO2$V2[i]
      g_data$tc[i] <- tc[[i]][r,c]
      g_data$vpd[i] <- vpd[[i]][r,c]
      g_data$fAPAR[i] <- fAPAR[[i]][r,c]
      g_data$ppfd[i] <- ppfd[[i]][r,c]
      g_data$sw_stress[i] <- sw_stress[[i]][r,c]
    }
    
    # if (anyNA(g_data)==F) {
    #   print(c(r,c))
    # }
    fit <- lm(gpp~CO2+tc+fAPAR+ppfd+vpd+sw_stress,data = g_data)
    summary(fit)
    
    # get dependencies and p value
    cor_attitude <- cor(g_data)
    cor2 <- rcorr(as.matrix(g_data))
    # 实现参数和p可视化
    symnum(cor_attitude)
    chart.Correlation(g_data,histogram = TRUE,pch=19)
    
    vif(fit, digits = 3)
    # stepwise method
    lm1.step <- step(fit, direction = "backward")
    
    # Anova test on the fitness
    fit2 <- lm(lm1.step[["call"]][["formula"]],data=g_data)
    # fit2 <- lm(gpp~tc+ppfd+vpd+sw_stress, data=g_data)
    anova(fit,fit2) # if p>0.5, not significantly difference, lower AIC model 
    # 如果检验不显著（p > 0.05），则使用较少变量的线性模型。
    # 使用AIC（Akaike Information Criterion，赤池信息准则）比较模型：AIC值越小的模型越优先选择。
    
    # 全子集回归
    leaps<-regsubsets(gpp~CO2+tc+fAPAR+ppfd+vpd+sw_stress,data = g_data, nbest=6)
    plot(leaps,scale='adjr2')
    # 证明全子集回归和其他回归方法得到的结果是一样的
    
    #相对重要性
    z_data=as.data.frame ( scale ( g_data ) )
    zfit=lm (lm1.step[["call"]][["formula"]],data=g_data)
    coef ( zfit )
    
#  }
#}
