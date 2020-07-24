rm(list = ls())

# Read annual GPP data
path <- "/Users/wenjia/Desktop/PhD/20190415_C4/Global/annual_gpp"
filenames <- dir(path)[1:length(dir(path))]
filepath <- sapply(filenames, function(x){
  paste(path,x,sep = "/")
})
annual_gpp <- lapply(filepath, function(x){
  read.csv(x,header = F)
})

# Generate grip map of the trend of each grid
trend_grid <- matrix(NA,nrow = 360,ncol = 720)
sig_grid <- matrix(NA,nrow = 360,ncol = 720)

for (r in 1:360) {
  for (c in 1:720) {
    gpp_each_grid <- data.frame(year=seq(1982,2016),gpp=rep(0,35))
    for (y in 1:35) {
      gpp_each_grid$gpp[y] <- annual_gpp[[y]][r,c]
    }
    if (anyNA(gpp_each_grid)==T) {
      trend_grid[r,c] <- NA
      sig_grid[r,c] <- NA
    } else {
      mod <- lm(gpp~year,data = gpp_each_grid)
      trend_grid[r,c] <- summary(mod)$coefficients[2,1]
      sig_grid[r,c] <- summary(mod)$coefficients[2,4]
    }
  }
}

write.table(trend_grid,"/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/grid_trend.csv",sep = ",",na="NaN",row.names = F,col.names = F)
write.table(sig_grid,"/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/trend_sig.csv",sep = ",",na="NaN",row.names = F,col.names = F)

# Test if it's positive or negative trend
trend_grid[which(trend_grid>0)] <- 1
trend_grid[which(trend_grid<0)] <- -1

write.table(trend_grid,"/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/grid_trend_direction.csv",sep = ",",na="NaN",row.names = F,col.names = F)










