drought <- function(sw_in, tc, pn, lat, elev, g_cover) {
  source("splash_Main_point.R")
  SP_result_daily <- splash(sw_in,tc,pn, lat, elev)
  # for (r in 1:nrow(SP_result_daily)) {
  #   if (is.na(SP_result_daily$pet[r])==T) {
  #     SP_result_daily$pet[r] <- 0
  #   }
  #   if (is.na(SP_result_daily$aet[r])==T) {
  #     SP_result_daily$aet[r] <- 0
  #   }
  # }
  
  # process one year's result into monthly result
  nm <- matrix()
  for (y in start_year:end_year) {
    j=y-start_year+1
    for (m in 1:12) {
      nm[12*j-12+m] <- julian_day(y,m+1,1)-julian_day(y,m,1)
    }
  }
  nm_peryear <- cumsum(nm)
  
  pd <- seq(from=start_year,to=end_year)
  ny <- julian_day(pd + 1, 1, 1) - julian_day(pd, 1, 1)
  ny_peryear <- cumsum(ny)
  
  SP_result_mon <- as.data.frame(matrix(NA,nrow=length(pd)*12,ncol=3))
  names(SP_result_mon) <- c("wn","alpha","re_f")
  for (mon in 1:length(nm_peryear)) {
    SP_result_mon$wn[mon] <-mean(SP_result_daily$wn[(nm_peryear[mon]-nm[mon]+1):nm_peryear[mon]])/150
  }
  for (yr in 1:length(pd)) {
    SP_result_mon$alpha[(yr*12-11):(yr*12)] <- mean(na.omit(SP_result_daily$aet[(ny_peryear[yr]-ny[yr]+1):ny_peryear[yr]]))/mean(na.omit(SP_result_daily$pet[(ny_peryear[yr]-ny[yr]+1):ny_peryear[yr]]))
  }
  
  for (k in 1:nrow(SP_result_mon)) {
    if (SP_result_mon$wn[k] > 0.9) {
      SP_result_mon$re_f[k] <- 1
    } else if (SP_result_mon$wn[k] <= 0.9) {
      if (g_cover==0) {
        phi_0 <- 0.179 + 0.45*SP_result_mon$alpha[k]
      } else {
        phi_0 <- 0.101 + 0.0063*SP_result_mon$alpha[k]
      }
      beta <- (phi_0 - 1) / 0.81
      SP_result_mon$re_f[k] <- beta * (SP_result_mon$wn[k]-0.9)^2 +1
    }
  }
  # return(SP_result_mon)
  return(SP_result_mon$re_f)
}

