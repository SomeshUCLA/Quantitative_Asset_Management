################################################################################################
#############             PS3                                                ###################
#############             coded by: Somesh Srivastava                        ###################
################################################################################################


if (!require(data.table)) install.packages("data.table")
if (!require(ggplot2)) install.packages("ggplot")
if (!require(zoo)) install.packages("zoo")
if (!require(lubridate)) install.packages("lubridate")
if (!require(moments)) install.packages("moments")



setwd("D:/MFE/Curriculum/Spring 2018/431 - Quantitative Asset Management_herskovic/Homework/PS3")

################################################################################################
#############             data reading                                       ###################
################################################################################################


CRSP_Stocks <- fread("./CRSP_Stocks.csv", header = TRUE)
CRSP_Stocks$date<- as.Date(as.character(CRSP_Stocks$date), "%Y%m%d")


################################################################################################
#############             PS3 Ques 1                                         ###################
################################################################################################

PS3_Q1 <- function(CRSP_Stocks) {
  
  ### datacleaning
  
  #Taking only Common shares (share code 10, 11) and Stocks traded in NYSE, AMEX, NASDAQ
  CRSP_Stocks <- CRSP_Stocks[(CRSP_Stocks$SHRCD %in% c(10, 11))  & (CRSP_Stocks$EXCHCD %in% c(1, 2, 3))]
  
  ## Number of share outstanding is in 1000 numbers so multiplying by 1000
  CRSP_Stocks[, SHROUT:=SHROUT*1000, ]
  
  #holding period return - marking NA for non numeric returns
  CRSP_Stocks$RET <- as.numeric(as.character(CRSP_Stocks$RET))
  
  #Negative price are bid-ask average
  CRSP_Stocks$PRC = abs(CRSP_Stocks$PRC)
  CRSP_Stocks$PRC <- as.numeric(as.character(CRSP_Stocks$PRC))

  #Delist Return
  #Change all special cases to NA
  CRSP_Stocks$DLRET <- as.numeric(as.character(CRSP_Stocks$DLRET))
  CRSP_Stocks$adjRET = ifelse(!is.na(CRSP_Stocks$RET), ifelse(is.na(CRSP_Stocks$DLRET), CRSP_Stocks$RET,((1+CRSP_Stocks$DLRET)*(1+CRSP_Stocks$RET)-1) ), CRSP_Stocks$DLRET)
  
  
  CRSP_Stocks[, market_cap := PRC*SHROUT]
  setorder(CRSP_Stocks, PERMNO, date)
  
  CRSP_Stocks[, lag_Mkt_cap:= shift(market_cap), by=PERMNO]
  
  #If a firm doesn't meet all of these requirements, it isn't included in any portfolio
  # 4.c price at t-13 not missing
  # 4.d ret(t-2) not missing
  # 4.e market_cap(t-1) not missing
  
  CRSP_Stocks[, flag:=ifelse( is.na( shift(PRC, 13)*shift(adjRET, 2)*shift(market_cap, 1) ), 0, 1), by=PERMNO]
  
  CRSP_Stocks[, log_ret:=log(1+adjRET)]
  
  CRSP_Stocks$log_ret[is.na(CRSP_Stocks$log_ret)] <- 0
  
  CRSP_Stocks[, Ranking_Ret := Reduce(`+`, shift(log_ret, 2:12)), by=PERMNO]
  
  
  CRSP_Stocks <- CRSP_Stocks[flag==1]
  
  CRSP_Stocks[, YearMonth := as.yearmon(ymd(date))]

  #creating output
  # filter for 1927 to 2017
  #filter for flag 1
  
  PS3_Q1_output <- CRSP_Stocks[year(date) %in% 1927:2017, c("YearMonth", "PERMNO", "EXCHCD" ,"lag_Mkt_cap", "adjRET", "Ranking_Ret")]
  
  return(PS3_Q1_output)
  
}

CRSP_Stocks_Momentum <- PS3_Q1(CRSP_Stocks)



################################################################################################
#############             PS3 Ques 2                                         ###################
################################################################################################

PS3_Q2 <- function(CRSP_Stocks_Momentum) {
  
  CRSP_Stocks_Momentum[, DM_decile := cut(Ranking_Ret, quantile(Ranking_Ret, probs = 0:10/10), include.lowest = TRUE, labels = FALSE), by=YearMonth]
  CRSP_Stocks_Momentum[, KRF_decile := as.integer(cut(Ranking_Ret, quantile(c(-Inf, Ranking_Ret[EXCHCD==1], Inf), probs = 0:10/10, na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]
  
  PS3_Q2_output <- CRSP_Stocks_Momentum[, c("YearMonth", "PERMNO", "lag_Mkt_cap", "adjRET", "DM_decile", "KRF_decile")]
  
  return (PS3_Q2_output)
}


CRSP_Stocks_Momentum_decile <- PS3_Q2(CRSP_Stocks_Momentum)


################################################################################################
#############             PS3 Ques 3                                         ###################
################################################################################################

FF_mkt <- fread("../PS1/F-F_Research_Data_Factors.CSV", skip=3, header = TRUE)
FF_mkt <- FF_mkt[, .(date=V1, `Mkt-RF` = `Mkt-RF`/100, SMB = SMB/100, HML=HML/100, RF=RF/100)]
FF_mkt[,  YearMonth:=as.yearmon(ymd(paste0(date,"01")))]

PS3_Q3 <- function(CRSP_Stocks_Momentum_decile, FF_mkt){
  
  #value weighted return for each month for each quantile DM portfolio
  CRSP_Stocks_Momentum_decile[, DM_Ret:= sum(adjRET*(lag_Mkt_cap/sum(lag_Mkt_cap)), na.rm = TRUE), by=c("YearMonth", "DM_decile")]
  
  #value weighted return for each month for each quantile KRF portfolio
  CRSP_Stocks_Momentum_decile[, KRF_Ret:= sum(adjRET*(lag_Mkt_cap/sum(lag_Mkt_cap)), na.rm = TRUE), by=c("YearMonth", "KRF_decile")]
  
  DM_Momentum_ret <- CRSP_Stocks_Momentum_decile[, c("YearMonth", "DM_decile", "DM_Ret")]
  KRF_Momentum_ret <- CRSP_Stocks_Momentum_decile[, c("YearMonth", "KRF_decile", "KRF_Ret")]
  
  DM_Momentum_ret <- unique(DM_Momentum_ret)
  colnames(DM_Momentum_ret)[2] <- "Decile"
  KRF_Momentum_ret <- unique(KRF_Momentum_ret)
  colnames(KRF_Momentum_ret)[2] <- "Decile"
  
  CRSP_Stocks_Momentum_returns <- merge(DM_Momentum_ret, KRF_Momentum_ret, by=c("YearMonth", "Decile"))
  
  CRSP_Stocks_Momentum_returns <- merge(CRSP_Stocks_Momentum_returns, FF_mkt[, c("RF", "YearMonth")], by=c("YearMonth"))
  
  return(CRSP_Stocks_Momentum_returns)
  
}

CRSP_Stocks_Momentum_returns <- PS3_Q3(CRSP_Stocks_Momentum_decile, FF_mkt)


################################################################################################
#############             PS3 Ques 4                                         ###################
################################################################################################

PS3_Q4 <- function(CRSP_Stocks_Momentum_returns) {
  output <- matrix(nrow = 4, ncol = 11)
  colnames(output) <- c(paste0("Decile", 1:10), "WML")
  rownames(output) <- c("r-rf", "sigma", "SR", "sk(m)")
  meanexcess <- CRSP_Stocks_Momentum_returns[, 12*mean(DM_Ret-RF), by=c("Decile")]
  sd <- CRSP_Stocks_Momentum_returns[, sqrt(12)*sd(DM_Ret), by=c("Decile")]
  SR <- meanexcess/sd
  sk_m <- CRSP_Stocks_Momentum_returns[, skewness(log(1+DM_Ret)), by=c("Decile")]
  
  setorder(CRSP_Stocks_Momentum_returns, YearMonth, Decile)
  
  Decile1_ret <- CRSP_Stocks_Momentum_returns[Decile==1]
  Decile10_ret <- CRSP_Stocks_Momentum_returns[Decile==10]
  Decile_1_10_ret <- merge(Decile1_ret, Decile10_ret, by=c("YearMonth"), suffixes = c(1, 10))
  Decile_1_10_ret[, R_WML:=DM_Ret10-DM_Ret1]
  
  WML_meanexcess <- Decile_1_10_ret[, 12*mean(R_WML)]
  WML_sd <- Decile_1_10_ret[, sqrt(12)*sd(R_WML)]
  SR_WML <- WML_meanexcess/WML_sd
  sk_WML <- Decile_1_10_ret[, skewness(log(1+R_WML+RF1))]
  
  output[1, ] <- c(unlist(meanexcess[, 2]), WML_meanexcess)*100
  output[2, ] <- c(unlist(sd[, 2]), WML_sd)*100
  output[3, ] <- c(unlist(SR[, 2]), SR_WML)
  output[4, ] <- c(unlist(sk_m[, 2]), sk_WML)
  
  output[1, ] <- round(output[1, ], 1)
  output[2, ] <- round(output[2, ], 1)
  output[3, ] <- round(output[3, ], 2)
  output[4, ] <- round(output[4, ], 2)
  
  return(output)
  
  
}

output <- PS3_Q4(CRSP_Stocks_Momentum_returns)


################################################################################################
#############             PS3 Ques 5                                         ###################
################################################################################################


#data downloaded from kent Daniel Website
DM_returns <- fread("./DM_data_2017_03/m_m_pt_tot.txt")
DM_returns <- DM_returns[, 1:3]
setnames(DM_returns,c("date","Decile","DM_Ret_website"))
DM_returns[,date:=as.yearmon(ymd(date))]

#Kent French 10 portfolio based on momentum 
KRF_returns <- fread("./10_Portfolios_Prior_12_2.CSV")
setnames(KRF_returns,c("date",1:10))
KRF_returns[,date:=as.yearmon(ymd(paste0(date,"01")))]

KRF_returns <- melt(KRF_returns, id.vars="date", variable.name = "Decile", value.name = "KRF_Ret_website")
KRF_returns[, Decile := as.integer(Decile)]
KRF_returns[, KRF_Ret_website := KRF_Ret_website/100]


PS3_Q5 <- function(CRSP_Stocks_Momentum_returns, DM_returns, KRF_returns) {
  
  colnames(CRSP_Stocks_Momentum_returns)[1] <- "date"
  CRSP_Stocks_Momentum_returns <- merge(CRSP_Stocks_Momentum_returns, DM_returns, by=c("date", "Decile"))
  CRSP_Stocks_Momentum_returns <- merge(CRSP_Stocks_Momentum_returns, KRF_returns, by=c("date", "Decile"))
  
  setorder(CRSP_Stocks_Momentum_returns, date, Decile)
  
  CRSP_Stocks_Momentum_returns[, DM_WML_Estimated := DM_Ret-shift(DM_Ret, 9), by=c("date")]
  CRSP_Stocks_Momentum_returns[, KRF_WML_Estimated := KRF_Ret-shift(KRF_Ret, 9), by=c("date")]
  
  CRSP_Stocks_Momentum_returns[, DM_WML_website := DM_Ret_website-shift(DM_Ret_website, 9), by=c("date")]
  CRSP_Stocks_Momentum_returns[, KRF_WML_website := KRF_Ret_website-shift(KRF_Ret_website, 9), by=c("date")]
  
  
  corr <- matrix(nrow = 2, ncol = 11)
  rownames(corr) <- c("DM_correlation", "KRF_correlation")
  colnames(corr) <- c(paste0("Decile ", 1:10), "WML")
  
  DM_correlation <- CRSP_Stocks_Momentum_returns[, cor(DM_Ret, DM_Ret_website), by=c("Decile")]
  KRF_correlation <- CRSP_Stocks_Momentum_returns[, cor(KRF_Ret, KRF_Ret_website), by=c("Decile")]
  
  WML_DM_correlation <- CRSP_Stocks_Momentum_returns[, cor(DM_WML_Estimated, DM_WML_website, use = "complete.obs")]
  WML_KRF_correlation <- CRSP_Stocks_Momentum_returns[, cor(KRF_WML_Estimated, KRF_WML_website, use = "complete.obs")]
  
  corr[1, ] <- c(unlist(DM_correlation[, 2]), WML_DM_correlation)
  corr[2, ] <- c(unlist(KRF_correlation[, 2]), WML_KRF_correlation)
  
  corr <- round(corr, 4)
  
  return(list(corr, CRSP_Stocks_Momentum_returns))

}

PS3_Q5_output <- PS3_Q5(CRSP_Stocks_Momentum_returns, DM_returns, KRF_returns)
PS3_Q5_output[1]




################################################################################################
#############             PS3 Ques 6                                         ###################
################################################################################################


input6 <- as.data.table(PS3_Q5_output[2])
PS3_Q6 <- function(input6){
  Momentum_ret <- input6[!is.na(input6$DM_WML_website)]
  #taking recent data from 2007
  Momentum_ret <- Momentum_ret[year(date)>2006] 
  
  plot(Momentum_ret$date, Momentum_ret$DM_WML_website, type="l", axes = FALSE, xlab = "Year", ylab = "Return", main = "Momentum Return Anomaly")
  points(Momentum_ret$date, Momentum_ret$DM_WML_website, col="red", pch=19)
  axis(side=1, at=c(2007:2020))
  axis(side = 2, at=seq(-0.5, 0.2, 0.1))
  box()
  
}       
PS3_Q6(input6)       
       

