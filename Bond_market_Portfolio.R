if (!require(data.table)) install.packages("data.table")
if (!require(tidyr)) install.packages("tidyr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(moments)) install.packages("moments")
if (!require(zoo)) install.packages("zoo")


setwd("D:/MFE/Curriculum/Spring 2018/431 - Quantitative Asset Management_herskovic/Homework/PS2")

options(warn=-1)
CRSP_bonds <- fread("./CRSP_BOND.csv", header = TRUE)
CRSP_bonds$MCALDT<- as.Date(strptime(as.character(CRSP_bonds$MCALDT), "%m/%d/%Y"), "%Y%m%d")
options(warn=0)


PS2_Q1 <- function(CRSP_bonds) {
  
  ### datacleaning
  
  #Replace NA in total amount outstanding to 0
  CRSP_bonds$TMTOTOUT[is.na(CRSP_bonds$TMTOTOUT)] <- 0
  
  #Replaceing Values -99 in return with 0 (no data available)
  CRSP_bonds$TMRETNUA[CRSP_bonds$TMRETNUA==-99] <- 0
  
  #Total amount outstanding is in millions
  CRSP_bonds[, TMTOTOUT:=TMTOTOUT*1000000]
  
  
  ### Calculation 
  CRSP_bonds[, yearmonth:=format(as.Date(MCALDT), "%Y-%m") ]
  
  CRSP_bonds[, sum_outstanding_amount:=sum(TMTOTOUT), by=yearmonth]
  CRSP_bonds[, vw_weight:=TMTOTOUT/sum_outstanding_amount]
  
  setorder(CRSP_bonds, KYCRSPID, yearmonth)
  #CRSP_bonds[, Bond_lag_MV:= shift(sum_outstanding_amount), by=KYCRSPID]
  CRSP_bonds[, vw_weight_lastMonth:= shift(vw_weight), by=KYCRSPID]
  
  CRSP_bonds[, vwret:=vw_weight_lastMonth*TMRETNUA]
  CRSP_bonds[, Bond_Vw_Ret:=sum(vwret, na.rm = TRUE), by=yearmonth]
  CRSP_bonds[, Bond_Ew_Ret:=mean(TMRETNUA), by=yearmonth]
  
  PS2_Q1_output <- CRSP_bonds[, c("yearmonth", "Bond_Ew_Ret", "Bond_Vw_Ret", "sum_outstanding_amount")]
  PS2_Q1_output=PS2_Q1_output[!duplicated(yearmonth)]
  
  setorder(PS2_Q1_output, yearmonth)
  PS2_Q1_output[, Bond_lag_MV:= shift(sum_outstanding_amount)]
  
  PS2_Q1_output <- separate(PS2_Q1_output, yearmonth, into = c("Year", "Month"), sep="-")
  PS2_Q1_output$Bond_lag_MV <- PS2_Q1_output$Bond_lag_MV/1000000
  PS2_Q1_output <- PS2_Q1_output[complete.cases(PS2_Q1_output),]
  setorder(PS2_Q1_output, Year, Month)
  
  PS2_Q1_output <- PS2_Q1_output[, c("Year", "Month", "Bond_lag_MV", "Bond_Ew_Ret", "Bond_Vw_Ret")]
  
  
  return(PS2_Q1_output)
  
}

PS2_Q1_output <- PS2_Q1(CRSP_bonds)
Monthly_CRSP_Bonds <- PS2_Q1_output



#### Question 2

#Calling funtion from previous assignment PS1_Q1
source("../PS1/PS1_005061880.R")
PS1_Q1_output <- PS1_Q1(CRSP_Stocks)
Monthly_CRSP_Stocks <- PS1_Q1_output


Monthly_CRSP_Riskless <- fread("../PS2/Riskless.csv", header = TRUE)
Monthly_CRSP_Riskless$caldt <- as.Date(as.character(Monthly_CRSP_Riskless$caldt), "%Y%m%d")
Monthly_CRSP_Riskless[, caldt:=format(as.Date(caldt), "%Y%m") ]
colnames(Monthly_CRSP_Riskless)[1] <- "date"
Monthly_CRSP_Riskless$date <- as.integer(Monthly_CRSP_Riskless$date)


PS2_Q2 <- function(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless){
  
  Monthly_CRSP_Bonds <- unite(Monthly_CRSP_Bonds, date, c("Year", "Month"), sep="")
  Monthly_CRSP_Bonds$date <- as.integer(Monthly_CRSP_Bonds$date)
  
  Monthly_CRSP_Stocks <- unite(Monthly_CRSP_Stocks, date, c("Year", "Month"), sep="")
  Monthly_CRSP_Stocks$date <- as.integer(Monthly_CRSP_Stocks$date)
  
  temp <- merge(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds)
  aggregate_data <- merge(temp, Monthly_CRSP_Riskless)
  
  
  aggregate_data <- separate(aggregate_data, date, into = c("Year", "Month"), sep=4)
  
  aggregate_data$Stock_Excess_Vw_Ret <- aggregate_data$Stock_Vw_Ret - aggregate_data$t30ret
  aggregate_data$Bond_Excess_Vw_Ret <- aggregate_data$Bond_Vw_Ret - aggregate_data$t30ret
  
  PS2_Q2_output <- aggregate_data[, .SD, .SDcols=c('Year', 'Month', 'Stock_lag_MV', 'Stock_Excess_Vw_Ret', 'Bond_lag_MV', 'Bond_Excess_Vw_Ret')]
  
  
  return (PS2_Q2_output)
  
}

PS2_Q2_output <- PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless)
Monthly_CRSP_Universe <- PS2_Q2_output

#Ques 3

PS2_Q3 <- function(Monthly_CRSP_Universe) {
  
  #stock_Excess_Vw_Ret volatility
  vol_37_toend <- rollapply(Monthly_CRSP_Universe$Stock_Excess_Vw_Ret, width=36, function(x) sd(x))
  vol_1_to_36 <- rollapply(Monthly_CRSP_Universe$Stock_Excess_Vw_Ret, width=seq_len(nrow(Monthly_CRSP_Universe)), sd, align="right")
  vol_stock <- c(vol_1_to_36[1:36], vol_37_toend[1:length(vol_37_toend)-1])
  Monthly_CRSP_Universe$volatiliy_stock <- vol_stock
  
  #Bond_Excess_Vw_Ret
  vol_37_toend <- rollapply(Monthly_CRSP_Universe$Bond_Excess_Vw_Ret, width=36, function(x) sd(x))
  vol_1_to_36 <- rollapply(Monthly_CRSP_Universe$Bond_Excess_Vw_Ret, width=seq_len(nrow(Monthly_CRSP_Universe)), sd, align="right")
  vol_bond <- c(vol_1_to_36[1:36], vol_37_toend[1:length(vol_37_toend)-1])
  Monthly_CRSP_Universe$volatiliy_bond <- vol_bond
  
  
 
  Monthly_CRSP_Universe[, Excess_Vw_Ret:= (Stock_lag_MV/(Stock_lag_MV+Bond_lag_MV))*Stock_Excess_Vw_Ret
                        +(Bond_lag_MV/(Stock_lag_MV+Bond_lag_MV))*Bond_Excess_Vw_Ret]
  
  Monthly_CRSP_Universe[, Excess_60_40_Ret:=0.6*Stock_Excess_Vw_Ret+0.4*Bond_Excess_Vw_Ret]
  Monthly_CRSP_Universe[, Stock_inverse_sigma_hat:=1/volatiliy_stock]
  Monthly_CRSP_Universe[, Bond_inverse_sigma_hat:=1/volatiliy_bond]
  
  Monthly_CRSP_Universe[, Unlevered_k:=1/(Stock_inverse_sigma_hat+Bond_inverse_sigma_hat)]
  Monthly_CRSP_Universe[, w_Unlevered_stock:=Unlevered_k*Stock_inverse_sigma_hat]
  Monthly_CRSP_Universe[, w_Unlevered_bond:=Unlevered_k*Bond_inverse_sigma_hat]
  
  
  setorder(Monthly_CRSP_Universe, Year, Month)
  Monthly_CRSP_Universe[, w_Unlevered_stock:=shift(w_Unlevered_stock)]
  Monthly_CRSP_Universe[, w_Unlevered_bond:=shift(w_Unlevered_bond)]
  
  
  Monthly_CRSP_Universe[, Excess_Unlevered_RP_Ret:=w_Unlevered_stock*Stock_Excess_Vw_Ret 
                        + w_Unlevered_bond*Bond_Excess_Vw_Ret]
  
  #ex post realized volatility of the benchmark 
  vol_benchmark <- sd(Monthly_CRSP_Universe$Excess_Vw_Ret)
    
  #vol(kx)=K*vol(x) using this formula k=?
  Monthly_CRSP_Universe[, levered_k := ( vol_benchmark/(sd(Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret
                                                                      +Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret, na.rm = TRUE)) )]
  
  
  Monthly_CRSP_Universe[, w_levered_stock:=levered_k*Stock_inverse_sigma_hat]
  Monthly_CRSP_Universe[, w_levered_bond:=levered_k*Bond_inverse_sigma_hat]
  
  
  setorder(Monthly_CRSP_Universe, Year, Month)
  Monthly_CRSP_Universe[, w_levered_stock:=shift(w_levered_stock)]
  Monthly_CRSP_Universe[, w_levered_bond:=shift(w_levered_bond)]
  
  
  Monthly_CRSP_Universe[, Excess_Levered_RP_Ret:=w_levered_stock*Stock_Excess_Vw_Ret 
                        + w_levered_bond*Bond_Excess_Vw_Ret]
  
  
  
  
  PS2_Q3_output <- Monthly_CRSP_Universe[, .SD, .SDcols=c('Year', 'Month', 'Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 'Excess_Vw_Ret', 'Excess_60_40_Ret', 
                                                          'Stock_inverse_sigma_hat', 'Bond_inverse_sigma_hat', 'Unlevered_k', 'Excess_Unlevered_RP_Ret', 'levered_k', 
                                                          'Excess_Levered_RP_Ret')]
  
  
  return(PS2_Q3_output)
}

PS2_Q3_output=PS2_Q3(Monthly_CRSP_Universe)
Port_Rets <- PS2_Q3_output



### Question 4

PS2_Q4 <- function(Port_Rets){
  PS2_Q4_output <- matrix(nrow = 6, ncol = 6)
  rownames(PS2_Q4_output) <- c('CRSP stocks', 'CRSP bonds', 'Value-weighted portfolio', '60/40 portfolio', 'unlevered RP', 'levered RP')
  colnames(PS2_Q4_output) <- c('Annualized Mean', 't-stat of Annualized Mean', 'Annualized Standard Deviation', 'Annualized Sharpe Ratio', 'Skewness', 'Excess Kurtosis')
  
  Port_Rets <- unite(Port_Rets, date, c("Year", "Month"), sep="")
  Port_Rets$date <- as.integer(Port_Rets$date)
  
  Port_Rets <- Port_Rets[date %in% 192901:201006 ]
  Port_Rets <- separate(Port_Rets, date, into = c("Year", "Month"), sep=4)
  
  
  PS2_Q4_output[, 1] <- 12*colMeans(Port_Rets[, c('Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 
                                               'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Excess_Unlevered_RP_Ret', 'Excess_Levered_RP_Ret')])
  
  
  PS2_Q4_output[, 2] <- as.numeric(Port_Rets[, lapply(.SD, function(x) { as.numeric(t.test(x)[1]) }), .SDcols=c('Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 
                                                                                                     'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Excess_Unlevered_RP_Ret', 'Excess_Levered_RP_Ret')])
  
  PS2_Q4_output[, 3] <- as.numeric(Port_Rets[, lapply(.SD, function(x) sqrt(12)*sd(x) ), .SDcols=c('Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 
                                                                 'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Excess_Unlevered_RP_Ret', 'Excess_Levered_RP_Ret')])
  PS2_Q4_output[, 4] <-  PS2_Q4_output[, 1]/PS2_Q4_output[, 3]
  
  PS2_Q4_output[, 5] <- as.numeric(Port_Rets[, lapply(.SD, function(x) skewness(x) ), .SDcols=c('Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 
                                                                                                   'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Excess_Unlevered_RP_Ret', 'Excess_Levered_RP_Ret')])
  PS2_Q4_output[, 6] <- as.numeric(Port_Rets[, lapply(.SD, function(x) kurtosis(x)-3 ), .SDcols=c('Stock_Excess_Vw_Ret', 'Bond_Excess_Vw_Ret', 
                                                                                                   'Excess_Vw_Ret', 'Excess_60_40_Ret', 'Excess_Unlevered_RP_Ret', 'Excess_Levered_RP_Ret')])
  return(PS2_Q4_output)
  
}

PS2_Q4_output <- PS2_Q4(Port_Rets)





