#############################################################################################
#### Loading packages
#############################################################################################

if (!require(data.table)) install.packages("data.table")
if (!require(zoo)) install.packages("zoo")
if (!require(moments)) install.packages("moments")
if (!require(sandwich)) install.packages("sandwich")
if (!require(lubridate)) install.packages("lubridate")
if (!require(lmtest)) install.packages("lmtest")

#############################################################################################
#### Data loading
#############################################################################################

#daily stock data

setwd("D:/MFE/Curriculum/Spring 2018/431 - Quantitative Asset Management_herskovic/Homework/PS5")
dailyStock <- fread("./dailyStocks.csv")
monthlyStock <- fread("./monthlyStocks.csv") 
ffDaily <- fread("./F-F_Research_Data_Factors_daily.CSV", skip = 4, header = TRUE)
ffMonthly <- fread("./F-F_Research_Data_Factors.CSV", skip = 3, header = TRUE)

#############################################################################################
#### Data cleaning
#############################################################################################

#cleaning daily file
#dailyStock <- dailyStock[EXCHCD %in% c(1,2,3) & SHRCD %in% c(10,11),]
dailyStock <- dailyStock[EXCHCD %in% c(1,2,3), ]
dailyStock[, date := as.Date(as.character(date), "%Y%m%d")]
dailyStock[, YearMonth := as.yearmon(ymd(date))]

dailyStock[, prob := .N > 16, by = .(PERMNO, YearMonth)]
dailyStock <- dailyStock[prob==TRUE, ]

##changing character entries in RET and DLRET to "NA"
for (i in c('RET', 'DLRET')) {
  dailyStock[get(i) %in% c('', 'A', 'B', 'C', 'P', 'S', 'T'), paste0(i) := NA]
  dailyStock[, paste0(i) := as.numeric(get(i))]
  dailyStock[get(i) %in% c(-66, -77, -88, -99), paste0(i) := NA]
}

dailyStock[is.na(DLRET) & !is.na(RET),
     adjRet := RET][!is.na(DLRET) & is.na(RET),
                    adjRet := DLRET][!is.na(DLRET) & !is.na(RET),
                                     adjRet := (1 + RET)*(1 + DLRET) - 1]

#dailyStock[, adjRet:=RET]
dailyStock <- dailyStock[!is.na(adjRet)]
##cleaning monthly file

#monthlyStock <- monthlyStock[EXCHCD %in% c(1,2,3) & SHRCD %in% c(10,11),]
monthlyStock <- monthlyStock[EXCHCD %in% c(1,2,3),]
monthlyStock[, date := as.Date(as.character(date), "%Y%m%d")]
monthlyStock[, YearMonth := as.yearmon(ymd(date))]

#changing character entries in RET and DLRET to "NA"
for (i in c('RET', 'DLRET')) {
  monthlyStock[get(i) %in% c('', 'A', 'B', 'C', 'P', 'S', 'T'), paste0(i) := NA]
  monthlyStock[, paste0(i) := as.numeric(get(i))]
  monthlyStock[get(i) %in% c(-66, -77, -88, -99), paste0(i) := NA]
}

monthlyStock[is.na(DLRET) & !is.na(RET),
           adjRet := RET][!is.na(DLRET) & is.na(RET),
                          adjRet := DLRET][!is.na(DLRET) & !is.na(RET),
                                           adjRet := (1 + RET)*(1 + DLRET) - 1]
rm(i)

#monthlyStock[, adjRet:=RET]
monthlyStock[, Mkt_Cap := abs(PRC)*SHROUT][, 
                                           lag_Mkt_Cap := shift(Mkt_Cap), 
                                           by = PERMNO]


## FF daily
colnames(ffDaily)[1] <- "date"
ffDaily[, date:=as.Date(as.character(date), "%Y%m%d")]
ffDaily[, c(2:5)] <- ffDaily[, c(2:5)]/100

## FF monthly
colnames(ffMonthly)[1] <- "date"
ffMonthly[, c(2:5)] <- ffMonthly[, c(2:5)]/100
ffMonthly[,  YearMonth:=as.yearmon(ymd(paste0(date,"01")))]





#############################################################################################
#### Analysis using daily data :: putting stocks into quantiles 1 to 5 every month 
#############################################################################################
#total vol ranking
dailyStock[, totalVol:=sd(adjRet, na.rm = TRUE), by=c("PERMNO", "YearMonth")]

setkey(ffDaily, date)
setkey(dailyStock, date)
dailyStock <- merge(dailyStock, ffDaily, all.x = TRUE)
dailyStock[, adjRet_RF:=adjRet-RF]
dailyStock[, idioVol:=(sigma(lm(adjRet_RF~`Mkt-RF`+ SMB + HML, na.action = na.omit))), by=c("PERMNO", "YearMonth")]

setkey(dailyStock, PERMNO, YearMonth)
setkey(monthlyStock, PERMNO, YearMonth)
monthlyStock <- merge(monthlyStock, unique(dailyStock[, c("YearMonth", "PERMNO", "totalVol", "idioVol")]), by=c("YearMonth", "PERMNO"), all.x = TRUE)

setorder(monthlyStock, PERMNO, YearMonth)
monthlyStock[, `:=`(totalVol=shift(totalVol), idioVol=shift(idioVol)), by=c("PERMNO")]
monthlyStock <- monthlyStock[year(YearMonth)>=2013]


monthlyStock[, totalVolRank := as.integer(cut(totalVol, quantile(c(-Inf, totalVol, Inf), probs = 0:5/5, na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]
monthlyStock[, idioVolRank := as.integer(cut(idioVol, quantile(c(-Inf, idioVol, Inf), probs = 0:5/5, na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]



#############################################################################################
#### Analysis using monthly data :: putting stocks into quantiles 1 to 5 every month 
#############################################################################################

## calculating variables 
monthlyStock[!is.na(totalVolRank) , mktCap_totalVol:= (sum(lag_Mkt_Cap, na.rm = TRUE)), by=c("YearMonth")]
monthlyStock[!is.na(totalVolRank) , mktShare_totalVol:= sum(lag_Mkt_Cap, na.rm = TRUE)/mktCap_totalVol, by=c("YearMonth", "totalVolRank")]
#monthlyStock[ , size_totalVol:= log(mean(lag_Mkt_Cap, na.rm = TRUE)), by=c("YearMonth", "totalVolRank")]
monthlyStock[!is.na(totalVolRank) , wt_totalVol:= lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), by=c("YearMonth", "totalVolRank")]
monthlyStock[!is.na(totalVolRank) , Ret_totalVol:= sum(wt_totalVol*adjRet, na.rm = TRUE), by=c("YearMonth", "totalVolRank")]
totalVolRanking <- unique(monthlyStock[!is.na(totalVolRank), c("YearMonth", "totalVolRank", "Ret_totalVol", "mktShare_totalVol", "lag_Mkt_Cap")])
totalVolRanking <- merge(totalVolRanking, ffMonthly, by=c("YearMonth"), all.x = TRUE)


monthlyStock[!is.na(idioVolRank) , mktCap_IdioVol:= (sum(lag_Mkt_Cap, na.rm = TRUE)), by=c("YearMonth")]
monthlyStock[!is.na(idioVolRank)  , mktShare_IdioVol:= sum(lag_Mkt_Cap, na.rm = TRUE)/mktCap_IdioVol, by=c("YearMonth", "idioVolRank")]
#monthlyStock[ , size_idioVol:= log(mean(lag_Mkt_Cap, na.rm = TRUE)), by=c("YearMonth", "idioVolRank")]
monthlyStock[!is.na(idioVolRank)  , wt_idioVol:= lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), by=c("YearMonth", "idioVolRank")]
monthlyStock[!is.na(idioVolRank)  , Ret_idioVol:= sum(wt_idioVol*adjRet, na.rm = TRUE), by=c("YearMonth", "idioVolRank")]
idioVolRanking <- unique(monthlyStock[!is.na(idioVolRank), c("YearMonth", "idioVolRank", "Ret_idioVol", "mktShare_IdioVol", "lag_Mkt_Cap" )])
idioVolRanking <- merge(idioVolRanking, ffMonthly, by=c("YearMonth"), all.x = TRUE)



#regression to find alpha against MKT(capm alpha) and FF3
totalVolRanking[, Ret_totalVol_RF:=Ret_totalVol-RF ]
totalVolRanking[, `:=`(alpha_capm=coef(lm((Ret_totalVol_RF)~`Mkt-RF`))[1],
                       alpha_capm_tstat=coeftest(lm((Ret_totalVol_RF)~`Mkt-RF`), vcov = NeweyWest(lm((Ret_totalVol_RF)~`Mkt-RF`)))[1, 3],
                       alpha_ff3=coef(lm((Ret_totalVol_RF)~`Mkt-RF`+ SMB + HML))[1],
                       alpha_ff3_tstat=coeftest(lm((Ret_totalVol_RF)~`Mkt-RF`+ SMB + HML), vcov = NeweyWest(lm((Ret_totalVol_RF)~`Mkt-RF`+ SMB + HML)))[1, 3]),
                by=c("totalVolRank")]

idioVolRanking[, Ret_idioVol_RF:=Ret_idioVol-RF]
idioVolRanking[, `:=`(alpha_capm=coef(lm((Ret_idioVol_RF)~`Mkt-RF`))[1],
                       alpha_capm_tstat=coeftest(lm((Ret_idioVol_RF)~`Mkt-RF`), vcov = NeweyWest(lm((Ret_idioVol_RF)~`Mkt-RF`)))[1, 3],
                       alpha_ff3=coef(lm((Ret_idioVol_RF)~`Mkt-RF`+ SMB + HML))[1],
                       alpha_ff3_tstat=coeftest(lm((Ret_idioVol_RF)~`Mkt-RF`+ SMB + HML), vcov = NeweyWest(lm((Ret_idioVol_RF)~`Mkt-RF`+ SMB + HML)))[1, 3]),
                by=c("idioVolRank")]


#############################################################################################
#### Ouput
#############################################################################################

panalA <- matrix(nrow=5, ncol=9)

colnames(panalA) <- c("Rank", "Mean", "Std. Dev.", "%Mkt Share", "Size", "Capm Alpha", "capm alpha tstat", "FF-3 Alpha", "FF3 Alpha tstat" )

panalA[, 1] <- 1:5
panalA[, 2] <- setorder(totalVolRanking[, mean(Ret_totalVol), by=c("totalVolRank")], totalVolRank)$V1
panalA[, 3] <- setorder(totalVolRanking[, sd(Ret_totalVol), by=c("totalVolRank")], totalVolRank)$V1
panalA[, 4] <- setorder(totalVolRanking[, mean(mktShare_totalVol), by=c("totalVolRank")], totalVolRank)$V1
panalA[, 5] <- setorder(totalVolRanking[, mean(log(lag_Mkt_Cap/1000), na.rm = TRUE), by=c("totalVolRank")], totalVolRank)$V1
panalA[, 6] <- setorder( unique(totalVolRanking[, alpha_capm, by=c("totalVolRank")]), totalVolRank)$alpha_capm 
panalA[, 7] <- setorder( unique(totalVolRanking[, alpha_capm_tstat, by=c("totalVolRank")]), totalVolRank)$alpha_capm 
panalA[, 8] <- setorder( unique(totalVolRanking[, alpha_ff3, by=c("totalVolRank")]), totalVolRank)$alpha_ff3
panalA[, 9] <- setorder( unique(totalVolRanking[, alpha_ff3_tstat, by=c("totalVolRank")]), totalVolRank)$alpha_ff3

panalA[, c(2, 3, 4, 6, 8)] <- panalA[, c(2, 3, 4, 6, 8)]*100
panalA[, 2:9] <- round(panalA[, 2:9], 2)


panalB <- matrix(nrow=5, ncol=9)

colnames(panalB) <- c("Rank", "Mean", "Std. Dev.", "%Mkt Share", "Size", "Capm Alpha", "capm alpha tstat", "FF-3 Alpha" , "FF3 Alpha tstat")

panalB[, 1] <- 1:5
panalB[, 2] <- setorder(idioVolRanking[, mean(Ret_idioVol), by=c("idioVolRank")], idioVolRank)$V1
panalB[, 3] <- setorder(idioVolRanking[, sd(Ret_idioVol), by=c("idioVolRank")], idioVolRank)$V1
panalB[, 4] <- setorder(idioVolRanking[, mean(mktShare_IdioVol), by=c("idioVolRank")], idioVolRank)$V1
panalB[, 5] <- setorder(idioVolRanking[, mean(log(lag_Mkt_Cap/1000), na.rm = TRUE), by=c("idioVolRank")], idioVolRank)$V1
panalB[, 6] <- setorder( unique(idioVolRanking[, alpha_capm, by=c("idioVolRank")]), idioVolRank)$alpha_capm 
panalB[, 7] <- setorder( unique(idioVolRanking[, alpha_capm_tstat, by=c("idioVolRank")]), idioVolRank)$alpha_capm 
panalB[, 8] <- setorder( unique(idioVolRanking[, alpha_ff3, by=c("idioVolRank")]), idioVolRank)$alpha_ff3
panalB[, 9] <- setorder( unique(idioVolRanking[, alpha_ff3_tstat, by=c("idioVolRank")]), idioVolRank)$alpha_ff3

panalB[, c(2, 3, 4, 6, 8)] <- panalB[, c(2, 3, 4, 6, 8)]*100
panalB[, 2:9] <- round(panalB[, 2:9], 2)

panalA
panalB







