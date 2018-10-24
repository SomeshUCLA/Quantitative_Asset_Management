
#############################################################################################
# 
#############################################################################################

if (!require(data.table)) install.packages("data.table")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(zoo)) install.packages("zoo")
if (!require(lubridate)) install.packages("lubridate")
if (!require(purrr)) install.packages("purrr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(moments)) install.packages("moments")
if (!require(reshape2)) install.packages("reshape2")
if (!require(dplyr)) install.packages("dplyr")


#############################################################################################
#merge CRSP and Linktable. Get and preprocess your data (if necessary), 

setwd("D:/MFE/Curriculum/Spring 2018/431 - Quantitative Asset Management_herskovic/Homework/PS4")

#################################################################################################
## crsp stock file loading and pre-processing
#################################################################################################

CRSP <- fread("./crsp_stock.csv", header = TRUE)
CRSP <- CRSP[EXCHCD %in% c(1,2,3) & SHRCD %in% c(10,11),]
CRSP[, date := as.Date(as.character(date), "%Y%m%d")]
CRSP[, YearMonth := as.yearmon(ymd(date))][order(date)]


#changing character entries in RET and DLRET to "NA"
for (i in c('RET', 'DLRET')) {
  CRSP[get(i) %in% c('', 'A', 'B', 'C', 'P', 'S', 'T'), paste0(i) := NA]
  CRSP[, paste0(i) := as.numeric(get(i))]
  CRSP[get(i) %in% c(-66, -77, -88, -99), paste0(i) := NA]
}

CRSP[is.na(DLRET) & !is.na(RET),
     adjRet := RET][!is.na(DLRET) & is.na(RET),
                 adjRet := DLRET][!is.na(DLRET) & !is.na(RET),
                               adjRet := (1 + RET)*(1 + DLRET) - 1]

## Number of share outstanding is in 1000 numbers so multiplying by 1000
#CRSP[, SHROUT:=SHROUT*1000, ]

# When more than one PERMNO, calculate market cap with total 
CRSP[, Mkt_Cap := abs(PRC)*SHROUT][, 
                                      lag_Mkt_Cap := shift(Mkt_Cap), 
                                      by = PERMNO]

# CRSP[, PERMNO_weight := lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), 
#      by = .(PERMCO, date)][,lag_Mkt_Cap := sum(lag_Mkt_Cap), by = .(PERMCO, date)]

CRSP[, PERMNO_weight := lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), 
      by = .(PERMCO, date)][, 
                            adjRet := sum(PERMNO_weight*adjRet), 
                            by = .(PERMCO, date)][,lag_Mkt_Cap := sum(lag_Mkt_Cap), by = .(PERMCO, date)]



#################################################################################################
## Link file loading and pre-processing
#################################################################################################

Link_table <- fread("./linking.csv", header = TRUE)
Link_table <- Link_table[LINKPRIM == 'P' | LINKPRIM == 'C']
Link_table[, LINKDT := as.Date(as.character(LINKDT), "%Y%m%d")][,
                          LINKENDDT := as.Date(as.character(LINKENDDT), "%Y%m%d")][,
                              gvkey := as.integer(gvkey)][,LPERMNO := as.integer(LPERMNO)][,LPERMCO:= as.integer(LPERMCO)]

colnames(Link_table)[1]<- "GVKEY"
Link_table <- replace_na(Link_table, replace = list(LINKDT="1970-01-01", LINKENDDT="2017-12-31"))


#################################################################################################
## Compustat file loading and pre-processing and merging pension data from pension file
#################################################################################################

COMPDT <- fread("./compustat_fundamental.csv", header = TRUE)

COMPDT <- unique(COMPDT)
compustat_pension <- fread("./compustat_pension.csv")
compustat_pension[, datadate := as.yearmon(ymd(datadate))][,GVKEY:= as.integer(gvkey)]
compustat_pension <- compustat_pension[, c("datadate", "prba", "GVKEY")]

COMPDT[, DATE := as.Date(as.character(datadate), "%Y%m%d")][,GVKEY:= as.integer(GVKEY)]
compustat_pension[, DATE := as.Date(as.character(datadate), "%Y%m%d")][,GVKEY:= as.integer(GVKEY)]

compustat_pension <- compustat_pension[, c("DATE", "prba", "GVKEY")]

#merge pension table with compustat 
setkey(COMPDT, DATE, GVKEY)
setkey(compustat_pension, DATE, GVKEY)
COMPDT <- compustat_pension[COMPDT]



####################################################################################################
##  merging compustat and linking table based on GVKEY and LINKDT<=DATE<=LINKEDDDT
####################################################################################################
setkey(COMPDT, GVKEY, DATE)
setkey(Link_table, GVKEY, LINKDT, LINKENDDT)
#compustat_link <- COMPDT[Link_table, on=.( GVKEY=GVKEY, DATE >= LINKDT , DATE <= LINKENDDT )]
#compustat_link <- COMPDT[Link_table, on=.( GVKEY=GVKEY )]
compustat_link <- merge(COMPDT, Link_table, by=c("GVKEY"), allow.cartesian = TRUE)

### compute book equity
compustat_link[, SHE := seq][is.na(SHE),
                       SHE := ceq + pstk,
                       by = .(DATE, LPERMCO)][is.na(SHE),
                                             SHE := at - lt - mib,
                                             by = .(DATE, LPERMCO)][is.na(SHE),
                                                                   SHE := at - lt,
                                                                   by = .(DATE, LPERMCO)]

compustat_link[, dt := ifelse(!is.na(txditc), txditc, itcb+ txdb) ]
compustat_link[, ps := ifelse(!is.na(pstkrv), pstkrv, ifelse(!is.na(pstkl), pstkl, pstk))]

compustat_link <- replace_na(compustat_link, replace = list(SHE=0, dt=0, ps=0, prba=0))
compustat_link$be <- compustat_link[, SHE - ps + dt - prba]

# taking relevant columns from compustat_link
#compustat_link <- compustat_link[, c("DATE", "GVKEY", "fyear", "be", "LPERMNO", "LPERMCO")]


####################################################################################################
##  merging compustat_link and CRSP table based on PERMNO and fyear vs. jul-jun-year and PERMCO
####################################################################################################

setkey(compustat_link, fyear, LPERMNO, LPERMCO)
#CRSP[, fyear:=year(date)] #fiscal year and calander year treating same as in the paper
CRSP[, july_jun_year := year(YearMonth) - (month(YearMonth) < 7)-1] 
#july_jun year with a lag so Jul1970 to Jun 1971 is july_jun_year fyear 1969 this will help in getting lagged book equity from 
#compustat table directly
setkey(CRSP, july_jun_year, PERMNO, PERMCO)

final_DT <- merge(CRSP, compustat_link, by.x = c("PERMNO", "july_jun_year", "PERMCO"), by.y = c("LPERMNO", "fyear", "LPERMCO"), all.x = TRUE, allow.cartesian = TRUE)


####################################################################################################
##  Cleaning merged file (CRSP + Compustat_link)
##  issues - many to one gvkey vs PERMCO, many GVKEY at same date, many PERMCO at same date
####################################################################################################

### First, if LC not LC LINKTYPE, only keep LC
## Create columns that index pathologies
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := sum(LINKTYPE == 'LC'), by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match == T & LINKTYPE != 'LC')]

### Second, if P and not P LINKPRIM, only keep P
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := sum(LINKPRIM == 'P'), by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match == T & LINKPRIM != 'P')]

### Third, if 1 and not 1 LIID, only keep 1
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := sum(LIID == 1), by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match == T & LIID != 1)]

### Fourth, use the link that's current
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := sum(is.na(LINKENDDT)), by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match == T & !is.na(LINKENDDT))]

### Fifth, use the link that's been around the longest
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := NULL]

final_DT[is.na(LINKENDDT), LINKENDDT := as.Date('2017-12-31', '%Y%m%d')]

final_DT[, Date_diff := as.integer(LINKENDDT - LINKDT)]
setorder(final_DT, PERMCO, date, Date_diff)
final_DT[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match != T)]

### Sixth, use the GVKEY that has been around the longest
final_DT[, prob := .N > 1, by = .(PERMCO, date)]
final_DT[, Good_match := NULL]
setorder(final_DT, GVKEY, LINKDT)
final_DT[prob == T, start_Date := as.integer(LINKDT[1]), by = .(GVKEY)]
setorder(final_DT, GVKEY, LINKENDDT)
final_DT[prob == T, end_Date := as.integer(LINKENDDT[.N]), by = .(GVKEY)]
final_DT[, Date_diff := as.integer(end_Date - start_Date)]
setorder(final_DT, PERMCO, date, Date_diff)
final_DT[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
final_DT = final_DT[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
#### "This is stupid, but at least it is reproducible" - Chady
setorder(final_DT, PERMCO, date, GVKEY)
final_DT = unique(final_DT, by = c('PERMCO', 'date'))

# Remove if GVKEY is NA
final_DT <- na.omit(final_DT, cols = "GVKEY")

## data should be between LINKDT and LINKENDDT
final_DT[, Good_match := (date >= LINKDT & date <= LINKENDDT)]
final_DT = final_DT[Good_match == T]

### Clean up extra variables and final_DT check of match
if(nrow(unique(final_DT, by = c('GVKEY', 'date'))) != nrow(final_DT) | nrow(unique(final_DT, by = c('PERMCO', 'date'))) != nrow(final_DT)) {print('1. Monthly Firm-level returns.R: There is an issue with your merge between Monthly CRSP and the CRSP/Compustat linktable!')}



######################################################################################################
# taking relevant column for analysis
######################################################################################################

final_DT_bkp <- final_DT
final_DT = final_DT[, .(PERMNO, july_jun_year, PERMCO, date,YearMonth, SHRCD, EXCHCD, DLRET, PRC, RET, SHROUT, adjRet, Mkt_Cap, lag_Mkt_Cap, GVKEY, be)]
final_DT[, year:=year(YearMonth)]
final_DT[, MEtoBeUsed:=shift(lag_Mkt_Cap, 6), by=c("year", "PERMNO")]
#final_DT <- final_DT[date >= "1972-07-01"]

rm(COMPDT, Link_table, compustat_link, compustat_pension, CRSP, i)

######################################################################################################
### Size decile and return calculation
######################################################################################################

#check if july lag market return is avaliable.
#final_DT[, july_jun_year := year(YearMonth) + (month(YearMonth) >=7)]
final_DT[month(YearMonth)==7, tmp:=!is.na(lag_Mkt_Cap)]
final_DT[, flag:=sum(tmp, na.rm = TRUE), by=c("PERMNO", "july_jun_year")]

size_DT <- final_DT[flag==1]
final_DT[, c("tmp", "flag")] <- NULL

## decile for Size
size_DT[month(YearMonth)==7, JulDecile := as.integer(cut(lag_Mkt_Cap, quantile(lag_Mkt_Cap[EXCHCD==1], probs = 0:10/10, na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]
setorder(size_DT, PERMNO, YearMonth)

size_DT[, decile := sum(JulDecile, na.rm = TRUE), by = c("PERMNO", "july_jun_year")]

#ignore 0 in decile, it's coming due to summation fyear wise
size_DT[ , wt:= lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), by=c("YearMonth", "decile")]

size_DT[ , Size_Ret:= sum(wt*adjRet, na.rm = TRUE), by=c("YearMonth", "decile")]

size_Ret <- unique(size_DT[decile>0 & YearMonth>="Jan 1973", c("YearMonth", "decile", "Size_Ret")])



rm(size_DT)

######################################################################################################
###                  BTM  decile and return
######################################################################################################


final_DT[, BTM := be/MEtoBeUsed]
final_DT[month(YearMonth)==7, tmp := (!is.na(MEtoBeUsed))*(!is.na(be) & (be>0))*(!is.na(lag_Mkt_Cap))]


#final_DT[month(YearMonth)==7, tmp1:=tmp]

final_DT[, flag:=sum(tmp, na.rm = TRUE), by=c("PERMNO", "july_jun_year")]

## this filtered table will be used for HML as well in next section
BTM_DT <- final_DT[flag==1]
final_DT[, c("BTM", "tmp", "flag")] <- NULL

## decile for BE/ME
BTM_DT[month(YearMonth)==7, JulDecile := as.integer(cut(BTM[month(YearMonth)==7], quantile(BTM[EXCHCD==1], probs = 0:10/10, na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]
setorder(BTM_DT, PERMNO, YearMonth)

BTM_DT[, decile := sum(JulDecile, na.rm = TRUE), by = c("PERMNO", "july_jun_year")]

BTM_DT[ , wt:= lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), by=c("YearMonth", "decile")]

BTM_DT[ , BTM_Ret:= sum(wt*adjRet, na.rm = TRUE), by=c("YearMonth", "decile")]

#BTM_DT[decile>0, BTM_Ret:= sum(adjRet*(lag_Mkt_Cap/sum(lag_Mkt_Cap)), na.rm = TRUE), by=c("YearMonth", "decile")]

BTM_Ret <- unique(BTM_DT[decile>0 & YearMonth>="Jan 1973", c("YearMonth", "decile", "BTM_Ret")])



## Merging size_ret and BTM_ret
Portfolio_DT <- merge(size_Ret, BTM_Ret, by=c("YearMonth", "decile" ))
rm(size_Ret, BTM_Ret)


######################################################################################################
###                 Loading FF size, BEME, FF3 files
######################################################################################################

## reading the file from FF size and BE-ME
size_ff <- fread("./Portfolios_Formed_on_ME.CSV")
size_ff <- size_ff[, c(1, 11:20)]
colnames(size_ff) <- c("V1", 1:10)
size_ff[,  YearMonth:=as.yearmon(ymd(paste0(V1,"01")))]
size_ff$V1 <- NULL
size_ff <- size_ff[YearMonth >= "Jan 1973" & YearMonth <= "Dec 2017"]
size_ff <- melt(size_ff, id.vars = "YearMonth", variable.name = "decile", value.name = "Size_ret_FF")
size_ff[, decile:=as.numeric(decile)][, Size_ret_FF:=Size_ret_FF/100]

Portfolio_DT <- merge(Portfolio_DT, size_ff, by=c("YearMonth", "decile"))

beme_ff <- fread("./Portfolios_Formed_on_BE-ME.CSV")
beme_ff <- beme_ff[, c(1, 11:20)]
colnames(beme_ff) <- c("V1", 1:10)
beme_ff[,  YearMonth:=as.yearmon(ymd(paste0(V1,"01")))]
beme_ff$V1 <- NULL
beme_ff <- beme_ff[YearMonth >= "Jan 1973" & YearMonth <= "Dec 2017"]
beme_ff <- melt(beme_ff, id.vars = "YearMonth", variable.name = "decile", value.name = "BTM_ret_FF")
beme_ff[, decile:=as.numeric(decile)][, BTM_ret_FF:=BTM_ret_FF/100]

Portfolio_DT <- merge(Portfolio_DT, beme_ff, by=c("YearMonth", "decile"))

ff3 <- fread("./F-F_Research_Data_Factors.CSV")
ff3[,  YearMonth:=as.yearmon(ymd(paste0(V1,"01")))] 
ff3$V1 <- NULL
ff3 <- ff3[YearMonth >= "Jan 1973" & YearMonth <= "Dec 2017"]
ff3[, RF:=RF/100]

Portfolio_DT <- merge(Portfolio_DT, ff3[, c("RF", "YearMonth")], by=c("YearMonth"))


##############################################################################################
##Calculating SMB (two cuts S/B) and  HML return (GrowthMediumValue=LoMidHi, three cuts L/M/H)
##############################################################################################

##decile for SMB
BTM_DT$JulDecile <- NULL
BTM_DT <- BTM_DT[YearMonth >= "Jul 1972"]

BTM_DT[month(YearMonth)==7, JulDecile := as.integer(cut(lag_Mkt_Cap, quantile(c(-Inf, lag_Mkt_Cap[EXCHCD==1], Inf), probs = 0:2/2, na.rm = TRUE)), include.lowest = TRUE, include.highest = TRUE), by=.(YearMonth)]

BTM_DT[, smallbig := sum(JulDecile, na.rm = TRUE), by = c("PERMNO", "july_jun_year")]

## decile for HML
BTM_DT$JulDecile <- NULL
BTM_DT[month(YearMonth)==7, JulDecile := as.integer(cut(BTM, quantile( c(-Inf, BTM[EXCHCD==1], Inf), probs = c(0, 0.3, 0.7, 1), na.rm = TRUE)), include.lowest = TRUE), by=.(YearMonth)]


BTM_DT[, growthNeutralValue := sum(JulDecile, na.rm = TRUE), by = c("PERMNO", "july_jun_year")]

BTM_DT[, doubleSixSort:= paste0(smallbig, growthNeutralValue)]

BTM_DT[ , doublesortWt:= lag_Mkt_Cap/sum(lag_Mkt_Cap, na.rm = TRUE), by=c("YearMonth", "doubleSixSort")]
BTM_DT[ , doubleSortRet:= sum(doublesortWt*adjRet, na.rm = TRUE), by=c("YearMonth", "doubleSixSort")]

doubleSort <- unique(BTM_DT[YearMonth >="Jan 1973", c("YearMonth", "doubleSixSort", "doubleSortRet")])

doubleSort <- as.data.table(dcast(doubleSort, YearMonth ~ doubleSixSort ,value.var="doubleSortRet"))
doubleSort <- merge(doubleSort, ff3[, c("RF","SMB", "HML", "YearMonth")], by=c("YearMonth"))
colnames(doubleSort) <- c("YearMonth", "SL", "SM", "SH", "BL", "BM", "BH", "RF", "SMB_FF", "HML_FF")

doubleSort[, SMB_Ret:= (1/3)*(SL+SM+SH) - (1/3)*(BL+BM+BH)]
doubleSort[, HML_Ret:= (1/2)*(SH + BH) - (1/2)*(SL + BL)]

doubleSort[, SMB_FF:=SMB_FF/100]
doubleSort[, HML_FF:= HML_FF/100]

##############################################################################################
## Calculating stats for Ques 2 (ME Size return)
##############################################################################################

output_q2 <- matrix(nrow = 5, ncol = 11)
colnames(output_q2) <- c(paste0("Decile", 1:10), "LongShort(Decile1-Decile10 or Small-Big)")
rownames(output_q2) <- c("r-rf", "sigma", "SR", "skewness", "Correlation")
meanexcess <- Portfolio_DT[, 12*mean(Size_Ret-RF), by=c("decile")]
sd <- Portfolio_DT[, sqrt(12)*sd(Size_Ret), by=c("decile")]
SR <- meanexcess/sd
sk <- Portfolio_DT[, skewness(log(1+Size_Ret)), by=c("decile")] 
corr <- Portfolio_DT[, cor(Size_Ret, Size_ret_FF), by=c("decile")]

setorder(Portfolio_DT, YearMonth, decile)
longshortsize <- Portfolio_DT[decile==1 , Size_Ret ] - Portfolio_DT[decile==10, Size_Ret]
longshortsizeff <- Portfolio_DT[decile==1 , Size_ret_FF] - Portfolio_DT[decile==10, Size_ret_FF]


output_q2[1, ] <- c(unlist(meanexcess[, 2]), 12*mean(longshortsize))*100
output_q2[2, ] <- c(unlist(sd[, 2]), sqrt(12)*sd(longshortsize))*100
output_q2[3, ] <- c(unlist(SR[, 2]), 12*mean(longshortsize)/(sqrt(12)*sd(longshortsize)))
output_q2[4, ] <- c(unlist(sk[, 2]), skewness(longshortsize))
output_q2[5, ] <- c(unlist(corr[, 2]), cor(longshortsize, longshortsizeff ))

output_q2

##############################################################################################
## Calculating stats for Ques 3 (BE/ME decile return)
##############################################################################################

output_q3 <- matrix(nrow = 5, ncol = 11)
colnames(output_q3) <- c(paste0("Decile", 1:10), "long-short(Decile10-Decile1 or Value-Growth)")
rownames(output_q3) <- c("r-rf", "sigma", "SR", "skewness", "Correlation")
meanexcess <- Portfolio_DT[, 12*mean(BTM_Ret-RF), by=c("decile")]
sd <- Portfolio_DT[, sqrt(12)*sd(BTM_Ret), by=c("decile")]
SR <- meanexcess/sd
sk <- Portfolio_DT[, skewness(log(1+BTM_Ret)), by=c("decile")] 
corr <- Portfolio_DT[, cor(BTM_Ret, BTM_ret_FF), by=c("decile")]

setorder(Portfolio_DT, YearMonth, decile)
longshortbtm <- Portfolio_DT[decile==10 , BTM_Ret-RF ] - Portfolio_DT[decile==1, BTM_Ret-RF]
longshortbtmff <- Portfolio_DT[decile==10 , BTM_ret_FF-RF ] - Portfolio_DT[decile==1, BTM_ret_FF-RF]


output_q3[1, ] <- c(unlist(meanexcess[, 2]), 12*mean(longshortbtm))*100
output_q3[2, ] <- c(unlist(sd[, 2]), sqrt(12)*sd(longshortbtm))*100
output_q3[3, ] <- c(unlist(SR[, 2]), 12*mean(longshortbtm)/(sqrt(12)*sd(longshortbtm)))
output_q3[4, ] <- c(unlist(sk[, 2]), skewness(log(1+longshortbtm)))
output_q3[5, ] <- c(unlist(corr[, 2]), cor(longshortbtm, longshortbtmff ))

output_q3

##############################################################################################
## Calculating stats for Ques 4 (value and size anomaly worked in the past few years?)
##############################################################################################

#size anomaly
Portfolio_DT <- Portfolio_DT[, excessSizeRet:= Size_Ret-RF]
size_anomaly <- as.data.table(dcast(Portfolio_DT[, c( "YearMonth", "decile", "excessSizeRet")], YearMonth ~ decile, value.var = "excessSizeRet"))  
size_anomaly$smbExcess <- size_anomaly$`1`-size_anomaly$`10`

size_anomaly <- size_anomaly[YearMonth >= "Jan 2008", c("YearMonth", "smbExcess")]
size_anomaly[, cumSMBRet:=cumprod(1+smbExcess)] 

#value anamoly
Portfolio_DT <- Portfolio_DT[, excessBTMRet:= BTM_Ret-RF]
value_anomaly <- as.data.table(dcast(Portfolio_DT[, c( "YearMonth", "decile", "excessBTMRet")], YearMonth ~ decile, value.var = "excessBTMRet"))  
value_anomaly$hmlExcess <- value_anomaly$`1`-value_anomaly$`10`

value_anomaly <- value_anomaly[YearMonth >= "Jan 2008", c("YearMonth", "hmlExcess")]
value_anomaly[, cumHMLRet:=cumprod(1+hmlExcess)] 

size_value_anomaly <- merge(size_anomaly, value_anomaly, by=c("YearMonth"))
size_value_anomaly <- merge(size_value_anomaly, ff3[, c("YearMonth", "Mkt-RF")], by.x = "YearMonth", by.y = "YearMonth", all.x = TRUE)
size_value_anomaly[, cumMktRet:=cumprod(1+`Mkt-RF`/100)]
size_value_anomaly[, YearMonth:=as.integer(format(YearMonth, "%Y%m"))]

ggplot()+
  geom_line(data=size_value_anomaly,aes(x=size_value_anomaly$YearMonth,y= size_value_anomaly$cumSMBRet, colour="S-B Excess Ret"), size=1)+
  geom_line(data=size_value_anomaly,aes(x=size_value_anomaly$YearMonth,y= size_value_anomaly$cumHMLRet, colour="H-L Excess Ret"), size=1)+
  geom_line(data=size_value_anomaly,aes(x=size_value_anomaly$YearMonth,y= size_value_anomaly$cumMktRet, colour="Market Excess Ret"), size=1)+
  geom_hline(yintercept=1, linetype="dotted", size=2)+
  scale_x_yearmon(size_value_anomaly$YearMonth, format = "%b %Y", n = 5)+
  labs(x = "Year Month",y="Cumulative Excess Long-Short Returns",colour="Labels",title="Size/Value Anamoly \n S-B, H-L decile cumulative returns")+
  theme(plot.title = element_text(color="Black", size=16, face="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))



##############################################################################################
## Calculating stats for Ques 5 (HML and SMB)
##############################################################################################

output_q5 <- matrix(nrow = 5, ncol = 2)
colnames(output_q5) <- c("HML", "SMB")
rownames(output_q5) <- c("r-rf", "sigma", "SR", "skewness", "Correlation")

meanexcess <- c( 12*mean(doubleSort$HML_Ret-doubleSort$RF), 12*mean(doubleSort$SMB_Ret-doubleSort$RF))
sd <- c(sd(doubleSort$HML_Ret), sd(doubleSort$SMB_Ret))
SR <- meanexcess/sd
sk_hml <- skewness(doubleSort$HML_Ret)
sk_smb <- skewness(doubleSort$SMB_Ret)

corr <- c(cor(doubleSort$HML_Ret, doubleSort$HML_FF), cor(doubleSort$SMB_Ret, doubleSort$SMB_FF))

output_q5[1, ] <- meanexcess*100
output_q5[2, ] <- sd*100
output_q5[3, ] <- SR
output_q5[4, ] <- c(sk_hml, sk_smb)
output_q5[5, ] <- corr

output_q5

## Have the factors been consistent across time? empirical evidence.
## SMB factor
doubleSort[, SMBExcess:=SMB_Ret-RF]
doubleSort <- merge(doubleSort, ff3[, c("YearMonth", "Mkt-RF")], by.x = "YearMonth", by.y = "YearMonth", all.x = TRUE)
doubleSort$`Mkt-RF` <- doubleSort$`Mkt-RF`/100
doubleSort <- doubleSort[YearMonth >= "Jan 2008"]
doubleSort[, `:=`(cumMktRet = cumprod(1+`Mkt-RF`), cumSMBRet=cumprod(1+SMBExcess))]


## HML factor
doubleSort[, HMLExcess:=HML_Ret-RF]
doubleSort[, `:=`(cumHMLRet=cumprod(1+HMLExcess))]

ggplot()+
  geom_line(data=doubleSort,aes(x=doubleSort$YearMonth,y= doubleSort$cumHMLRet, colour="HML Excess Ret"), size=1)+
  geom_line(data=doubleSort,aes(x=doubleSort$YearMonth,y= doubleSort$cumSMBRet, colour="SMB Excess Ret"), size=1)+
  geom_line(data=doubleSort,aes(x=doubleSort$YearMonth,y= doubleSort$cumMktRet, colour="Market Excess Ret"), size=1)+
  geom_hline(yintercept=1, linetype="dotted", size=2)+
  scale_x_yearmon(doubleSort$YearMonth, format = "%b %Y", n = 5)+
  labs(x = "Year Month",y="Cumulative Excess factor Return",colour="Labels",title="HML/SMB factors (Cumulative Return)")+
  theme(plot.title = element_text(color="Black", size=16, face="bold.italic", hjust=0.5),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))




















