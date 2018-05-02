require(nza)
require(nzr)
require(xgboost)
require(caTools)
require(caret)
require(pROC)



nzConnectDSN('DBM_SANDBOX')

nzdata = nzQuery("SELECT HIST.*,
	   NVL(RESP.TARGET_RESPONSE, 0) AS TARGET_RESPSONSE
                 FROM (
                 SELECT DISTINCT DETAIL.LAST_NAME,
                 DETAIL.ADDRESS_LINE_1,
                 DETAIL.CITY_NAME,
                 DETAIL.STATE_CODE,
                 DETAIL.ZIP,
                 1 AS TARGET_RESPONSE
                 FROM (
                 SELECT IND.LAST_NAME,
                 ADDR.ADDRESS_LINE_1,
                 ADDR.CITY_NAME,
                 ADDR.STATE_CODE,
                 ADDR.ZIP,
                 IND.REWARDS_ACTIVE_CODE,
                 IND.VISA_STATUS,
                 DT.DATE_VALUE,
                 SH.INBOUND_INTERACTION_KEY,
                 ST.CHANNEL_GROUP,
                 PD.DEPARTMENT_MEMBER_NUMBER,
                 PD.SUB_DEPARTMENT_MEMBER_NUMBER,
                 SUM(SD.SALE_QUANTITY) AS UNITS,
                 SUM(SD.SALES_PRICE) AS SALES
                 FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
                 JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
                 JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_DATE DT ON SD.SALE_DATE_KEY = DT.DATE_KEY
                 JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_PERSONA PERS ON SH.CUSTOMER_MEMBER_KEY = PERS.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_INDIVIDUAL IND ON PERS.INDIVIDUAL_MEMBER_KEY = IND.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_ADDRESS ADDR ON IND.ADDRESS_KEY = ADDR.MEMBER_KEY
                 WHERE ST.CHANNEL_GROUP IN ('DIRECT','RETAIL')
                 AND DT.DATE_VALUE- 1 >= '2017-10-13'::DATE
                 AND SD.ACTUAL_SALE_FLAG = 'Y'
                 AND SD.RETURN_FLAG <> 'Y'
                 AND PD.DEPARTMENT_MEMBER_NUMBER NOT IN (875,999)
                 GROUP BY IND.LAST_NAME, ADDR.ADDRESS_LINE_1, ADDR.CITY_NAME, ADDR.STATE_CODE, ADDR.ZIP, IND.REWARDS_ACTIVE_CODE, IND.VISA_STATUS, DT.DATE_VALUE, 
                 SH.INBOUND_INTERACTION_KEY, ST.CHANNEL_GROUP, PD.DEPARTMENT_MEMBER_NUMBER, PD.SUB_DEPARTMENT_MEMBER_NUMBER
                 ORDER BY IND.LAST_NAME, ADDR.ADDRESS_LINE_1, ADDR.CITY_NAME, ADDR.STATE_CODE, ADDR.ZIP
                 ) AS DETAIL
                 ORDER BY DETAIL.LAST_NAME, DETAIL.ADDRESS_LINE_1, DETAIL.CITY_NAME, DETAIL.STATE_CODE, DETAIL.ZIP
                 ) RESP
                 RIGHT JOIN (
                 SELECT DETAIL.LAST_NAME,
                 DETAIL.ADDRESS_LINE_1,
                 DETAIL.CITY_NAME,
                 DETAIL.STATE_CODE,
                 DETAIL.ZIP,
                 MAX(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-(365) AND '2017-10-13'::DATE THEN 1 ELSE 0 END) AS PURCH_6MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-(365) AND '2017-10-13'::DATE THEN DETAIL.SALES ELSE 0 END) AS SLS_6MO,
                 MAX(CASE WHEN DETAIL.REWARDS_ACTIVE_CODE IN ('A','Y') THEN 'Y' ELSE 'N' END) AS REWARDS,
                 MAX(CASE WHEN DETAIL.VISA_STATUS = 'Y' THEN 'Y' ELSE 'N' END) AS VISA,
                 MIN(DETAIL.DATE_VALUE) AS FIRST_DATE,
                 MAX(DETAIL.DATE_VALUE) AS LAST_DATE,
                 MAX(DETAIL.DATE_VALUE)-MIN(DETAIL.DATE_VALUE) AS TIME_AS_CUSTOMER,
                 COUNT(DISTINCT DETAIL.INBOUND_INTERACTION_KEY) AS ORDERS_LT,
                 (MAX(DETAIL.DATE_VALUE) - MIN(DETAIL.DATE_VALUE)) / NULLIF(COUNT(DISTINCT DETAIL.INBOUND_INTERACTION_KEY), 0)::DOUBLE AS AVG_DAYS_BTW_PURCH,
                 NVL(SUM(DETAIL.SALES)/NULLIF(COUNT(DISTINCT DETAIL.INBOUND_INTERACTION_KEY),0)::DOUBLE,0) AS AVG_TICKET_LT,
                 NVL(SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE THEN DETAIL.SALES ELSE 0 END) /
                 NULLIF(COUNT(DISTINCT CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE THEN DETAIL.INBOUND_INTERACTION_KEY ELSE NULL END),0),0) AS AVG_TICKET_24MO, 
                 NVL(SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE THEN DETAIL.SALES ELSE 0 END) /
                 NULLIF(COUNT(DISTINCT CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE THEN DETAIL.INBOUND_INTERACTION_KEY ELSE NULL END),0),0) AS AVG_TICKET_48MO,
                 SUM(CASE WHEN DETAIL.CHANNEL_GROUP = 'RETAIL' THEN DETAIL.SALES ELSE 0 END)/ NULLIF(SUM(DETAIL.SALES),0) AS PERCENT_RETAIL_SALES,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE THEN DETAIL.SALES ELSE 0 END) AS SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-365 AND '2017-10-13'::DATE THEN DETAIL.SALES ELSE 0 END) AS SALES_12MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER IN (100, 151, 200, 400, 450) THEN DETAIL.SALES ELSE 0 END) AS CORE_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 100 THEN DETAIL.SALES ELSE 0 END) AS D100_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 151 THEN DETAIL.SALES ELSE 0 END) AS D151_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 175 THEN DETAIL.SALES ELSE 0 END) AS D175_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 200 THEN DETAIL.SALES ELSE 0 END) AS D200_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 300 THEN DETAIL.SALES ELSE 0 END) AS D300_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 350 THEN DETAIL.SALES ELSE 0 END) AS D350_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 400 THEN DETAIL.SALES ELSE 0 END) AS D400_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 450 THEN DETAIL.SALES ELSE 0 END) AS D450_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 475 THEN DETAIL.SALES ELSE 0 END) AS D475_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 500 THEN DETAIL.SALES ELSE 0 END) AS D500_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 600 THEN DETAIL.SALES ELSE 0 END) AS D600_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 650 THEN DETAIL.SALES ELSE 0 END) AS D650_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-729 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 675 THEN DETAIL.SALES ELSE 0 END) AS D675_SALES_24MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER IN (100, 151, 200, 400, 450) THEN DETAIL.SALES ELSE 0 END) AS CORE_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 100 THEN DETAIL.SALES ELSE 0 END) AS D100_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 151 THEN DETAIL.SALES ELSE 0 END) AS D151_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 175 THEN DETAIL.SALES ELSE 0 END) AS D175_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 200 THEN DETAIL.SALES ELSE 0 END) AS D200_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 300 THEN DETAIL.SALES ELSE 0 END) AS D300_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 350 THEN DETAIL.SALES ELSE 0 END) AS D350_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 400 THEN DETAIL.SALES ELSE 0 END) AS D400_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 450 THEN DETAIL.SALES ELSE 0 END) AS D450_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 475 THEN DETAIL.SALES ELSE 0 END) AS D475_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 500 THEN DETAIL.SALES ELSE 0 END) AS D500_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 600 THEN DETAIL.SALES ELSE 0 END) AS D600_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 650 THEN DETAIL.SALES ELSE 0 END) AS D650_SALES_48MO,
                 SUM(CASE WHEN DETAIL.DATE_VALUE BETWEEN '2017-10-13'::DATE-1458 AND '2017-10-13'::DATE AND DETAIL.DEPARTMENT_MEMBER_NUMBER = 675 THEN DETAIL.SALES ELSE 0 END) AS D675_SALES_48MO
                 FROM (
                 SELECT IND.LAST_NAME,
                 ADDR.ADDRESS_LINE_1,
                 ADDR.CITY_NAME,
                 ADDR.STATE_CODE,
                 ADDR.ZIP,
                 IND.REWARDS_ACTIVE_CODE,
                 IND.VISA_STATUS,
                 DT.DATE_VALUE,
                 SH.INBOUND_INTERACTION_KEY,
                 ST.CHANNEL_GROUP,
                 PD.DEPARTMENT_MEMBER_NUMBER,
                 PD.SUB_DEPARTMENT_MEMBER_NUMBER,
                 SUM(SD.SALE_QUANTITY) AS UNITS,
                 SUM(SD.SALES_PRICE) AS SALES
                 FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
                 JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
                 JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_DATE DT ON SD.SALE_DATE_KEY = DT.DATE_KEY
                 JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_PERSONA PERS ON SH.CUSTOMER_MEMBER_KEY = PERS.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_INDIVIDUAL IND ON PERS.INDIVIDUAL_MEMBER_KEY = IND.MEMBER_KEY
                 JOIN EDW_SPOKE..DIM_ADDRESS ADDR ON IND.ADDRESS_KEY = ADDR.MEMBER_KEY
                 WHERE ST.CHANNEL_GROUP IN ('DIRECT','RETAIL')
                 AND DT.DATE_VALUE BETWEEN '2017-10-13'::DATE - (365*5) AND '2017-10-13'::DATE
                 AND SD.ACTUAL_SALE_FLAG = 'Y'
                 AND SD.RETURN_FLAG <> 'Y'
                 AND PD.DEPARTMENT_MEMBER_NUMBER NOT IN (875,999)
                 GROUP BY IND.LAST_NAME, ADDR.ADDRESS_LINE_1, ADDR.CITY_NAME, ADDR.STATE_CODE, ADDR.ZIP, IND.REWARDS_ACTIVE_CODE, IND.VISA_STATUS, DT.DATE_VALUE, 
                 SH.INBOUND_INTERACTION_KEY, ST.CHANNEL_GROUP, PD.DEPARTMENT_MEMBER_NUMBER, PD.SUB_DEPARTMENT_MEMBER_NUMBER
                 ORDER BY IND.LAST_NAME, ADDR.ADDRESS_LINE_1, ADDR.CITY_NAME, ADDR.STATE_CODE, ADDR.ZIP, DT.DATE_VALUE, SH.INBOUND_INTERACTION_KEY,
                 PD.DEPARTMENT_MEMBER_NUMBER, PD.SUB_DEPARTMENT_MEMBER_NUMBER
                 ) AS DETAIL
                 GROUP BY DETAIL.LAST_NAME, DETAIL.ADDRESS_LINE_1, DETAIL.CITY_NAME, DETAIL.STATE_CODE, DETAIL.ZIP
                 ) HIST ON RESP.LAST_NAME = HIST.LAST_NAME
                 AND RESP.ADDRESS_LINE_1 = HIST.ADDRESS_LINE_1
                 AND RESP.CITY_NAME = HIST.CITY_NAME
                 AND RESP.STATE_CODE = HIST.STATE_CODE
                 AND RESP.ZIP = HIST.ZIP
                 WHERE HIST.AVG_TICKET_48MO > 0
                 ORDER BY RANDOM()
                 LIMIT 100000")



nzDisconnect()

modelData <- nzdata
nzdata <- NULL

#modelData <- CHURNDATA
#CHURNDATA <- NULL

modelData$LAST_NAME <- NULL
modelData$ADDRESS_LINE_1 <- NULL
modelData$CITY_NAME <- NULL
modelData$STATE_CODE <- NULL
modelData$ZIP <- NULL


summary(modelData)

modelData$PURCH_6MO <- factor(modelData$PURCH_6MO, levels=c(0,1))
modelData$SLS_6MO <- as.numeric(modelData$SLS_6MO)
modelData$REWARDS <- factor( ifelse(modelData$REWARDS=='Y',1,0), levels=c(1,0) )
modelData$VISA <- factor(ifelse(modelData$VISA=='Y', 1,0), levels=c(1,0))
modelData$FIRST_DATE <- as.Date(modelData$FIRST_DATE)
modelData$LAST_DATE <- as.Date(modelData$LAST_DATE)
modelData$ORDERS_LT <- as.numeric(modelData$ORDERS_LT)
modelData$AVG_DAYS_BTW_PURCH <- as.numeric(modelData$AVG_DAYS_BTW_PURCH)
modelData$TIME_AS_CUSTOMER <- as.numeric(modelData$TIME_AS_CUSTOMER)
modelData$AVG_TICKET_24MO <- as.numeric(modelData$AVG_TICKET_24MO)
modelData$AVG_TICKET_48MO <- as.numeric(modelData$AVG_TICKET_48MO)
modelData$AVG_TICKET_LT <- as.numeric(modelData$AVG_TICKET_LT)
modelData$PERCENT_RETAIL_SALES <- as.numeric(modelData$PERCENT_RETAIL_SALES)
modelData$SALES_24MO <- as.numeric(modelData$SALES_24MO)
modelData$CORE_SALES_24MO <- as.numeric(modelData$SALES_24MO)
modelData$CORE_SALES_48MO <- as.numeric(modelData$CORE_SALES_48MO)
modelData$SALES_12MO <- - as.numeric(modelData$SALES_12MO)

modelData$D100_SALES_24MO <- as.numeric(modelData$D100_SALES_24MO)
modelData$D151_SALES_24MO <- as.numeric(modelData$D151_SALES_24MO)
modelData$D175_SALES_24MO <- as.numeric(modelData$D175_SALES_24MO)
modelData$D200_SALES_24MO <- as.numeric(modelData$D200_SALES_24MO)
modelData$D300_SALES_24MO <- as.numeric(modelData$D300_SALES_24MO)
modelData$D350_SALES_24MO <- as.numeric(modelData$D350_SALES_24MO)
modelData$D400_SALES_24MO <- as.numeric(modelData$D400_SALES_24MO)
modelData$D450_SALES_24MO <- as.numeric(modelData$D450_SALES_24MO)
modelData$D475_SALES_24MO <- as.numeric(modelData$D475_SALES_24MO)
modelData$D500_SALES_24MO <- as.numeric(modelData$D500_SALES_24MO)
modelData$D600_SALES_24MO <- as.numeric(modelData$D600_SALES_24MO)
modelData$D650_SALES_24MO <- as.numeric(modelData$D650_SALES_24MO)
modelData$D675_SALES_24MO <- as.numeric(modelData$D675_SALES_24MO)

modelData$D100_SALES_48MO <- as.numeric(modelData$D100_SALES_48MO)
modelData$D151_SALES_48MO <- as.numeric(modelData$D151_SALES_48MO)
modelData$D175_SALES_48MO <- as.numeric(modelData$D175_SALES_48MO)
modelData$D200_SALES_48MO <- as.numeric(modelData$D200_SALES_48MO)
modelData$D300_SALES_48MO <- as.numeric(modelData$D300_SALES_48MO)
modelData$D350_SALES_48MO <- as.numeric(modelData$D350_SALES_48MO)
modelData$D400_SALES_48MO <- as.numeric(modelData$D400_SALES_48MO)
modelData$D450_SALES_48MO <- as.numeric(modelData$D450_SALES_48MO)
modelData$D475_SALES_48MO <- as.numeric(modelData$D475_SALES_48MO)
modelData$D500_SALES_48MO <- as.numeric(modelData$D500_SALES_48MO)
modelData$D600_SALES_48MO <- as.numeric(modelData$D600_SALES_48MO)
modelData$D650_SALES_48MO <- as.numeric(modelData$D650_SALES_48MO)
modelData$D675_SALES_48MO <- as.numeric(modelData$D675_SALES_48MO)
modelData$TARGET_RESPSONSE <- factor(modelData$TARGET_RESPSONSE, levels=c(0,1))

summary(modelData)

table(modelData$TARGET_RESPSONSE)
table(modelData$PURCH_6MO)

noMissingTarget <- modelData[!is.na(modelData$TARGET_RESPSONSE), ]
noMissingTarget <- NULL

columnList <- as.list(colnames(noMissingTarget))

vars <- c('PURCH_6MO','REWARDS', 'VISA', 'TIME_AS_CUSTOMER',
           'ORDERS_LT', 'AVG_DAYS_BTW_PURCH', 'AVG_TICKET_LT', 'AVG_TICKET_24MO',
           'PERCENT_RETAIL_SALES', 'SALES_12MO', 'CORE_SALES_24MO',
          'D100_SALES_24MO', 'D151_SALES_24MO', 'D175_SALES_24MO', 'D200_SALES_24MO', 'D300_SALES_24MO',
          'D350_SALES_24MO', 'D400_SALES_24MO', 'D450_SALES_24MO', 'D475_SALES_24MO', 'D500_SALES_24MO',
          'D600_SALES_24MO', 'D650_SALES_24MO', 'D675_SALES_24MO',  'TARGET_RESPSONSE')

vars.x.num <- c('ORDERS_LT', 'AVG_DAYS_BTW_PURCH', 'AVG_TICKET_LT', 'AVG_TICKET_24MO', 'TIME_AS_CUSTOMER',
                'PERCENT_RETAIL_SALES', 'SALES_12MO', 'CORE_SALES_24MO',
                'D100_SALES_24MO', 'D151_SALES_24MO', 'D175_SALES_24MO', 'D200_SALES_24MO', 'D300_SALES_24MO',
                'D350_SALES_24MO', 'D400_SALES_24MO', 'D450_SALES_24MO', 'D475_SALES_24MO', 'D500_SALES_24MO',
                'D600_SALES_24MO', 'D650_SALES_24MO', 'D675_SALES_24MO')
vars.x.nonum <- c('PURCH_6MO','REWARDS','VISA')

vars.y <- c('TARGET_RESPSONSE')

modelData2 <- modelData[ , vars]


var_diagnostics <- function(data, xvars1, xvars2){
  for(i in xvars1){
    print(paste(i, 'contains', sum(is.na(data[,i])), 'empty values.'))
  } #end inner for loop
  
  for(j in xvars2){
    print(j)
    print(table(data[,j]))
  }
  
  for(p in xvars1){
    for(q in xvars1){
      cc <- cor(data[,p], data[,q], use='pairwise.complete.obs', method=c('pearson'))
      if(cc > .8 & p != q){
        print(paste(p, ' - ', q, ' = ', cc))
      }
      
    }
  }
  
} #end function

var_diagnostics(modelData2, vars.x.num, vars.x.nonum)


scaled = scale(modelData2[, vars.x.num], center = TRUE)


mahal <- mahalanobis(scaled[ , vars.x.num],
                     colMeans(scaled[ , vars.x.num], na.rm = TRUE),
                     cov(scaled[ , vars.x.num], 
                         use = "pairwise.complete.obs"),
                     tol=1e-20)

cutoff <- qchisq(1-.001, length(vars.x.num))

summary(mahal < cutoff)

modelData3 <- cbind(as.data.frame(scaled), 
                    as.data.frame(modelData2[, vars.x.nonum]))
modelData3$TARGET_RESPONSE <- modelData2[, vars.y]

nooutliers <- as.data.frame(subset(modelData3, mahal < cutoff))




train <- nooutliers[1:50000,] 
test <- nooutliers[50001:93667,]


form <- 'TARGET_RESPONSE ~ ORDERS_LT + AVG_DAYS_BTW_PURCH + AVG_TICKET_LT +
         TIME_AS_CUSTOMER + SALES_12MO +
        CORE_SALES_24MO + D200_SALES_24MO + D350_SALES_24MO + D400_SALES_24MO +
        D450_SALES_24MO + PURCH_6MO + REWARDS + VISA'
logmod <- glm(formula = form,
              family = binomial(link='logit'),
              data=train)

save(logmod, file = "C:\\Users\\pairwin\\Documents\\GitHub\\RAnalyses\\CHURN_LOGISTIC.rda")


summary(logmod)
anova(logmod, test='Chisq')
#plot(logmod)

target <-c('TARGET_RESPONSE')
finalvars <- c('ORDERS_LT','AVG_DAYS_BTW_PURCH','AVG_TICKET_LT', 'TIME_AS_CUSTOMER',
'SALES_12MO','CORE_SALES_24MO', 'D200_SALES_24MO', 'D350_SALES_24MO',
'D400_SALES_24MO','D450_SALES_24MO','PURCH_6MO','REWARDS', 'VISA')

train$PURCH_6MO <- as.numeric(as.character(train$PURCH_6MO))
train$REWARDS <- as.numeric(as.character(train$REWARDS))
train$VISA <- as.numeric(as.character(train$VISA))
train$TARGET_RESPONSE <- as.numeric(as.character(train$TARGET_RESPONSE))

test$PURCH_6MO <- as.numeric(as.character(test$PURCH_6MO))
test$REWARDS <- as.numeric(as.character(test$REWARDS))
test$VISA <- as.numeric(as.character(test$VISA))
test$TARGET_RESPONSE <- as.numeric(as.character(test$TARGET_RESPONSE))

train.data <- as.matrix(train[,finalvars])
train.label <- as.array(train[,target])

test.data <- as.matrix(test[,finalvars])
test.label <- as.array(test[,target])

#----------------Advanced features --------------
# to use advanced features, we need to put data in xgb.DMatrix
dtrain <- xgb.DMatrix(data = train.data, label=train.label)
dtest <- xgb.DMatrix(data = test.data, label=test.label)
#---------------Using watchlist----------------
# watchlist is a list of xgb.DMatrix, each of them is tagged with name
watchlist <- list(train=dtrain, test=dtest)
# to train with watchlist, use xgb.train, which contains more advanced features
# watchlist allows us to monitor the evaluation result on all data in the list 
print("Train xgboost using xgb.train with watchlist")
bst <- xgb.train(data=dtrain, 
                 max_depth=length(finalvars), 
                 eta=0.3, 
                 nrounds=53, 
                 subsample = 0.8, 
                 min_child_weight = 5,
                 colsample_bytree = .5, 
                 watchlist=watchlist, 
                 eval_metric = 'error',
                 nthread = 8, 
                 objective = "binary:logistic", 
                 early_stopping_rounds = 53)
# Finally, you can check which features are the most important.
print("Most important features (look at column Gain):")
imp_matrix <- xgb.importance(feature_names = colnames(train.data), model = bst)
print(imp_matrix)

# Feature importance bar plot by gain
print("Feature importance Plot : ")
print(xgb.plot.importance(importance_matrix = imp_matrix))

label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

predfactor <- factor(ifelse(pred >.5, 1,0), levels=c(1,0))

confusionMatrix(predfactor, factor(test.label, levels=c(1,0)))

rc <- roc(test.label, pred, plot=TRUE, smooth=TRUE)


#-------------------save and load models-------------------------
# save model to binary local file
xgb.save(bst, 'C:\\Users\\pairwin\\Documents\\GitHub\\RAnalyses\\xgboost_churn.model')
# load binary model to R
bst2 <- xgb.load('C:\\Users\\pairwin\\Documents\\GitHub\\RAnalyses\\xgboost_churn.model')


