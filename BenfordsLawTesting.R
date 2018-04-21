#Benford's law

library(BenfordTests)
require(RODBC)
library(dplyr)


ch <- odbcConnect("DBM_SANDBOX", believeNRows=FALSE)
nzdata <- sqlQuery(ch, paste("SELECT ST.STORE_NUMBER,
 	   SH.TERMINAL,
	   ASS.CASHIER_NUMBER,
	   ASS.ASSOCIATE_NAME,
	   SH.TRANSACTION_CODE,
	   SUM(SD.SALES_PRICE) AS SALES
FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
JOIN EDW_SPOKE..DIM_ASSOCIATE ASS ON SH.ASSOCIATE_MEMBER_KEY = ASS.MEMBER_KEY
JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
JOIN EDW_SPOKE..DIM_DATE DT ON SD.SALE_DATE_KEY = DT.DATE_KEY
JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
WHERE ST.CHANNEL_GROUP = 'RETAIL'
	AND DT.DATE_VALUE BETWEEN CURRENT_DATE - 14 AND CURRENT_DATE
  AND ST.STORE_NUMBER IN ('001')
  AND SD.SALES_PRICE >= 1
GROUP BY ST.STORE_NUMBER, SH.TERMINAL, ASS.CASHIER_NUMBER, ASS.ASSOCIATE_NAME, SH.TRANSACTION_CODE
ORDER BY ST.STORE_NUMBER, SH.TERMINAL, ASS.CASHIER_NUMBER, ASS.ASSOCIATE_NAME, SH.TRANSACTION_CODE"))

X <- nzdata[which(nzdata$SALES > 0),  ]

stores <- as.array(unique(X$STORE_NUMBER))

for (i in stores) {
  if (i == 1) {
    final <- data.frame(store = integer(),
                        cashier_num = character(),
                        cashier_name = character(),
                        samplesize = integer(),
                        usq = double(),
                        pval = double())
  }
  X2 <- X[which(X$STORE_NUMBER == i), ]
  cashiers <- as.array(unique(X2$CASHIER_NUMBER))
  for (j in cashiers){
    X3 <- X2[which(X2$CASHIER_NUMBER == j), ]
    if (length(X3$SALES) > 100){
      op <- usq.benftest(X3$SALES, digits = 1)
      X4 <- data.frame(store = i,
                       cashier_num = j,
                       cashier_name = unique(X3$ASSOCIATE_NAME),
                       samplesize = length(X3$SALES),
                       usq = op$statistic,
                       pval = op$p.value)
      final <- rbind(final, X4)
    }
    
  }
}


#filtered <- final[which(final$pval < 0.05 & final$samplesize >= 500), ]
filtered <- final[which(final$pval < 0.05), ]



validation <- sqlQuery(ch, paste("SELECT ST.STORE_NUMBER,
     SH.INBOUND_INTERACTION_KEY,
     SUM(SD.SALES_PRICE) AS SALES
FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
JOIN EDW_SPOKE..DIM_ASSOCIATE ASS ON SH.ASSOCIATE_MEMBER_KEY = ASS.MEMBER_KEY
JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
JOIN EDW_SPOKE..DIM_DATE DT ON SD.SALE_DATE_KEY = DT.DATE_KEY
JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
WHERE ST.STORE_NUMBER = '001'
	AND DT.DATE_VALUE BETWEEN CURRENT_DATE - 365 AND CURRENT_DATE
  AND SD.SALES_PRICE >= 1
  AND SD.RETURN_FLAG <> 'Y'
  AND SD.ACTUAL_SALE_FLAG = 'Y'
GROUP BY ST.STORE_NUMBER, SH.INBOUND_INTERACTION_KEY
ORDER BY ST.STORE_NUMBER, SH.INBOUND_INTERACTION_KEY
                "))

usq.benftest(validation$SALES, digits=1)
signifd.analysis(validation$SALES, digits=1)



