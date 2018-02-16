library(RODBC)
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)
library(DomoR)
library(hts)
library(stringr)
#library(splitstackshape)

#DomoR::init('basspro.domo.com', '9602905555e390fe466e9de1940bf9a4cf8e734e5678b1ef')

options(digits=9)

ch <- odbcConnect("DBM_SANDBOX", believeNRows=FALSE)
querytext <- paste("SELECT DT.FISCAL_YEAR_PERIOD,
                           ST.STORE_NUMBER,
                           PD.SUB_DEPARTMENT_DISPLAY_NUMBER,
	                         SUM(SD.SALE_QUANTITY) AS UNITS
                   FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
                   JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
                   LEFT JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
                   JOIN EDW_SPOKE..DIM_DATE DT ON SH.TRANSACTION_DATE_KEY = DT.DATE_KEY
                   LEFT JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
                   WHERE (ST.CHANNEL_GROUP = 'RETAIL' OR ST.CHANNEL_NAME = 'INTERNET')
                   AND SD.ACTUAL_SALE_FLAG = 'Y'
                   AND DT.FISCAL_YEAR in (2013,2014,2015,2016,2017)
                   AND SD.RETURN_FLAG <> 'Y'
                   AND PD.DEPARTMENT_MEMBER_NUMBER NOT IN (875,999)
                   AND PD.SKU_DISPOSITION_CODE IN ('A','N')
                   GROUP BY DT.FISCAL_YEAR_PERIOD, ST.STORE_NUMBER, PD.SUB_DEPARTMENT_DISPLAY_NUMBER
                   ORDER BY DT.FISCAL_YEAR_PERIOD, ST.STORE_NUMBER, PD.SUB_DEPARTMENT_DISPLAY_NUMBER")
res <- sqlQuery(ch, querytext)
res$STORE_NUMBER <- str_pad(res$STORE_NUMBER, width=2, side='left', pad='0')
res$FISCAL_YEAR_PERIOD <- as.character(res$FISCAL_YEAR_PERIOD)
#res$SUB_DEPARTMENT_DISPLAY_NUMBER <- str_pad(res$SUB_DEPARTMENT_DISPLAY_NUMBER, width=5, side='left', pad='_0')
res$STORE_DEPT <- paste(res$STORE_NUMBER, res$SUB_DEPARTMENT_DISPLAY_NUMBER, sep='')



storeNums <- unique(res$STORE_NUMBER)
depts <- as.character(unique(res$SUB_DEPARTMENT_DISPLAY_NUMBER))

dt <- res
dt$SUB_DEPARTMENT_DISPLAY_NUMBER <- NULL
dt$STORE_NUMBER <- NULL

dt <- dt %>% select(STORE_DEPT, FISCAL_YEAR_PERIOD, UNITS)

dt2 <- arrange(dt, STORE_DEPT, FISCAL_YEAR_PERIOD)

library(tidyr)

widedt <- spread(data=dt,
                 key=STORE_DEPT,
                 value = UNITS)
widedt$FISCAL_YEAR_PERIOD <- NULL


blnames <- unique(dt2$STORE_DEPT)


myts <- ts(widedt, frequency=12)

colnames(myts) <- blnames

# gps <- rbind(
#   sort(rep(storeNums,each=160)), 
#   rep(depts, 84))               

gps <- rbind(
  substr(res$STORE_DEPT, 1, 2),
  substr(res$STORE_DEPT, 3, 11)
)


gmyts <- gts(myts, groups=gps)


fc <- forecast(gmyts, h=12)

fc$bts

test <- fc$bts

DomoR::init('basspro.domo.com', '9602905555e390fe466e9de1940bf9a4cf8e734e5678b1ef')

#create(final, 'R - Class Forecast - PI', 'Updated Forecasts for Units sold by Product Class')

# 3ccf75d8-b302-4126-92fa-67b0b1494f8d
replace_ds('3ccf75d8-b302-4126-92fa-67b0b1494f8d', final)
