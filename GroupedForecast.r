library(RODBC)
library(forecast)
library(ggplot2)
library(dplyr)
library(zoo)
library(DomoR)
library(hts)
library(stringr)


options(digits=9)

ch <- odbcConnect("DBM_SANDBOX", believeNRows=FALSE)
querytext <- paste("SELECT DT.FISCAL_YEAR_PERIOD,
                    ST.STORE_NUMBER,
                    PD.DEPARTMENT_MEMBER_NUMBER,
                    PD.DEPARTMENT_NAME,
                    PD.SUB_DEPARTMENT_MEMBER_NUMBER,
                    PD.SUB_DEPARTMENT_NAME,
                    PD.CLASS_MEMBER_NUMBER,
                    PD.CLASS_NAME,
                    PD.SUB_CLASS_MEMBER_NUMBER,
                    PD.SUB_CLASS_NAME,
                   SUM(SD.SALE_QUANTITY) AS UNITS
                   FROM EDW_SPOKE..FACT_BPS_SALES_HEADER SH
                   JOIN EDW_SPOKE..FACT_BPS_SALES_DETAIL SD ON SH.INBOUND_INTERACTION_KEY = SD.SH_INBOUND_INTERACTION_KEY
                   LEFT JOIN EDW_SPOKE..DIM_STORE ST ON SD.STORE_MEMBER_KEY = ST.MEMBER_KEY
                   JOIN EDW_SPOKE..DIM_DATE DT ON SH.TRANSACTION_DATE_KEY = DT.DATE_KEY
                   LEFT JOIN EDW_SPOKE..DIM_PRODUCT_BPS PD ON SD.SALES_PRODUCT_MEMBER_KEY = PD.MEMBER_KEY
                   WHERE (ST.CHANNEL_GROUP = 'RETAIL')
                   AND SD.ACTUAL_SALE_FLAG = 'Y'
                   AND (DT.DATE_VALUE <= (SELECT LAST_DAY_OF_PRIOR_FISCAL_PERIOD FROM EDW_SPOKE..DIM_DATE WHERE DATE_VALUE = CURRENT_DATE ) 
                   AND DT.DATE_VALUE >= (SELECT FIRST_DAY_OF_FISCAL_PERIOD FROM EDW_SPOKE..DIM_DATE WHERE DATE_VALUE = CURRENT_DATE-'60 Months'::INTERVAL))
                   AND SD.RETURN_FLAG <> 'Y'
                   AND PD.DEPARTMENT_MEMBER_NUMBER NOT IN (875,999)
                   AND PD.SKU_DISPOSITION_CODE IN ('A','N')
                   GROUP BY 1,2,3,4,5,6,7,8,9,10
                   ORDER BY 1,2,3,4,5,6,7,8,9,10")
res <- sqlQuery(ch, querytext)
res$STORE_NUMBER <- str_pad(res$STORE_NUMBER, width=2, side='left', pad='0')
res$FISCAL_YEAR_PERIOD <- as.character(res$FISCAL_YEAR_PERIOD)
res$DEPARTMENT_MEMBER_NUMBER <- str_pad(as.character(res$DEPARTMENT_MEMBER_NUMBER), width=4, side='left', pad=c('0'))
res$SUB_DEPARTMENT_MEMBER_NUMBER <- str_pad(as.character(res$SUB_DEPARTMENT_MEMBER_NUMBER), width=4, side='left', pad=c('0'))
res$CLASS_MEMBER_NUMBER <- str_pad(as.character(res$CLASS_MEMBER_NUMBER), width=4, side='left', pad=c('0'))
res$SUB_CLASS_MEMBER_NUMBER <- str_pad(as.character(res$SUB_CLASS_MEMBER_NUMBER), width=4, side='left', pad=c('0'))
res$STORE_DEPT <- paste(res$STORE_NUMBER,'_', res$DEPARTMENT_DISPLAY_NUMBER, '_', res$SUB_DEPARTMENT_MEMBER_NUMBER,
                        '_', res$CLASS_MEMBER_NUMBER, '_', res$SUB_CLASS_MEMBER_NUMBER, sep='')


FiscalPeriods <- unique(res$FISCAL_YEAR_PERIOD)
storeNums <- unique(res$STORE_NUMBER)

dt <- res
dt$DEPARTMENT_DISPLAY_NUMBER <- NULL
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
  substr(unique(res$STORE_DEPT), 1, 2),
  substr(unique(res$STORE_DEPT), 4, 7),
  substr(unique(res$STORE_DEPT), 9, 12),
  substr(unique(res$STORE_DEPT), 14, 17),
  substr(unique(res$STORE_DEPT), 19, 22)
  
)


gmyts <- gts(myts, groups=gps)


fc <- forecast(gmyts, h=12)

test <- as.data.frame(fc$bts)
test$time <- as.array(c(0,1,2,3,4,5,6,7,8,9,10,11))

testtidy <- gather(test,
                   'Store_Dept',
                   'Prediction',
                   1:170683)


DomoR::init('basspro.domo.com', '9602905555e390fe466e9de1940bf9a4cf8e734e5678b1ef')

#create(final, 'R - Class Forecast - PI', 'Updated Forecasts for Units sold by Product Class')

# 3ccf75d8-b302-4126-92fa-67b0b1494f8d
replace_ds('3ccf75d8-b302-4126-92fa-67b0b1494f8d', testtidy)
