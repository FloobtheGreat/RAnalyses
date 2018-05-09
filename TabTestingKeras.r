TabModelData <- read.csv("~/Git/RAnalyses/TabModelData.csv")
View(TabModelData)
df <- TabModelData
TabModelData <- NULL
TabModelData <- NULL
df$DATE_VALUE <- NULL
df$RECORD <- NULL
df$LAST_NAME <- NULL
df$ADDRESS_LINE_1
df$ADDRESS_LINE_1 <- NULL
df$STATE_CODE <- NULL
df$ZIP <- NULL
df$MINSALEDATE <- NULL
df$LASTTABDATE <- NULL
summary(df)

df$REWARDS_CUSTOMER <- factor(df$REWARDS_CUSTOMER)

train.y <- df$PURCHASED_FIRST_15
collist <- c('MALES_IN_HOUSHOLD','FEMALES_IN_HOUSHOLD','REWARDS_CUSTOMER',
             'DAYS_AS_CUSTOMER','TOTAL_TRANSACTIONS','REW_TRANSACTIONS','TOTAL_SPEND',
             'DAYS_SINCE_PURCHASE')
train.x <- df[, collist]


train.x$MALES_IN_HOUSHOLD <- scale(train.x$MALES_IN_HOUSHOLD)
train.x$FEMALES_IN_HOUSHOLD <-scale(train.x$FEMALES_IN_HOUSHOLD)
train.x$DAYS_AS_CUSTOMER <- scale(train.x$DAYS_AS_CUSTOMER)
train.x$TOTAL_TRANSACTIONS <- scale(train.x$TOTAL_TRANSACTIONS)
train.x$REW_TRANSACTIONS <- scale(train.x$REW_TRANSACTIONS)
train.x$TOTAL_SPEND <- scale(train.x$TOTAL_SPEND)
train.x$DAYS_SINCE_PURCHASE <- scale(train.x$DAYS_SINCE_PURCHASE)

summary(train.x)

library(keras)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 16, activation = 'relu', input_shape = c(8)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'relu')


model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)