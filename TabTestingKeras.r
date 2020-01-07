TabModelData <- read.csv("~/GitHub/RAnalyses/TabModelData.csv")
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
collist <- c('MALES_IN_HOUSHOLD','REWARDS_CUSTOMER',
             'DAYS_AS_CUSTOMER','TOTAL_TRANSACTIONS','TOTAL_SPEND',
             'DAYS_SINCE_PURCHASE')
train.x <- df[, collist]


train.x$MALES_IN_HOUSHOLD <- scale(train.x$MALES_IN_HOUSHOLD)
train.x$FEMALES_IN_HOUSHOLD <-NULL
train.x$DAYS_AS_CUSTOMER <- scale(train.x$DAYS_AS_CUSTOMER)
train.x$TOTAL_TRANSACTIONS <- scale(train.x$TOTAL_TRANSACTIONS)
train.x$REW_TRANSACTIONS <- NULL
train.x$TOTAL_SPEND <- scale(train.x$TOTAL_SPEND)
train.x$DAYS_SINCE_PURCHASE <- scale(train.x$DAYS_SINCE_PURCHASE)

summary(train.x)

library(keras)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 12, activation = 'relu', input_shape = c(6)) %>%
  layer_dense(units = 24, activation = 'relu') %>%
  layer_dropout(rate=0.8) %>%
  layer_dense(units = 36, activation = 'relu') %>%
  layer_dropout(rate=0.8) %>%
  layer_dense(units = 18, activation = 'relu') %>%
  layer_dropout(rate=0.8) %>%
  layer_dense(units = 6, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')

summary(model)


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adadelta',
  metrics = c('accuracy')
)


trainX<- as.matrix(train.x)
trainY <- to_categorical(as.matrix(train.y), num_classes = 2)


history <- model %>% fit(x = trainX, y = trainY, 
                         epochs = 10, batch_size = 128, 
                         validation_split = 0.4,
                         verbose = 1,
                         shuffle = T
)

plot(history)

# Plot the model loss
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
lines(history$metrics$val_loss, col="green")
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Plot the model accuracy
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
lines(history$metrics$val_acc, col="green")
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))
