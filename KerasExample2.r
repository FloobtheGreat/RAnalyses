library(keras)
library(mlbench)

data("BostonHousing")

var.x <- c('crim','zn','indus','nox','rm','age','dis','rad','tax','ptratio','b','lstat')
var.y <- c('medv')
x <- as.matrix(BostonHousing[, var.x])
y <- as.matrix(BostonHousing[, var.y])


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 24, activation = 'relu', input_shape = c(12)) %>% 
  #layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 48, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 12, activation = 'relu') %>% 
  #layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss = 'mse',
  optimizer = optimizer_rmsprop(lr=0.001)
)

history <- model %>% fit(
  x, y, 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2
)
plot(history)


