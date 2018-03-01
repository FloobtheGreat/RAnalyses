library(keras)
library(mlbench)

data("BostonHousing")

var.x <- c('crim','zn','indus','nox','rm','age','dis','rad','tax','ptratio','b','lstat')
var.y <- c('medv')
x <- as.matrix(BostonHousing[, var.x])
y <- as.matrix(BostonHousing[, var.y])


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 48, activation = 'relu', input_shape = c(12)) %>% 
  #layer_dropout(rate = 0.8) %>% 
  layer_dense(units = 24, activation = 'relu') %>%
  #layer_dropout(rate = 0.9) %>% 
  layer_dense(units = 12, activation = 'relu') %>% 
  #layer_dropout(rate = 0.9) %>% 
  layer_dense(units = 6, activation = 'relu') %>% 
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss = 'mse',
  optimizer = optimizer_adam(lr = .0001)
)

history <- model %>% fit(
  x, y, 
  epochs = 400, batch_size = 64, 
  validation_split = 0.3
)
plot(history)



