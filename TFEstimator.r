library(tfestimators)

mtcdat <- mtcars

# return an input_fn for a given subset of data
mtcars_input_fn <- function(data, num_epochs = 1, batch_size = 64) {
  input_fn(data, 
           features = c("disp", "cyl"), 
           response = "mpg",
           batch_size = batch_size,
           num_epochs = num_epochs)
}

cols <- feature_columns(
  column_numeric("disp"),
  column_numeric("cyl")
)

model <- dnn_regressor(feature_columns = cols,
                       hidden_units = c(10,10,10))

indices <- sample(1:nrow(mtcars), size = 0.80 * nrow(mtcars))
train <- mtcars[indices, ]
test  <- mtcars[-indices, ]

# train the model
model %>% train(mtcars_input_fn(train, num_epochs = 100))

#tensorboard(log_dir = "/tmp/tftest", launch_browser = TRUE)

model %>% evaluate(mtcars_input_fn(test))

obs <- mtcars[1:3, ]
model %>% predict(mtcars_input_fn(obs))
