require(tfestimators)
require(nzr)

nzConnectDSN('DBM_SANDBOX')

nzdata <- nzQuery(paste('SELECT * FROM MODEL_12MO_VAL'))

nzDisconnect()

head(nzdata)

vars <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
          'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
          'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
          'OTHER_SALES_LT','VISITS_LT','FISH_MARINE_PERC','CAMPING_WATERSPORTS_PERC',
          'HUNTING_PERC','FOOTWEAR_PERC','SHOOTING_PERC','GIFT_PERC','APPAREL_PERC',
          'OTHER_PERC','DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER','REWARDS_CUSTOMER',             
          'YOY_TREND','SALES_TARGET_N12')

vars.x <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
            'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
            'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
            'OTHER_SALES_LT','VISITS_LT','FISH_MARINE_PERC','CAMPING_WATERSPORTS_PERC',
            'HUNTING_PERC','FOOTWEAR_PERC','SHOOTING_PERC','GIFT_PERC','APPAREL_PERC',
            'OTHER_PERC','DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER','REWARDS_CUSTOMER',             
            'YOY_TREND')
vars.y <- c('SALES_TARGET_N12')

datasub <- nzdata[, vars]

datasub_input_fn <- function(data, num_epochs = 1) {
  input_fn(data,
           features = vars.x,
           response = vars.y,
           batch_size = 64,
           num_epochs = num_epochs)
}

cols <- feature_columns(
  column_numeric('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
                 'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
                 'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
                 'OTHER_SALES_LT','VISITS_LT','FISH_MARINE_PERC','CAMPING_WATERSPORTS_PERC',
                 'HUNTING_PERC','FOOTWEAR_PERC','SHOOTING_PERC','GIFT_PERC','APPAREL_PERC',
                 'OTHER_PERC','DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER',          
                 'YOY_TREND','REWARDS_CUSTOMER')
)


mod = dnn_regressor(hidden_units = c(10, 20, 10),
                    feature_columns = cols,
                    optimizer = "Adagrad",
                    activation_fn = "relu")


indices <- sample(1:nrow(datasub), size=0.80 *nrow(datasub))
train <- datasub[indices, ]
test <- datasub[-indices, ]

mod %>% train(datasub_input_fn(train, num_epochs=10))



