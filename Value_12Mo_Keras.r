require(keras)
require(RODBC)
ch <- odbcConnect("DBM_SANDBOX", believeNRows=FALSE)
nzdata <- sqlQuery(ch, paste("SELECT * FROM MODEL_12MO_VAL"))

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


nzdata2 <- nzdata[ , vars ]
nzdata2 <- nzdata2[!is.na(nzdata2$DAYS_AS_CUSTOMER), ]

summary(nzdata2)


indices <- sample(1:nrow(nzdata2), size=0.80 *nrow(nzdata2))
train <- nzdata2[indices, ]
test <- nzdata2[-indices, ]


var_diagnostics <- function(data, yvars, xvars){
  for(j in yvars){
    for(i in xvars){
      
      corr <- cor.test(data[, i], data[ , j], method='pearson')
    
    
      form <- paste(j, ' ~ ', i)
      mod <- lm(formula = form, data=data)
      sm <- summary(mod)
      tpval <- sm$coefficients[2,4]
      est <- sm$coefficients[2,1]
      rsq <- sm$adj.r.squared
        
      
      
      if((rsq >= 0.1) & (tpval <= 0.05)) {
        print(paste(i,' Corr Coeff: ',corr$estimate, 'pval: ', corr$p.value))
        print(paste(i,' R2: ', rsq, ' Est: ', est, ' pval: ', tpval))
      } #end if
    } #end inner for loop
  } #end outer for loot
  
} #end function

var_diagnostics(train, vars.y, vars.x)

vars.x <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
            'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
            'VISITS_LT')


  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 16, activation = 'relu', input_shape = c(8)) %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 8, activation = 'relu') %>%
    layer_dense(units = 1)
  
  
  #summary(model)
  
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(lr = 0.001)
  )
  
  train.x <- as.matrix(train[, vars.x])
  train.y <- as.matrix(train[, vars.y])
  
  history <- model %>% fit(x = train.x, y = train.y, 
    epochs = 350, batch_size = 64, 
    validation_split = 0.2,
    verbose = 1,
    shuffle = T
  )
  
  plot(history)