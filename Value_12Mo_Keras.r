require(keras)
require(RODBC)
require(mice)
ch <- odbcConnect("DBM_SANDBOX", believeNRows=FALSE)
nzdata <- sqlQuery(ch, paste("SELECT * FROM MODEL_12MO_VAL"))

head(nzdata)

vars <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
          'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
          'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
          'OTHER_SALES_LT','VISITS_LT',
          'DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER','REWARDS_CUSTOMER',             
          'YOY_TREND','SALES_TARGET_N12')

vars.x <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
            'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
            'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
            'OTHER_SALES_LT','VISITS_LT',
            'DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER','REWARDS_CUSTOMER',             
            'YOY_TREND')
vars.y <- c('SALES_TARGET_N12')

vars.x.num <-  c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
                'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
                'FOOTWEAR_SALES_LT','SHOOTING_SALES_LT','GIFT_SALES_LT','APPAREL_SALES_LT',
                'OTHER_SALES_LT','VISITS_LT',
                'DAYS_SINCE_PURCHASE','DAYS_AS_CUSTOMER',             
                'YOY_TREND')

nzdata2 <- nzdata[ , vars ]

summary(nzdata2[ , vars.x.num])


var_diagnostics <- function(data, yvars, xvars){
  for(j in yvars){
    for(i in xvars){
      
      print(paste(i, 'contains', sum(is.na(data[,i])), 'empty values.'))
      
      # hist(data[ , i], breaks='FD',
      #      main=paste("Histogram of" , i))
      # plot(data[,i], data[,j], type = 'p', xlab=paste(i), ylab=paste(j))
      
      # corr <- cor.test(data[, i], data[ , j], method='pearson')
      # 
      # 
      # form <- paste(j, ' ~ ', i)
      # mod <- lm(formula = form, data=data)
      # sm <- summary(mod)
      # tpval <- sm$coefficients[2,4]
      # est <- sm$coefficients[2,1]
      # rsq <- sm$adj.r.squared
      #   
      # 
      # 
      # if((rsq >= 0.05) & (tpval <= 0.1)) {
      #   print(paste(i,' Corr Coeff: ',corr$estimate, 'pval: ', corr$p.value))
      #   print(paste(i,' R2: ', rsq, ' Est: ', est, ' pval: ', tpval))
      # } #end if
    } #end inner for loop
  } #end outer for loot
  
} #end function

var_diagnostics(nzdata2, vars.y, vars.x)





nzdata2 <- nzdata2[!is.na(nzdata2$DAYS_AS_CUSTOMER), ]

## Scale Data

scaled = scale(nzdata2, center = F)

summary(scaled)

mahal <- mahalanobis(scaled[ , vars.x.num],
                     colMeans(scaled[ , vars.x.num], na.rm = TRUE),
                     cov(scaled[ , vars.x.num], 
                         use = "pairwise.complete.obs"),
                     tol=1e-20)

cutoff <- qchisq(1-.001, length(vars.x.num))

summary(mahal < cutoff)


nooutliers <- subset(scaled, mahal < cutoff)

summary(nooutliers)

correl <- cor(nooutliers[ ,vars.x.num])
symnum(correl)

indices <- sample(1:nrow(nooutliers), size=0.80 *nrow(nooutliers))
train <- as.data.frame(nooutliers[indices, ])
test <- as.data.frame(nooutliers[-indices, ])


var_diagnostics <- function(data, yvars, xvars){
  for(j in yvars){
    for(i in xvars){
      
      hist(data[ , i], breaks='FD')
      
      
      # corr <- cor.test(data[, i], data[ , j], method='pearson')
      # 
      # 
      # form <- paste(j, ' ~ ', i)
      # mod <- lm(formula = form, data=data)
      # sm <- summary(mod)
      # tpval <- sm$coefficients[2,4]
      # est <- sm$coefficients[2,1]
      # rsq <- sm$adj.r.squared
      #   
      # 
      # 
      # if((rsq >= 0.05) & (tpval <= 0.1)) {
      #   print(paste(i,' Corr Coeff: ',corr$estimate, 'pval: ', corr$p.value))
      #   print(paste(i,' R2: ', rsq, ' Est: ', est, ' pval: ', tpval))
      # } #end if
    } #end inner for loop
  } #end outer for loot
  
} #end function

var_diagnostics(train, vars.y, vars.x)

vars.x <- c('SALES_R12','SALES_R24','SALES_R48','SALES_RLT',
            'FISHING_MARINE_SALES_LT','CAMPING_WATERSPORTS_SALES_LT','HUNTING_HUNTCLOTH_SALES_LT',
            'VISITS_LT')


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 68, activation = 'relu', input_shape = c(17)) %>%
  layer_dense(units = 34, activation = 'relu') %>%
  layer_dense(units = 17, activation = 'relu') %>%
  layer_dense(units = 1)
  
  
  summary(model)
  
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(lr = 0.001)
  )
  
  train.x <- as.matrix(train[, vars.x])
  train.y <- as.matrix(train[, vars.y])
  
  history <- model %>% fit(x = train.x, y = train.y, 
    epochs = 20, batch_size = 128, 
    validation_split = 0.2,
    verbose = 1,
    shuffle = T
  )
  
  plot(history)