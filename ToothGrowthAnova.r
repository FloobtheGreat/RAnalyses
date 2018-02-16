data <- ToothGrowth

data$supp <- factor(data$supp)
data$dose <- factor(data$dose)

fit <- aov(len ~ supp + dose, data=data)

layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots

summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests 

TukeyHSD(fit)

layout(matrix(c(1),1,1))

library(gplots)
plotmeans(data$len~data$dose,xlab="Dose",
          ylab="Tooth Length", main="Mean Plot\nwith 95% CI") 

plotmeans(data$len~data$supp,xlab="Supplement",
          ylab="Tooth Length", main="Mean Plot\nwith 95% CI") 

