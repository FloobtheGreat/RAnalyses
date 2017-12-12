
Mar1Tab2 <- Mar1Tab[Mar1Tab$SALES > 0]

res.ftest <- var.test(SALES ~ HOLDOUT_FLAG, data = Mar1Tab2)
res.ftest

res.ttest <- t.test(SALES ~ HOLDOUT_FLAG, data = Mar1Tab1)
res.ttest


library(ggplot2)


ggplot(Mar1Tab2, aes(x=SALES))+ geom_histogram(bins=100)+facet_grid(~HOLDOUT_FLAG)


library(MASS)
tbl = table(Mar1Tab$HOLDOUT_FLAG, Mar1Tab$RESPONDED_FLAG)
tbl

chisq.test(tbl)

res.wilcox <- wilcox.test(SALES ~ HOLDOUT_FLAG, data = Mar1Tab)
res.wilcox
