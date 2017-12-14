require(nza)
require(nzr)
require(xgboost)

f <- function(x) {
  x^(1/3)
}


nzConnectDSN('DBM_SANDBOX')

nzdata = nz.data.frame('MODEL_12MO_VAL')

# if(nzExistTable('apply_output')) nzDeleteTable('apply_output')
# 
# r <- nzApply(nzdata$SALES_TARGET_N12, NULL, f, output.name='apply_output', output.signature = list(CUBEROOT=NZ.DOUBLE))
# 
# hist(r$CUBEROOT, breaks=100)





nzDisconnect()
