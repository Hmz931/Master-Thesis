# -------------------------------------------------------------------------
# Generate aggregate uncertainty estimates
# -------------------------------------------------------------------------
  
# Initialization

T = dim(ut)[1]
N = dim(ut)[2]
h = dim(ut)[3]

# Cross-sectional average
utcsa = matrix(0,T,h)
for (i in 1:h){
  utcsa[,i] = rowMeans(sqrt(ut[,,i]))
}

save.image("ut.RData")

write_xlsx(data.frame(cbind( datatime[5:244] , utcsa )) , 'Tunisian_Uncertainty_Index.xlsx'   )
