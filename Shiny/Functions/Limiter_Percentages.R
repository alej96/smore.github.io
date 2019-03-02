X = matrix(rnorm(100,2,2), ncol=1)
Y = array(0/0, dim=c(nrow(X), ncol(X)))

X = Store_POS$`Store Index`
Y = array(0/0, dim=c(nrow(Store_POS), 1))


case = which( (X-1)<0 )
if (length(case)>0) { Y[case] = 0.1 }

case = which( (X-1)*(X-1.5)<=0 )
Y[case] = 0.25

case = which( (X-1.5)*(X-4)<=0 )
Y[case] = 0.5

case = which(X>=4)
Y[case] = 0.75

Store_POS$`Limiter Percentages`= Y



