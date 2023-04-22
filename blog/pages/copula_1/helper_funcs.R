pair_w_hist <- function(data, names, ...){
  n <- ncol(data)
  par(mfrow=c(n,n))
  for (i in 1:n){
    for (j in 1:n){
      if (i == j)
        hist(data[,i], col=adjustcolor('darkblue', 0.4), breaks=seq(0,1,0.05),
             ylab='', xlab='', main=names[i], ...)
      else
        plot(data[,j], data[,i], pch=16, ylab='', xlab='', main='',xlim=c(0,1), ylim=c(0,1),
             ...)
    }
  }
  par(mfrow=c(1,1))
}
