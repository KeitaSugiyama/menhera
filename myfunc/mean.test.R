#集団に対する項目正答率、テスト平均の予測
#数値積分を利用する
##引数
#     ip: 3母数の項目母数
#     w : 項目の重み（全ての項目の重みをベクトルで与える）
#     mu: 母平均
#     sigma: 母標準偏差
#     D: 正規計量は1.7、ロイシティック計量は1.0
#戻り値
#     mu.of.test: テスト平均
#     w : 項目の重み
#     mu.item : 項目の正答率の予測値
mean.test<-function(ip, w=rep(1,nrow(ip)),mu=0,sigma=1,D=1.7){
  a <- ip[,1]; b<-ip[,2]; c<-ip[,3]
  ni<-nrow(ip)
  mu.item<-numeric(ni)
  for (i in 1:ni){
    pigms<-function(theta)  
        {(c[i]+((1-c[i])/(1+exp(-1*D*a[i]*(theta-b[i])))))*
          dnorm(theta, mean=mu, sd=sigma)}
    mu.item[i] <- integrate(pigms, -Inf, Inf)$value
  }
  mu.of.test <- sum(w*mu.item)
  fit<-(list(mu.of.test=mu.of.test,w=w,mu.item=mu.item,mu=mu,sigma=sigma))
  class(fit)<-'mt2'
  return(invisible(fit))}

print.mt2 <- function(x){
   print(x$mu.of.test)
}
