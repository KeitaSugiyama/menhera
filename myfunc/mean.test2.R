#集団に対する項目正答率、テスト平均の予測
#(6.14)式の近似式を利用する。3母数モデルは計算できない。
#                            2母数までしか計算できない
#引数
#     ip: 3母数の項目母数
#     w : 項目の重み（全ての項目の重みをベクトルで与える）
#     mu: 母平均
#     sigma: 母標準偏差
#     D: 正規計量は1.7、ロジスティック計量は1.0
#戻り値
#     mu.of.test: テスト平均
#     w : 項目の重み
#     mu.item : 項目の正答率の予測値
mean.test2<-function(ip, w=rep(1,nrow(ip)),mu=0,sigma=1,D=1.7){
  a <- ip[,1]; b<-ip[,2]; c<-ip[,3]
  mu.item <- 1/(1+exp((-1*D)*((a/sigma)/(sqrt(a^2 + (1/(sigma^2)))))*(mu-b)))
  mu.of.test <- sum(w*mu.item)
  fit<-(list(mu.of.test=mu.of.test,w=w,mu.item=mu.item,mu=mu,sigma=sigma))
  class(fit)<-'mt2'
  return(invisible(fit))}

print.mt2 <- function(x){
   print(x$mu.of.test)
}
