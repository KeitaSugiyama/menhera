#IRTにおける信頼性係数の推定
#引数
#     ip: 3母数の項目母数
#     mu: 母平均
#     sigma: 母標準偏差
#     n:  求積点の数
#戻り値
#     信頼性、誤差分散
#
#   現時点でロジスティック計量のみに対応
#   指定するのは被験者分布の「標準偏差」であることに注意する
reli.co <- function(ip,mu=0,sigma=1,n=15){
   tiff<-tif(ip,normal.qu(n=n,mu=mu,sigma=sigma)$quad.points)
   誤差分散<-sum((1/tiff$f) * normal.qu(n=n,mu=mu,sigma=sigma)$quad.weights)
   信頼性<-sigma^2 / ( sigma^2 +誤差分散)
   fit<-(list(信頼性=信頼性,誤差分散=誤差分散))
   return(fit)}
