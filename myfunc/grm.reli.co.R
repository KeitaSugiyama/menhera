#信頼性係数の推定（段階反応モデル）
#引数
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     mu: 母平均
#     sigma: 母標準偏差
#     n:  求積点の数
#     D: 正規計量は1.7、ロイシティック計量は1.0
#戻り値
#     信頼性、誤差分散
grm.reli.co <- function(a, bc, mu=0, sigma=1, n=15, D=1.7){
   a<-as.vector(a);   bc<-as.matrix(bc)
   tiff<-grm.information(a, bc,theta=normal.qu(n=n,mu=mu,sigma=sigma)$quad.points, D=D)
   誤差分散<-sum((1/tiff$infor.test) * 
                     normal.qu(n=n,mu=mu,sigma=sigma)$quad.weights)
   信頼性<-sigma^2 / ( sigma^2 +誤差分散)
   fit<-(list(信頼性=信頼性,誤差分散=誤差分散))
   return(fit)}
