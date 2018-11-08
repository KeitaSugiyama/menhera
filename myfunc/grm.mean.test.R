#集団に対する項目正答率、テスト平均の予測（段階反応モデル）
#(9.16)式の積分を利用する
##引数
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     w : 項目の重み（全ての項目の重みをベクトルで与える）
#     mu: 母平均
#     sigma: 母標準偏差
#     n:  求積点の数
#戻り値
#     mu.of.teat: テスト平均
#     w : 項目の重み
grm.mean.test<-function(a, bc,mu=0,sigma=1,D=1.7, n=15, 
   W=matrix(rep(c(0:ncol(bc)),nrow(bc)),nrow(bc),ncol(bc)+1,byrow=T)){
   a<-as.vector(a);   bc<-as.matrix(bc)
   tc<-grm.tcc(a, bc,D,theta=normal.qu(n=n,mu=mu,sigma=sigma)$quad.points)
   mu.of.test<-sum(tc[,"tcc.grm"]*normal.qu(n=n,mu=mu,sigma=sigma)$quad.weights)
   fit<-(list(mu.of.test=mu.of.test,W=W))
   class(fit)<-'mt2'
   return(invisible(fit))}

print.mt2 <- function(x){
   print(x$mu.of.test)
}
