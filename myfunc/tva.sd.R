# tva.sd θに対して重み付のテスト得点の分散・SD
#引数
#     ip: 3母数の項目母数
#     w : 項目の重み（全ての項目の重みをベクトルで与える）
#     theta: 特性の尺度
#     D: 正規計量は1.7、ロジスティック計量は1.0
#戻り値
#     theta   : 特性の尺度
#     tva     : テスト得点の分散
#     tsd     : テスト得点のSD
#
#     プロット関数も定義されている。
tva.sd<-function(ip, w=rep(1,nrow(ip)),theta=seq(-3,3,0.1),D=1.7){
  a<-ip[,1]; b<-ip[,2]; c<-ip[,3]
  tv<-0
  for (i in 1:length(a)){
    p<-c[i]+((1-c[i])/(1+exp(-1*D*a[i]*(theta-b[i]))))
    tv<-tv+w[i]^2*p*(1-p)
  }
  fit<-cbind(theta,tv,sqrt(tv))
  colnames(fit)<-c("theta","test.va","test.sd")
  class(fit)<-'tva.sd'
  return(fit)}

plot.tva.sd <- function(x,method='sd',xlab="theta", ...){
   if (method=='sd'){
   plot(x[,1],x[,3],type="l",xlab=xlab, ...)
   }else{
   plot(x[,1],x[,2],type="l",xlab=xlab, ...)
   }
}

