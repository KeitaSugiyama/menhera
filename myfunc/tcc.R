# tcc θに対して重み付のテスト特性曲線を返す
#引数
#     ip: 3母数の項目母数
#     w : 項目の重み（全ての項目の重みをベクトルで与える）
#     theta: 特性の尺度
#     D: 正規計量は1.7、ロジスティック計量は1.0
#戻り値
#     theta   : 特性の尺度
#     tcc.trf : テスト平均値
#
#   プロット関数も定義されている。
tcc<-function(ip, w=rep(1,nrow(ip)),theta=seq(-3,3,0.1),D=1.7){
  a<-ip[,1]; b<-ip[,2]; c<-ip[,3]
  tc<-0
  for (i in 1:length(a)){
    p<-c[i]+((1-c[i])/(1+exp(-1*D*a[i]*(theta-b[i]))))
    tc<-tc+w[i]*p
  }
  fit<-cbind(theta,tc)
  colnames(fit)<-c("theta","tcc.trf")
  class(fit)<-'tcc'
  return(fit)}

plot.tcc <- function(x,xlab="theta",ylab="test score", ...){
   plot(x[,1],x[,2],type="l",xlab=xlab,ylab=ylab, ...)
}

