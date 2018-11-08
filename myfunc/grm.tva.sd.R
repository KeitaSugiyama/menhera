# grm.tva.sd θの関数で任意の重みのテスト得点の分散と標準偏差
#引数
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     theta: 特性の尺度
#     W : 項目の重み行列　i項目jカテゴリの重み（ni * nc+1）
#         規定値は0からC-1 
#     D: 正規計量は1.7、ロイシティック計量は1.0
#戻り値
#     theta   : 特性の尺度
#     tva.grm : テストの分散
#     tsd.grm : テストの標準偏差
#
#     プロット関数も定義。
grm.tva.sd<-function(a, bc, D=1.7, theta=seq(-3,3,0.1), 
           W=matrix(rep(c(0:ncol(bc)),nrow(bc)),nrow(bc),ncol(bc)+1,byrow=T)){
  a<-as.vector(a);   bc<-as.matrix(bc);   ni<- nrow(bc)
  nc<- ncol(bc); nt<-length(theta); tva.grm<-numeric(nt); tsd.grm<-numeric(nt)
  for (i in 1:nt){
    p01<-1/(1+exp((-1)*D*(a %*% t(rep(1,nc)))*(matrix(theta[i],ni,nc)-bc))) #ni*nc
    p02<- cbind( rep(1,ni), p01, rep(0,ni))                       #ni*(nc+2) 
    p03<-matrix(0,ni,nc+1)
    for (j in 1:(nc+1)){
       p03[,j]<-p02[,j]-p02[,j+1]
    }
    tva.grm[i]<-sum((1-p03) * p03 * W^{2})
    tsd.grm[i]<-sqrt(tva.grm[i])
  }
  fit<-cbind(theta,tva.grm,tsd.grm)
  colnames(fit)<-c("theta","tva.grm","tsd.grm")
  class(fit)<-'tva.sd.grm'
  return(fit)}

plot.tva.sd.grm <- function(x,method='sd',xlab="theta", ...){
   if (method=='sd'){
   plot(x[,1],x[,3],type="l",xlab=xlab, ...)
   }else{
   plot(x[,1],x[,2],type="l",xlab=xlab, ...)
   }
}
