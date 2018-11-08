#被検者母数の推定(ML,MAP)（段階反応モデル）
#引数
#     X: 0からC-1の多変量カテゴリカル順序変数(ns x ni)
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     D: 正規計量は1.7、ロジスティック計量は1.0
#     mu: 事前分布の平均　規定値は 0
#     sigma: 事前分布の標準偏差　規定値は 1
#     method: 推定法の指定、"ML"は最尤法、"BM"はベイズMAP推定、規定値は"ML"
#       max : 推定値の最大値
#       min : 推定値の最小値
#戻り値
#     theta: 被検者母数のベクトル
#     se:    標準誤差
grm.theta <- function(X, a, bc, D=1.7, mu = 0, sigma = 1, method = "ML",max=3.0,min=-3.0){
   X<-as.matrix(X);     a<-as.vector(a);   bc<-as.matrix(bc)
   ns  <- nrow(X);      ni<- ncol(X);      nc  <- ncol(bc)
   theta<-numeric(ns);  LL01<-numeric(ni)
#対数尤度関数
  grm.LL <- function(theta){  
    p01<-1/(1+exp(-D*(a %*% t(rep(1,nc)))*(matrix(theta,ni,nc)-bc)))  #ni*nc
    p02<- cbind( rep(1,ni), p01, rep(0,ni))                       #ni*(nc+2) 
    for (j in 1:ni){ LL01[j]<-p02[j,x[j]+1]-p02[j,x[j]+2]}
    LL02<-sum(log(LL01))
    if (method != "ML") {LL02 = LL02 + log(dnorm(theta, mu, sigma))}
    return(LL02)
  }
#最適化
  for (i in 1:ns){
    x<-as.vector(X[i,])
    theta[i]<-optim(0.0, grm.LL, method="BFGS", control=list(fnscale = -1))$pa
    if (theta[i]>max) {theta[i]<-max}
    if (theta[i]<min) {theta[i]<-min}
  }
  ti<-grm.information(a, bc, theta, D)$infor.test
  if (method != "ML") {ti = ti + 1/(sigma * sigma)}
  se = sqrt(1/ti)
  fit<-cbind(theta,se)
return(fit)
}
