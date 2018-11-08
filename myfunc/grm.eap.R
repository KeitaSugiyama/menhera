#被検者母数のEAP推定（段階反応モデル）
#引数
#     X: 0からC-1の多変量カテゴリカル順序変数(ns x ni)
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     D: 正規計量は1.7、ロジスティック計量は1.0
#     mu: 事前分布の平均　規定値は 0
#     sigma: 事前分布の標準偏差　規定値は 1
#     n: 積分求積点
#
#戻り値
#     o: (ns x 2)の行列、1列目は推定値、2列目は標準誤差
#
grm.eap <- function(X, a, bc, D=1.7, mu=0, sigma=1, n=30){
   X<-as.matrix(X);     a<-as.vector(a);   bc<-as.matrix(bc)
   ns  <- nrow(X);      ni<- ncol(X);      nc  <- ncol(bc)
   qp = normal.qu(n,mu=mu,sigma=sigma)$quad.points
   qw = normal.qu(n,mu=mu,sigma=sigma)$quad.weights
   o=sapply(1:ns, function(i) eap.inside(x=X[i, ],qp,qw,a,bc,D,nc,ni,n),USE.NAMES=F)
   rownames(o) = c("est", "sem")
   return(t(o))
}


eap.inside <- function (x,qp,qw,a,bc,D,nc,ni,n) {
    grm.LL <- function(theta){  
      LL01<-numeric(ni)
      p01<-1/(1+exp(-D*(a %*% t(rep(1,nc)))*(matrix(theta,ni,nc)-bc)))  #ni*nc
      p02<- cbind( rep(1,ni), p01, rep(0,ni))                       #ni*(nc+2) 
      for (j in 1:ni){ LL01[j]<-p02[j,x[j]+1]-p02[j,x[j]+2]}
      LL02<-sum(log(LL01))
      return(LL02)
    }
    ll<-numeric(n)
    for (i in 1:n) {
       ll[i]<-grm.LL(qp[i])
    }
    wl = exp(ll) * qw
    swl = sum(wl)
    xx = sum(wl * qp)/swl
    dev = qp - xx
    sem = sqrt(sum(wl * dev * dev)/swl)
    return(c(xx, sem))
}
