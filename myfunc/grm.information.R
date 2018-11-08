#項目情報・テスト情報関数（段階反応モデル）
#引数
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     theta: 情報量を計算するseq  スカラーでも可
#     D: 正規計量は1.7、ロイシティック計量は1.0
#戻り値
#     infor.test: テスト情報      (nt)
#     infor.item: 項目情報        (nt * ni)
#     theta: 入力した特性値       
grm.information <- function(a, bc, theta=seq(-3,3,0.1), D=1.7){
   a<-as.vector(a);   bc<-as.matrix(bc)
   ni  <- nrow(bc);  nc<- ncol(bc);  nt<-length(theta)
   infor.item <- matrix(0,nt,ni)
   for (i in 1:nt){
     p01 <- 1/(1+exp(-D*(a %*% t(rep(1,nc)))*(matrix(theta[i],ni,nc) - bc))) 
     p02 <- cbind( rep(1,ni), p01, rep(0,ni)); q02 <- 1-p02      #ni*(nc+2) 
     p03 <- matrix(0,ni,(nc+1))
     for (j in 1:(nc+1)){
       p03[,j]<-((p02[,j]*q02[,j]-p02[,j+1]*q02[,j+1])^2)/(p02[,j]-p02[,j+1])
     }
     infor.item[i,] <- a^2 * t(apply(p03,1,sum))
   }
   infor.item <- infor.item * D^2
   infor.test <- apply(infor.item,1,sum)
   fit<-(list(infor.item=infor.item,infor.test=infor.test,theta=theta))
   class(fit)<-'grm.information'
   return(invisible(fit))
   }

print.grm.information <- function(x){
   head(x$infor.test)
}
plot.grm.information <- function(x,xlab="theta",ylab="information", ...){
   plot(x$theta,x$infor.test,type="l",xlab=xlab, ylab=ylab, ...)
}

