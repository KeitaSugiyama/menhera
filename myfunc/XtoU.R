#カテゴリカル順序変数の値を0からC-1にする
#引数
#     X: 多変量カテゴリカル順序変数
#戻り値
#     U: 0からC-1のカテゴリカル順序変数
#
#
XtoU<-function(X){
   ns<-nrow(X); ni<-ncol(X)
   a01<-X[1,]
   for (i in 2:ns) {a01<-unique(c(a01,X[i,]))}
   a<-sort(unlist(a01))
   len<-length(a)
   b<-c(0:(len-1))
   Y<-X
   for (i in 1:ns){ 
      for (j in 1:ni){ 
         for (k in 1:len){ 
            if (X[i,j]==a[k]) {Y[i,j]=b[k]}}
      }
   }
   return(Y)
}
