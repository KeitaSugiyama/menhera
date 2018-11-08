#その項目自身を除いたテスト得点との相関による識別力
#引数
#    x : 受験者×項目得点の行列
#
#戻り値
#    識別力ベクトル
#
disc.co<-function(x){
  m<-ncol(x)
  test.sco <-  apply(x, 1,'sum')
  test.sco.a<-matrix(test.sco,nrow(x),ncol(x),byrow=F)
  test.sco.b<-test.sco.a - x
  dis<-c(1:m)
  for (i in 1:m){
    dis[i]<-cor(test.sco.b[,i], x[,i])
  }
  names(dis)<-colnames(x)
  return(dis)
}
