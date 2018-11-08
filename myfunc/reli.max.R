#最大信頼性とそれを与える平均・SD
#引数
#     ip: 3母数の項目母数
#戻り値
#     最大信頼性とそれを与える平均・標準偏差
#
#   現時点でロジスティック計量のみに対応
#   出力されるのは被験者分布の「標準偏差」であることに注意する
reli.max <- function(ip){
    reli<-function(x){-1*reli.co(ip,x[1],x[2])$信頼性}
    aaa<-optim(c(0,1),reli)
    mu <- aaa$par[1]
    sigma <- aaa$par[2]
    sigma_e_2<-reli.co(ip,mu,sigma)$誤差分散
    romax <- -1*aaa$value
    fit<-c(mu,sigma,sigma_e_2,romax)
    names(fit)<-c("シータ平均","シータ標準偏差","誤差分散","最大信頼性")
    return(fit)}
