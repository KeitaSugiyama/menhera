#最大信頼性とそれを与える平均・SD（段階反応モデル）
#引数
#     a: 識別力ベクトル(ni)
#     bc: 困難度母数(ni x nc)     ncはカテゴリ数-1
#     D: 正規計量は1.7、ロイシティック計量は1.0
#戻り値
#     最大信頼性とそれを与える平均・SD
grm.reli.max <- function(a,bc,D=1.7){
    a<-as.vector(a);   bc<-as.matrix(bc)
    reli<-function(x){-1*grm.reli.co(a, bc, x[1], x[2], D=D)$信頼性}
    aaa<-optim(c(0,1),reli)
    mu <- aaa$par[1]
    sigma <- aaa$par[2]
    romax <- -1*aaa$value
    esigma2<- sigma^2 * (1-romax) / romax
    fit<-c(mu,sigma,romax,esigma2)
    names(fit)<-c("平均","SD","最大信頼性","誤差分散")
    return(fit)}
