#共通被験者計画における周辺最尤推定による等化係数
#豊田(1986)の方法(ロジスティック計量にのみ対応)
#引数
#    old.ip : 目的とするスケール上に既に乗っている母数のセット。
#    new.ip : old.ipと同じスケール上に位置付けなければならない母数のセット。
#    old.th : old.ipによる被験者母数の推定尺度値
#    new.th : new.ipによる被験者母数の推定尺度値(old.thと同じ長さ)
#    qun    : 求積点の数、規定値は15、正式に利用する際には30程度を指定する。
#    rasch  : 論理値　1母数モデルのときはT 規定値はF
#
#戻り値
#    傾き      :等化係数傾き
#    切片      :等化係数切片
#    mu        :newスケール上の被験者母数の母平均
#    sigma     :newスケール上の被験者母数の母標準偏差
#    初期傾き  :mean-sigma法による傾き
#    初期切片  :mean-sigma法による切片
#    scaled.ip :oldスケール上に変換したnew.ip
#
equate.com.sub<-function(old.ip,new.ip,old.th,new.th,rasch=F,qun=15,lower=-3,upper=3){
ns<-length(old.th)      #被験者数

#周辺対数尤度
merloglike<-function(slope, intercept, mu, sigma){
  norq<-normal.qu(n=qun,mu=mu,sigma=sigma,lower=lower,upper=upper)
  qp<-norq$quad.points
  qw<-norq$quad.weights
  tr.qp<-slope*qp+intercept
  cum<-0
  for (i in 1:ns){
    cu<-sum(dnorm(old.th[i], mean=tr.qp, sd=sqrt(1/(tif(old.ip,tr.qp)$f))) *
            dnorm(new.th[i], mean=   qp, sd=sqrt(1/(tif(new.ip,   qp)$f))) *qw)
    cum<-cum+log(cu)}
return(cum)}

#1母数、2,3母数モデルの最適化関数
merloglike01<-function(x){-1*merloglike(slope= 1.0,intercept=x[1],mu=x[2],sigma=x[3])}
merloglike02<-function(x){-1*merloglike(slope=x[1],intercept=x[2],mu=x[3],sigma=x[4])}

#初期値設定
omt<-mean(old.th);  ost<-sd(old.th);  nmt<-mean(new.th);  nst<-sd(new.th)
if (rasch==T) {k<-1} else{k<-ost/nst};  l<-omt-k*nmt

#最適化実行
if (rasch==T) { aaa<-optim(c(l, nmt, nst),merloglike01);  
   slope<-1.0; intercept <-aaa$par[1]; mu <-aaa$par[2]; sigma <-aaa$par[3]}
else  { aaa<-optim(c(k, l, nmt, nst),merloglike02); slope<-aaa$par[1]; 
   intercept <-aaa$par[2]; mu <-aaa$par[3]; sigma <-aaa$par[4]}

merloglike<- (-1)*aaa$value
scaled.ip <-new.ip
scaled.ip[,1] <-new.ip[,1]/slope 
scaled.ip[,2] <-new.ip[,2]*slope+intercept 

fit<-list(傾き=slope, 切片=intercept, mu=mu, sigma=sigma, 初期傾き=k, 初期切片=l,scaled.ip=scaled.ip)
return(fit)}
