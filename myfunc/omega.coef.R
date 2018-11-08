#Ω係数を計算する(１因子でも多因子でも計算可能)
#引数
#     x: 生データ行列（受検者×項目）
#     nfactors:　因子数
#戻り値
#     Ω係数
#(独自性に観測変数の分散をかけて、誤差分散を求める)
#  関数faを使用
#
omega.coef<-function(x,nfactors=1,fm="ml",...){
  1 - ( sum(fa(x,nfactors=nfactors,fm=fm,...)$uniquenesses*diag(var(x))) / 
        sum(var(x)) )
}
