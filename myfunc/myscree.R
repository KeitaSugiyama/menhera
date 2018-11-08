#固有値のスクリープロットを描く
#引数
#     r: 生データ行列（受検者×項目）
#戻り値
#     固有値のスクリープロット
#  関数faを使用
#
myscree<-function(r,b="固有値の順位")
   {plot(fa(r)$e.values,type='o',xlab=b,ylab="固有値")}
