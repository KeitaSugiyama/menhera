#計量の変換
#引数
#     ip.old: 変換前の3母数の項目母数
#     method: "Na<La"  L計量をN計量に変換、a!=1 のとき
#             "La<Na"  N計量をL計量に変換、a!=1 のとき
#             "1<a"    １母数モデルの共通の係数を１に変換
#　　　　　　 "N1<L1"  L計量をN計量に変換、a =1 のとき
#             "L1<N1"  N計量をL計量に変換、a =1 のとき
#戻り値
#     ip.new: 変換後の3母数の項目母数
#注意点
#     "Na<La"と"La<Na"は何度も行き来できる。
#     "1<a"は１回しか使えない  (a=1) <- (a!=1) の方向だけ
#     "N1<L1"と"L1<N1"は何度も行き来できる。
ipt<- function(ip.old,method){
   ip.new <- ip.old
   switch(method,
        "Na<La"= {ip.new[,1]<-ip.old[,1]/1.7},
        "La<Na"= {ip.new[,1]<-ip.old[,1]*1.7},
        "1<a"= {ip.new[,2]<-ip.old[,1]*ip.old[,2]; ip.new[,1]<-1},
        "N1<L1"= {ip.new[,2]<-ip.old[,2]/1.7},
        "L1<N1"= {ip.new[,2]<-ip.old[,2]*1.7},
        )
   return(ip.new)
}
