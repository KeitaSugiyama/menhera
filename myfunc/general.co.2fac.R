#一般化可能性係数（繰り返しの無い2因子計画）
#
#引数
#    x : summary(aov())の結果のリスト
#    k : ２番目に投入した要因の数のベクトル
#　　　　　　ちなみに１番目の要因は測定対象
#
#戻り値
#    2列の行列。1列目は評定数。2列目は一般化可能性係数。
#    データに一致した場合はクロンバックのアルファ係数
general.co.2fac <- function(x,k,rs="R"){       
   if(rs =="S") {y<-x}
      else    {y<-x[[1]]}
   sa<-(y$"Mean Sq"[1]-y$"Mean Sq"[3])/(y$"Df"[2]+1)
   sb<-(y$"Mean Sq"[2]-y$"Mean Sq"[3])/(y$"Df"[1]+1)
   se<-y$"Mean Sq"[3]
   評定数<-k
   一般化係数<-round(sa/(sa+se/k),3)
   az<-cbind(評定数,一般化係数)
   return(az)
}
