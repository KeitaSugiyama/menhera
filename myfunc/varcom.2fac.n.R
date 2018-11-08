#繰り返しの無い2因子データの分散成分の推定
#
#引数
#    x : summary(aov())の結果のリスト
#
#戻り値
#    最終列に分散成分を有する分散分析表
#
varcom.2fac.n <- function(x,rs="R"){       
   if(rs =="S") {y<-x}
      else    {y<-x[[1]]}
   sa<-(y$"Mean Sq"[1]-y$"Mean Sq"[3])/(y$"Df"[2]+1)
   sb<-(y$"Mean Sq"[2]-y$"Mean Sq"[3])/(y$"Df"[1]+1)
   se<-y$"Mean Sq"[3]
   y[,6]<-c(sa,sb,se)
   names(y)<-c("Df","Sum Sq","Mean Sq","F value",
      "Pr(>F)","分散成分")
   cat("繰り返しの無い2因子データの分散成分の推定",fill=T)
   return(y)
}
