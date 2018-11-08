import pyper
import pandas as pd

#データベース
db = pd.read_csv("data.csv")
params = pd.read_csv("params.csv")

#入力されたデータ(値は0~5の6段階評価)
user_data = pd.DataFrame([[3,2,1,1,1,1,4,1,2,1,1,1,4,1,1,1,"偏差値"]], 
columns=["V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","偏差値"])

data = (db.append(user_data, ignore_index=True))

#Rでの分析
r = pyper.R(use_pandas='True')
r.assign("data", data)
r.assign("params", params)

r("source('myfunc/myfunc.R')")
r("library(psych)")
r("library(irtoys)")
r("library(ltm)")
#17列目の偏差値は分析データから外し、最新行のみ分析
r("data<-data[nrow(data),1:16]")
#母数の推定
r("a <- grm.theta(data,a=params[,6]/1.7,bc=params[,c(1,2,3,4,5)],D=1.7,method ='ML')")
r("偏差値<-round(a[,1],4)*10+50")

#ユーザーの偏差値
value = r.get("偏差値")
data.iat[len(data)-1,16] = value
ranking = data.rank(method="min")
#偏差値
print(value)
#順位
print(ranking.iat[len(data)-1,16])
#受験人口
print(len(data))