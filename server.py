#!/bin/env python http://localhost:8888/
# -*- coding: utf-8 -*-
from __future__ import print_function
import os
import tornado.ioloop
import tornado.web
import pyper
import pandas as pd

class MainHandler(tornado.web.RequestHandler):
    def get(self):
        self.render("index.html")
        
    def post(self):
    	#データベース
    	db = pd.read_csv("data.csv")
    	params = pd.read_csv("params.csv")
    	#入力されたデータ(値は0~5の6段階評価)
    	user_data = pd.DataFrame([[int(self.get_argument("q1")),
    	int(self.get_argument("q2")),int(self.get_argument("q3")),int(self.get_argument("q4")),int(self.get_argument("q5")),int(self.get_argument("q6")),int(self.get_argument("q7")),int(self.get_argument("q8")),int(self.get_argument("q9")),int(self.get_argument("q10")),int(self.get_argument("q11")),int(self.get_argument("q12")),int(self.get_argument("q13")),int(self.get_argument("q14")),int(self.get_argument("q15")),int(self.get_argument("q16")),"偏差値"]], 
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
    	print(data)
    	ranking = data.rank(ascending=False,method='max')
    	print(ranking)
    	#順位
    	rank = ranking.iat[len(data)-1,16].astype(int)
    	#受験人口
    	every = len(data)
    	self.render("result.html", value=value,rank=rank ,every=every)

application = tornado.web.Application([
    (r"/", MainHandler),
    (r"/result", MainHandler),
    ],
    template_path=os.path.join(os.getcwd(),  "templates"),
    static_path=os.path.join(os.getcwd(),  "static"),
)

if __name__ == "__main__":
    application.listen(8888)
    print("Server is up ...")
    tornado.ioloop.IOLoop.instance().start()