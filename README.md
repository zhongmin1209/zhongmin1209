# zhongmin1209
data #R语言时间序列
#三阶指数平滑http://www.cnblogs.com/sylvanas2012/p/4328861.html
w<-read.csv("Power.csv",header=T)
library(robustbase)#稳健统计
#三阶指数平滑http://www.cnblogs.com/sylvanas2012/p/4328861.html
library(TSA)#时间序列包
library(forecast)#预测包
