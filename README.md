# zhongmin1209
data #R语言时间序列
#三阶指数平滑http://www.cnblogs.com/sylvanas2012/p/4328861.html#
w<-read.csv("Power.csv",header=T)#读取数据
library(robustbase)#稳健统计;
