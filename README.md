# zhongmin1209
data #R语言时间序列
#三阶指数平滑http://www.cnblogs.com/sylvanas2012/p/4328861.html
w<-read.csv("Power.csv",header=T)
library(robustbase)#稳健统计
#三阶指数平滑http://www.cnblogs.com/sylvanas2012/p/4328861.html
library(TSA)#时间序列包
library(forecast)#预测包
t1<-c(248160,247080,218400,121620,104940,186240,246240,264480,216240,135480,183360,370440,336720,337740,302460,219480,240180,375600,414060,474600,407100,266640,325260,401640,448380,398460,357180,304920,293760,494280,568980,688920,520980,314640,366420,437640,456360,402600,395580,326640,367020,671700,754680,813420,661560,390840,416160,518760,514800,347280,535800,368280,439440,704100,709620,848580,639660,376620,432480,523980
)
t2<-c(107440,99872,68056,73656,33952,26168,56424,59544,50128,42896,66832,72224,80796,91650,72750,75330,52170,38280,37410,39630,44340,26730,33690,87750,158250,110430,128160,43860,34230,34140,47850,92040,83010,59670,53790,156570,174540,188040,157500,91500,82080,121410,149280,175620,168510,112500,98250,168180,205860,201660,159870,136920,130320,179760,198510,236940,242640,172800,166410,247500
)
t3<-c(702400,625680,551520,514240,491760,680800,841760,949920,806800,561360,652080,652960,512560,745520,623600,561200,559440,747440,875200,947200,837040,598000,641200,737600,815840,710240,660720,676800,636000,774320,1012000,1193600,979680,690720,769440,917680,985280,811440,791520,715200,698400,1003200,1146320,1254560,1079760,752560,827840,1050160,1127840,782720,1308000,901968,964239,1324108,1472264,1708180,1445778,1002179,1122708,1339056
)
t4<-c(300900,318200,245300,195300,172600,338400,439800,450800,420200,242500,235100,331100,330800,327300,348400,304400,287500,531900,642300,719200,674700,452900,379100,453100,494900,477100,399500,430500,391800,697200,777800,899000,858600,464700,469300,744400,589800,523200,491100,453700,486500,817200,852900,903000,746500,414600,485800,606000,627200,429700,660200,505300,545000,874300,849200,984800,808400,479400,546900,653600
)
t5<-c(69840,61080,54510,52890,49020,55710,74430,86040,85770,59760,67050,75210,66480,85260,75120,74550,67650,93810,132540,163770,161190,83430,89610,102720,116250,86100,104430,99600,86550,101220,148890,196200,171000,93420,99120,105840,106980,90720,87330,87660,86010,135870,178170,205380,178200,103200,105630,117960,129060,100230,123420,119460,109920,154110,186390,215820,184650,101160,113010,124620
)
d1<-ts(t1,start=c(2011,1),frequency=12)
d2<-ts(t2,start=c(2011,1),frequency=12)
d3<-ts(t3,start=c(2011,1),frequency=12)
d4<-ts(t4,start=c(2011,1),frequency=12)
d5<-ts(t5,start=c(2011,1),frequency=12)
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
mP<- HoltWinters(d5,beta=FALSE)#时间序列集d5的HoltWinters模型建立
Pforecasts2 <-forecast.HoltWinters(mP, h=12)%预测12个月的值
Pforecasts2
par(mfrow=c(2,2))#R语言多图形
#par(mar=c(5, 4, 4, 2) + 0.1)
par(mai=c(0.5, 0.5,0.5, 0.3))#R语言边框设置
par(oma=c(1,1, 1, 1))

图1
plot(window(d1,start=c(2011,1)),ylab="电量",xlab="年份",main="办公建筑 A")
points(window(d1,start=c(2011,1)),pch=Month)

图2
plot(window(d2,start=c(2011,1)),ylab="电量",xlab="年份",main="办公建筑 B")
points(window(d2,start=c(2011,1)),pch=Month)
图3
plot(window(d3,start=c(2011,1)),ylab="电量",xlab="年份",main="办公建筑 C")
points(window(d3,start=c(2011,1)),pch=Month)
图4
plot(window(d4,start=c(2011,1)),ylab="电量",xlab="年份",main="办公建筑 D")
points(window(d4,start=c(2011,1)),pch=Month)
图5
plot(window(d5,start=c(2011,1)),ylab="电量",xlab="年份",main="办公建筑 E")
points(window(d5,start=c(2011,1)),pch=Month)

mP<- HoltWinters(d5 )
Pforecasts2 <-forecast.HoltWinters(mP, h=12)

Data<-ts(t1,start=c(2011,1),frequency=12)
plot(window(Data,start=c(2011,1)),ylab="电量",xlab="年份")

Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(Data,start=c(2011,1)),pch=Month)
#差分前acf图
acf(as.vector(Data),main="",xlab="滞后",lag.max=25)
#一次差分后acf和pacf
acf(as.vector(diff(Data)),main="",lag.max=25,xlab="滞后")
pacf(as.vector(diff(Data)),main="",lag.max=25,xlab="滞后")
eacf(as.vector(diff(Data)))
periodogram(Data)#一次差分后的周期图
#对数据进行拆分
 plot(stl(Data, "per"))
mP<- HoltWinters(Data,optim.start = c(alpha = 0.1, beta = 0.1, gamma = 0.5) )
