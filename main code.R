###加载需要的包
install.packages('mvstats')
library(mvstats)    #因子分析(主成分法求解)mvstats包，需要在本地加载

setwd("/Users")    #设置工作路径
mydata<-read.csv("data.csv",fileEncoding = "GBK")   #读入原始数据（GBK指读入文件的编码方式）
n=dim(mydata)[1]                                    #查看样本量
summary(mydata) 	                                  #查看数据基本描述，每一条样本对应一段路程

###描述性分析
#mac电脑需要自己设定字体，此处设置为黑体
par(family="STXihei")
#设定与ppt主色调相同的颜色
col_ppt=rgb(red=127,green=127,blue=127,max=255)     #灰色  
#行驶时长－行驶里程散点图
plot(mydata$行驶时长,mydata$行驶里程,col=col_ppt,xlab="行驶时长（小时）",ylab="行驶里程（千米）")
#平均时速－平均转速散点图
plot(mydata$转速平均值,mydata$平均时速,col=col_ppt,xlab="平均时速（千米/小时)",ylab="平均引擎转速（转/分钟）")

#出行习惯柱状图
a1<-length(which(mydata$早晚高峰>0))/n   #早晚高峰路程占比
a2<-length(which(mydata$深夜出行>0))/n   #深夜出行路程占比
a3<-length(which(mydata$疲劳驾驶>0))/n   #疲劳驾驶路程占比
a<-cbind(a1,a2,a3)                       #获得出行习惯数据
r=barplot(a,beside=T,col=col_ppt,names=c("早晚高峰","深夜出行","疲劳驾驶"),ylim=c(0,0.5),xlab="出行习惯",ylab="比例")
mylab=paste(round(100*a,1),"%",sep="");
text(r,a+0.05,mylab,pos=1)   #给柱状图添加数值标注

#平均时速直方图
hist(mydata$平均时速,xlab="平均时速（千米/小时)",ylab="频数",main="",col=col_ppt)
#查看平均时速最高和最低的两条观测
mydata[mydata$平均时速==max(mydata$平均时速),]
mydata[mydata$平均时速==min(mydata$平均时速),]

par(mfrow=c(1,2))   #画1*2的图

#行驶里程、行驶时长的箱线图
boxplot(log(mydata$行驶里程)~mydata$车号,col=col_ppt,xlab="车号",ylab="对数行驶里程")
boxplot(log(mydata$行驶时长)~mydata$车号,col=col_ppt,xlab="车号",ylab="对数行驶时长")

#平均时速、时速标准差的箱线图
boxplot(mydata$平均时速~mydata$车号,col=col_ppt,xlab="车号",ylab="平均时速（千米/小时）")
boxplot(mydata$时速标准差~mydata$车号,col=col_ppt,xlab="车号",ylab="时速标准差（千米/小时）") 

#转速平均值、平稳性的箱线图
boxplot(mydata$转速平均值~mydata$车号,col=col_ppt,xlab="车号",ylab="转速平均值（转/分钟）")  
boxplot(mydata$平稳性~mydata$车号,col=col_ppt,xlab="车号",ylab="驾驶平稳性")  

mydata0<-mydata[,-15]                               #去除最后一列车号，用于因子分析
###因子分析（利用主成分法）
#确定保留因子的个数
par(mfrow=c(1,1))
pca.fit=princomp(mydata0)                      #使用princomp函数（使用特征分解办法求解主成分）
screeplot(pca.fit,type="lines")                #绘制碎石图，判断出保留3个因子
fac.out=factpc(mydata0,3,rotation="varimax")   #用主成分法做因子分析，提取三个公因子，方差最大旋转
fac.out$Vars                                   #输出三个公因子的累积方差贡献率	
cbind(round(fac.out$loadings,3),round(fac.out$common,3))   #输出三个公因子的载荷矩阵和共性方差，解释公因子的含义
#经观察结果，得知第一个公因子为行驶速度，第二个公因子为行驶强度，第三个为出行习惯

###聚类分析
fac.score=fac.out$scores              #获得每段路程的因子得分，每行是一个行程，每列是该行程在相应因子上的得分
colnames(fac.score)<-c("行驶速度","行驶强度","出行习惯")
set.seed(2)                           #固定随机种子
km.out<-kmeans(fac.score,7,nstart=20) #按7类进行聚类
km.out$centers                        #查看7类的类中心
km.out$size                           #查看每类样本量
km.out$cluster                        #查看每条观测所属的类别
#路段举例
mydata[which(km.out$cluster==3),][1,] #类别三：早晚高峰时段、较拥堵的缓慢驾驶行为
mydata[which(km.out$cluster==4),][2,] #类别四：深夜速度较快的驾驶行为
mydata[which(km.out$cluster==6),][3,] #类别六：白天非高峰时段、速度较快的长途驾驶行为

###驾驶习惯分析
temp=table(mydata$车号,km.out$cluster)           #查看8位驾驶人行为分布
#驾驶行为堆砌条形图
temp.ratio<-apply(temp,1,function(x) x/sum(x))   #转化成比例数据，使结果更直观
par(xpd=T,mar=par()$mar+c(0,0,0,2))              #xpd=T允许图例出现在图形边缘空白处，c(0,0,0,2)将右侧边缘空白位置增大
barplot(temp.ratio,horiz=T,names=c(1:8),xlab="比例",ylab="车号",border=NA,
        col=c("#1C86EE","#FF7F00","#B5B5B5","#FFA500","gray61","#4169E1","gold2"),
        legend.text=paste("类",c(1:7),sep=""),args.legend=list(x="right",bty="n",inset=-0.2))
#args.legend是图例属性参数，其中x="right"令图例处于图形右侧，bty="n"去除图例边框，inset用于调整图例位置

