#连接数据库
library(RODBC)
library(ggplot2)
library(stringr)
library(recharts)
con = odbcConnect("gym")
sqlTables(con)
sale <- sqlQuery(con,"select * from sale",Encoding("UTF-8"))
#as.character(sale$course_name)
#ggplot(data = sale,aes(x = course_name)) + geom_bar()

#对course_name列进行处理，合并相似的课程名称
course_pos <- as.data.frame(str_locate(sale$course_name,"课")) 
course <- str_sub(sale$course_name,end = course_pos[,1])
sale1 <- data.frame(sale,course=course,stringsAsFactors = F)
a <- sale1[is.na(sale1$course),"course_name"]
a <- as.character(a)
sale1[is.na(sale1$course),"course"] <- a


course_full <- as.data.frame(table(sale1$course))
colnames(course_full) = c("course","count")
course_full <- course_full[order(course_full[,2],decreasing = T ),]
#销售课程数量分布
#ggplot(data = course_full[1:10,],aes(x = "", y = count,fill = course)) + 
  geom_bar(stat = "identity",width=1) + 
  coord_polar(theta = "y")

#将销售人员和教练汇总
saleman <- as.data.frame(table(sale$saleman_name)) 
saleman <- saleman[order(saleman[,2],decreasing = T ),]
colnames(saleman) <- c("name","count")

coach <- as.data.frame(table(sale$coach_name)) 
coach <- coach[order(coach[,2],decreasing = T ),]
colnames(coach) <- c("name","count")

#画图
saleman_bar <- eBar(saleman[1:10,],xvar = ~name,~count,title = "销售人员")
coach_bar <- eBar(coach[1:10,],xvar = ~name,~count,title = "教练")
course <- ePie(course_full[1:10,],reset_radius = c(80,120),showL = F,title = "销售课程分布") 
sale_type <- ePie(as.data.frame(table(sale$sale_type)) ,reset_radius = c(80,120),showL = F,title = "销售类型") 

#销售金额汇总
breaks <- c(0,1000,2000,3000,5000,10000,50000,100000,300000)
amount <- cut(sale1$amount, breaks = breaks)
amount <- as.data.frame(table(amount))
amount <- amount[order(amount[,2],decreasing = T ),]
amount_pie <- ePie(amount,reset_radius = c(80,120),showL = F,title = "销售金额")

#有效时间汇总
breaks <- c(0,370,730,1200,1600,2000,5000,100000,20000)
period <- cut(sale1$duration, breaks = breaks)
period <- as.data.frame(table(period))
period <- period[order(period[,2],decreasing = T ),]
period_pie <- ePie(period,reset_radius = c(80,120),showL = F,title = "有效期")

#求课程平均价格
price <- tapply(sale$amount,sale1$course,median)

course & saleman_bar & coach_bar & sale_type & amount_pie &period_pie
