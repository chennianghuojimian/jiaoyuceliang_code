# author:"孙艺恺"
# date:"20231004"

# install.packages("CTT")
# install.packages("ggalt")
# install.packages("ggplot2")
# install.packages("psych")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("mirt")

library(dplyr)
library(readxl)
library(ggplot2)
library(ggalt)
library(psych)
library(CTT)
library(mirt)


# CTT包导入顺序需要在psych包后面  
# 因为两者都有polyserial函数，在后面的会覆盖前面的，
# 我们要用CTT里面的polyserial函数

# 将excel文件转化为编码为utf-8的csv文件
setwd("D:/研究生任务/杂七杂八的文件/上课ppt/教育测量/作业") # 转移到存储数据的文件夹
data <- read_excel("小分表-某市高三摸底考试（数学）脱敏.xlsx")
write.csv(data, "小分表-某市高三摸底考试（数学）脱敏.csv", fileEncoding = "UTF-8")
data <- read.csv("小分表-某市高三摸底考试（数学）脱敏.csv", encoding = "UTF-8")
data <- data[-1, -1] # 去掉第一行与第一列无用数据
rownames(data) <- 1:nrow(data) # 重置索引
data$班级 <- gsub("[班]", "", data$班级) # 将班级名称进行统一
# 一.代表单选 二.代表多选 三.代表填空 四.代表大题 ...代表考生答案（只在单项选择与
# 多项选择中出现）
print(nrow(data))
# 将分值转化为数值类型
nameQuestion <- c("总分", "客观题", "主观题","一.1", "一.2", "一.3", "一.4", "一.5", 
                  "一.6", "一.7", "一.8", "二.9", "二.10", "二.11","二.12", "三.13", 
                  "三.14", "三.15", "三.16.1", "三.16.2", "四.17.1","四.17.2", "四.18.1", 
                  "四.18.2", "四.19.1", "四.19.2", "四.20.1", "四.20.2",  "四.21.1", "四.21.2", 
                  "四.22.1", "四.22.2")
for(i in nameQuestion){
  data[i] <- apply(data[i], c(1, 2), as.numeric)
}


# dataTrueSelect用于存储选择题正确答案
dataTrueSelect <- data.frame(  
  题目 = c("一.1", "一.2", "一.3", "一.4", "一.5", "一.6", 
         "一.7", "一.8", "二.9", "二.10", "二.11", "二.12"),
  正确选项 = c("B", "A", "A", "C", "D", "B", "A", "C", "BD", "BC", "BD", "AC")
)

# datamarks用于存储题目的分值 
datamarks <- data.frame(
  题目 = c("一.1", "一.2", "一.3", "一.4", "一.5", "一.6", "一.7",
              "一.8", "二.9", "二.10", "二.11", "二.12", "三.13", "三.14", 
              "三.15", "三.16.1", "三.16.2", "四.17.1", "四.17.2", "四.18.1",
              "四.18.2", "四.19.1", "四.19.2", "四.20.1", "四.20.2", "四.21.1", 
              "四.21.2", "四.22.1", "四.22.2"),
  分值 = c(5, 5, 5, 5, 5, 5, 5,
              5, 5, 5, 5 ,5, 5, 5,
              5, 2, 3, 5, 5, 6,
              6, 4, 8, 6, 6, 3,
              9, 3, 9)
)
datamarksall <- data.frame(
  题目 = c("一.1", "一.2", "一.3", "一.4", "一.5", "一.6", "一.7", "一.8", 
         "二.9", "二.10", "二.11", "二.12", "三.13", "三.14", "三.15", 
         "三.16", "四.17", "四.18", "四.19", "四.20", "四.21", "四.22.2"),
  分值 = c(5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5 ,5, 5, 5,
         5, 10, 12, 12, 12, 12, 12)
)

# dataSelection用于存储考生选择题答案
dataSelection <- data[c("考号",
                        "...12", "...14", "...16", "...18", "...20",
                        "...22", "...24", "...26", "...28", "...30",
                        "...32", "...34")]
colnames(dataSelection) <- c("考号", "一.1", "一.2", "一.3", "一.4", 
                             "一.5", "一.6", "一.7", "一.8", "二.9", 
                             "二.10", "二.11", "二.12")

# 用来筛选有效作答试卷,单选题超过百分之八十的答案为同一选项则认定为无效作答试卷
stuError = c()
for(i in 1:10357){
  if(sum(dataSelection[i,2:9]=="A")>=6){
    stuError = c(stuError, dataSelection[i, 1])
  }else if(sum(dataSelection[i,2:9]=="B")>=6){
    stuError = c(stuError, dataSelection[i, 1])
  }else if(sum(dataSelection[i,2:9]=="C")>=6){
    stuError = c(stuError, dataSelection[i, 1])
  }else if(sum(dataSelection[i,2:9]=="D")>=6){
    stuError = c(stuError, dataSelection[i, 1])
  }
}
# 筛选试卷数据
for(i in stuError){
  data <- subset(data, 考号!=i)
}
rownames(data) <- 1:nrow(data) # 筛选后重置索引

# 试卷筛选后，重新存储存储考生选择题答案
dataSelection <- data[c("考号",
                        "...12", "...14", "...16", "...18", "...20",
                        "...22", "...24", "...26", "...28", "...30",
                        "...32", "...34")]
colnames(dataSelection) <- c("考号", "一.1", "一.2", "一.3", "一.4", 
                             "一.5", "一.6", "一.7", "一.8", "二.9", 
                             "二.10", "二.11", "二.12")

# dataScore用于存储每道题目的得分
dataScore <- data[c("考号", "学号", "学校", "班级", "总分", "客观题", "主观题",  
                    "联考排名", "学校排名", "班级排名", "一.1", "一.2", "一.3", 
                    "一.4", "一.5", "一.6", "一.7", "一.8", "二.9", "二.10", "二.11",
                    "二.12", "三.13", "三.14", "三.15", "三.16.1", "三.16.2", "四.17.1",
                    "四.17.2", "四.18.1", "四.18.2", "四.19.1", "四.19.2", "四.20.1",
                    "四.20.2", "四.21.1", "四.21.2", "四.22.1", "四.22.2" )]

# dataScoreDes用于描述性统计
dataScoreDes <- data[c("一.1", "一.2", "一.3", "一.4", "一.5", "一.6", "一.7", 
                       "一.8", "二.9", "二.10", "二.11", "二.12", "三.13", "三.14",
                       "三.15", "三.16.1", "三.16.2", "四.17.1", "四.17.2", "四.18.1",
                       "四.18.2", "四.19.1", "四.19.2", "四.20.1", "四.20.2", 
                       "四.21.1", "四.21.2", "四.22.1", "四.22.2", "总分", "客观题", "主观题")]

# 将大题小分数据转化每题总得分
三.16 <- rowSums(dataScoreDes[, 16:17])
四.17 <- rowSums(dataScoreDes[, 18:19])
四.18 <- rowSums(dataScoreDes[, 20:21])
四.19 <- rowSums(dataScoreDes[, 22:23])
四.20 <- rowSums(dataScoreDes[, 24:25])
四.21 <- rowSums(dataScoreDes[, 26:27])
四.22 <- rowSums(dataScoreDes[, 28:29])
sumScoreData <- cbind(dataScoreDes[, 1:15], 三.16, 四.17, 
                      四.18, 四.19, 四.20, 四.21, 四.22, dataScoreDes[, 30:32])
write.csv(sumScoreData, "Score.csv", fileEncoding = "UTF-8")

# 对分数进行描述性统计并将统计结果输出为csv文件
write.csv(describe(sumScoreData), file="descriptiveScore.csv")
summary(sumScoreData) # 查看分数最小值、下四分之一分位数、中位数、上四分之一分位数、最大值

# 计算班级数量
sumSchool = 0
for(i in unique(dataScore$学校)){
  sumSchool = sumSchool + length(unique(dataScore[dataScore$学校==i, ]$班级))
}


distractorAnalysis(dataSelection[,2:9], c("B", "A", "A", "C", "D", "B", "A", "C"))

# 计算每道大题的难度系数
item_diffall <- colMeans(sumScoreData[,1:22]) # 计算每道题目均值
# datamarksall记录每道大题的分值
for(i in 1:nrow(datamarksall)){
  item_diffall[i] <- 1 - item_diffall[i]/datamarksall[i, 2]
}
print(item_diffall)


# 计算每道大题(拆分为小题)的难度系数
item_diff <- colMeans(dataScore[,11:39]) # 计算每道题目均值
# datamarks记录每道大题(拆分为小题)的分值
for(i in 1:nrow(datamarks)){
  item_diff[i] <- 1 - item_diff[i]/datamarks[i, 2]
}
print(item_diff)

# 计算项目鉴别指数
quantile(dataScore[,5], c(0.27, 0.73)) # 计算可得上27分数界限为78，下百分之27分数界限为42
group1 <- sumScoreData[sumScoreData[,23]>=78, 1:22]
group2 <- sumScoreData[sumScoreData[,23]<=42, 1:22]
itemdisall <- colMeans(group1)-colMeans(group2)
for(i in 1:nrow(datamarksall)){
  itemdisall[i] <- itemdisall[i]/datamarksall[i,2]
}
print(itemdisall)


# 计算项目鉴别指数
quantile(dataScore[,5], c(0.27, 0.73)) # 计算可得上27分数界限为78，下百分之27分数界限为42
group1 <- dataScore[dataScore[,5]>=78, 11:39]
group2 <- dataScore[dataScore[,5]<=42, 11:39]
itemdis <- colMeans(group1)-colMeans(group2)
for(i in 1:nrow(datamarks)){
  itemdis[i] <- itemdis[i]/datamarks[i,2]
}
print(itemdis)

# 查看问卷的Cronbach′ s alpha系数
alpha(dataScore[,11:39]) # 总体
alpha(dataScore[,11:27]) # 客观题
alpha(dataScore[,28:39]) # 主观题
alpha(cbind(dataScore[,19:22], dataScore[28:39])) # 多选题与多级计分题目

# 绘制ICC特征曲线
empirical_plot(dataScore[,11:39], c(9:12), smooth = TRUE) # 绘制多选题ICC曲线
empirical_plot(dataScore[,11:39], c(18:29), smooth = TRUE) # 绘制解答题ICC曲线
empirical_plot(dataScore[,11:39], c(9), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(10), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(11), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(12), smooth = TRUE)
# 解答题
empirical_plot(dataScore[,11:39], c(18), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(19), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(20), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(21), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(22), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(23), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(24), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(25), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(26), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(27), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(28), smooth = TRUE)
empirical_plot(dataScore[,11:39], c(29), smooth = TRUE)
# IRT分析

# 单选题
level1 <- as.data.frame(cbind(t1 <- ifelse(dataScore$一.1 == 5,1,0),
                              t2 <- ifelse(dataScore$一.2 == 5,1,0),
                              t3 <- ifelse(dataScore$一.3 == 5,1,0),
                              t4 <- ifelse(dataScore$一.4 == 5,1,0),
                              t5 <- ifelse(dataScore$一.5 == 5,1,0),
                              t6 <- ifelse(dataScore$一.6 == 5,1,0),
                              t7 <- ifelse(dataScore$一.7 == 5,1,0),
                              t8 <- ifelse(dataScore$一.8 == 5,1,0)))

# 多选题
level2 <- as.data.frame(cbind(t9 <- ifelse(dataScore$二.9 == 5,"p3",
                                           ifelse(dataScore$二.9 == 2, "p2", "p1")),
                              t10 <- ifelse(dataScore$二.10 == 5,"p3",
                                            ifelse(dataScore$二.10 == 2, "p2", "p1")),
                              t11 <- ifelse(dataScore$二.11 == 5,"p3",
                                            ifelse(dataScore$二.11 == 2, "p2", "p1")),
                              t12 <- ifelse(dataScore$二.12 == 5,"p3",
                                            ifelse(dataScore$二.12 == 2, "p2", "p1"))))

level2 <- as.data.frame(cbind(t9 <- ifelse(dataScore$二.9 == 5, 3,
                                           ifelse(dataScore$二.9 == 2, 2, 1)),
                              t10 <- ifelse(dataScore$二.10 == 5, 3,
                                            ifelse(dataScore$二.10 == 2, 2, 1)),
                              t11 <- ifelse(dataScore$二.11 == 5,3,
                                            ifelse(dataScore$二.11 == 2, 2, 1)),
                              t12 <- ifelse(dataScore$二.12 == 5, 3,
                                            ifelse(dataScore$二.12 == 2, 2, 1))))

#填空
level3 <- as.data.frame(cbind(t13 <- ifelse(dataScore$三.13 == 5,1,0),
                              t14 <- ifelse(dataScore$三.14 == 5,1,0),
                              t15 <- ifelse(dataScore$三.15 == 5,1,0),
                              t16 <- ifelse(dataScore$三.16.1 == 2,1,0),
                              t17 <- ifelse(dataScore$三.16.2 == 3,1,0)))

# 解答题
level4 <- as.data.frame(cbind(t18 <- ifelse(dataScore$四.17.1 >= 4,"p3",
                                           ifelse(dataScore$四.17.1 < 2, "p1", "p2")),
                              t19 <- ifelse(dataScore$四.17.2 >= 4, "p3",
                                            ifelse(dataScore$四.17.2 <2, "p1", "p2")),
                              t20 <- ifelse(dataScore$四.18.1 >= 5, "p3",
                                            ifelse(dataScore$四.18.1 <= 2, "p1", "p2")),
                              t21 <- ifelse(dataScore$四.18.2 >= 5, "p3",
                                            ifelse(dataScore$四.18.2 <= 2, "p1", "p2")),
                              t22 <- ifelse(dataScore$四.19.1 >= 5, "p3",
                                            ifelse(dataScore$四.19.1 <= 2, "p1", "p2")),
                              t23 <- ifelse(dataScore$四.19.2 >= 5, "p3",
                                            ifelse(dataScore$四.19.2 <= 2, "p1", "p2")),
                              t24 <- ifelse(dataScore$四.20.1 >= 5, "p3",
                                            ifelse(dataScore$四.20.1 <= 2, "p1", "p2")),
                              t25 <- ifelse(dataScore$四.20.2 >= 5, "p3",
                                            ifelse(dataScore$四.20.2 <= 2, "p1", "p2")),
                              t26 <- ifelse(dataScore$四.21.1 >= 2, "p3", "p1"),
                              t27 <- ifelse(dataScore$四.21.2 >= 7, "p3",
                                            ifelse(dataScore$四.21.2 <= 3, "p1", "p2")),
                              t28 <- ifelse(dataScore$四.22.1 >= 2, "p3", "p1"),
                              t29 <- ifelse(dataScore$四.22.2 >= 7, "p3",
                                            ifelse(dataScore$四.22.2 <= 3, "p1", "p2"))
                              )
                        )

level4 <- as.data.frame(cbind(t18 <- ifelse(dataScore$四.17.1 >= 4, 3,
                                            ifelse(dataScore$四.17.1 < 2, 1, 2)),
                              t19 <- ifelse(dataScore$四.17.2 >= 4, 3,
                                            ifelse(dataScore$四.17.2 <2, 1, 2)),
                              t20 <- ifelse(dataScore$四.18.1 >= 5, 3,
                                            ifelse(dataScore$四.18.1 <= 2, 1, 2)),
                              t21 <- ifelse(dataScore$四.18.2 >= 5, 3,
                                            ifelse(dataScore$四.18.2 <= 2, 1, 2)),
                              t22 <- ifelse(dataScore$四.19.1 >= 5, 4,
                                            ifelse(dataScore$四.19.1 <= 2, 1, 3)),
                              t23 <- ifelse(dataScore$四.19.2 >= 6, 3,
                                            ifelse(dataScore$四.19.2 <= 2, 1, 2)),
                              t24 <- ifelse(dataScore$四.20.1 >= 5, 3,
                                            ifelse(dataScore$四.20.1 <= 2, 1, 2)),
                              t25 <- ifelse(dataScore$四.20.2 >= 5, 3,
                                            ifelse(dataScore$四.20.2 <= 2, 1, 2)),
                              t26 <- ifelse(dataScore$四.21.1 >= 2, 3, 1),
                              t27 <- ifelse(dataScore$四.21.2 >= 7, 3,
                                            ifelse(dataScore$四.21.2 <= 3, 1, 2)),
                              t28 <- ifelse(dataScore$四.22.1 >= 2, 3, 1),
                              t29 <- ifelse(dataScore$四.22.2 >= 7, 3,
                                            ifelse(dataScore$四.22.2 <= 3, 1, 2))
)
)

# 将四种题型重新组为一张试卷
grade_level <- cbind(level1,level2,level3,level4)
colnames(grade_level) <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9",
                           "t10","t11","t12","t13","t14","t15","t16","t17","t18","t19",
                           "t20","t21","t22","t23","t24","t25","t26","t27","t28","t29")
model1 <- mirt(grade_level,model = 1,itemtype = 'gpcm')
summary(model1)
coef(model1,IRTpars=TRUE)

# 每个人的能力值
theta1 <- fscores(model1)
theta2 <- as.data.frame(theta1)

# IRT拟合曲线
itemplot(model1,12)
itemplot(model1,18)
itemplot(model1,19)
itemplot(model1,20)
itemplot(model1,21)
itemplot(model1,22)
itemplot(model1,23)
itemplot(model1,24)
itemplot(model1,25)
itemplot(model1,26)
itemplot(model1,27)
itemplot(model1,28)
itemplot(model1,29)
# 绘制信息函数图
itemplot(model1,12,type = "info")
itemplot(model1,18,type = "info")
itemplot(model1,19,type = "info")
itemplot(model1,20,type = "info")
itemplot(model1,21,type = "info")
itemplot(model1,22,type = "info")
itemplot(model1,23,type = "info")
itemplot(model1,24,type = "info")
itemplot(model1,25,type = "info")
itemplot(model1,26,type = "info")
itemplot(model1,27,type = "info")
itemplot(model1,28,type = "info")
itemplot(model1,29,type = "info")
# 解答题目不做拆分拟合IRT
level4_all <- as.data.frame(cbind(t18 <- ifelse(sumScoreData$四.17 >= 8, 3,
                                            ifelse(sumScoreData$四.17 <= 3, 1, 2)),
                              t19 <- ifelse(sumScoreData$四.18 >= 9, 3,
                                            ifelse(sumScoreData$四.18 <= 3, 1, 2)),
                              t20 <- ifelse(sumScoreData$四.19 >= 9, 3,
                                            ifelse(sumScoreData$四.19 <= 3, 1, 2)),
                              t21 <- ifelse(sumScoreData$四.20 >= 9, 3,
                                            ifelse(sumScoreData$四.20 <= 3, 1, 2)),
                              t22 <- ifelse(sumScoreData$四.21 >= 9, 3,
                                            ifelse(sumScoreData$四.21 <= 3, 1, 2)),
                              t23 <- ifelse(sumScoreData$四.22 >= 9, 3,
                                            ifelse(sumScoreData$四.22 <= 3, 1, 2))
                              )
                            )
grade_level <- cbind(level1,level2,level3,level4_all)
colnames(grade_level) <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9",
                           "t10","t11","t12","t13","t14","t15","t16","t17","t18","t19",
                           "t20","t21","t22","t23")
model1 <- mirt(grade_level,model = 1,itemtype = 'gpcm')
# 展示拟合结果
summary(model1)
coef(model1,IRTpars=TRUE)

# 每个人的能力值
theta1 <- fscores(model1)
theta2 <- as.data.frame(theta1)

# IRT拟合曲线
itemplot(model1,18)
itemplot(model1,19)
itemplot(model1,20)
itemplot(model1,21)
itemplot(model1,22)
itemplot(model1,23)
# 绘制信息函数图
itemplot(model1,18,type = "info")
itemplot(model1,19,type = "info")
itemplot(model1,20,type = "info")
itemplot(model1,21,type = "info")
itemplot(model1,22,type = "info")
itemplot(model1,23,type = "info")
# 选项筛选
# 创建一个行数为学生数、16列的空数据框
dataSelection_ABCD <- data.frame(matrix(ncol = 16, nrow = nrow(dataSelection)))

# 可以选择给每一列起一个适当的名称
colnames(dataSelection_ABCD) <- c("二.9.A", "二.9.B", "二.9.C", "二.9.D", 
                                  "二.10.A", "二.10.B", "二.10.C", "二.10.D", 
                                  "二.11.A", "二.11.B", "二.11.C", "二.11.D",
                                  "二.12.A", "二.12.B", "二.12.C", "二.12.D")

for(i in 1:nrow(dataSelection)){
  if(grepl("A", dataSelection[i,10])){
    dataSelection_ABCD[i, 1] <- 1
  }else{
    dataSelection_ABCD[i, 1] <- 0 
  }
  if(grepl("B", dataSelection[i,10])){
    dataSelection_ABCD[i, 2] <- 1
  }else{
    dataSelection_ABCD[i, 2] <- 0 
  }
  if(grepl("C", dataSelection[i,10])){
    dataSelection_ABCD[i, 3] <- 1
  }else{
    dataSelection_ABCD[i, 3] <- 0 
  }
  if(grepl("A", dataSelection[i,10])){
    dataSelection_ABCD[i, 4] <- 1
  }else{
    dataSelection_ABCD[i, 4] <- 0 
  }
  
  
  if(grepl("A", dataSelection[i,11])){
    dataSelection_ABCD[i, 5] <- 1
  }else{
    dataSelection_ABCD[i, 5] <- 0
  }
  
  if(grepl("B", dataSelection[i,11])){
    dataSelection_ABCD[i, 6] <- 1
  }else{
    dataSelection_ABCD[i, 6] <- 0
  }
  
  if(grepl("C", dataSelection[i,11])){
    dataSelection_ABCD[i, 7] <- 1
  }else{
    dataSelection_ABCD[i, 7] <- 0
  }
  
  if(grepl("D", dataSelection[i,11])){
    dataSelection_ABCD[i, 8] <- 1
  }else{
    dataSelection_ABCD[i, 8] <- 0
  }
  
  if(grepl("A", dataSelection[i,12])){
    dataSelection_ABCD[i, 9] <- 1
  }else{
    dataSelection_ABCD[i, 9] <- 0
  }
  if(grepl("B", dataSelection[i,12])){
    dataSelection_ABCD[i, 10] <- 1
  }else{
    dataSelection_ABCD[i, 10] <- 0
  }
  if(grepl("C", dataSelection[i,12])){
    dataSelection_ABCD[i, 11] <- 1
  }else{
    dataSelection_ABCD[i,11] <- 0
  }
  if(grepl("D", dataSelection[i,12])){
    dataSelection_ABCD[i, 12] <- 1
  }else{
    dataSelection_ABCD[i, 12] <- 0
  }
  
  if(grepl("A", dataSelection[i,13])){
    dataSelection_ABCD[i, 13] <- 1
  }else{
    dataSelection_ABCD[i, 13] <- 0
  }
  if(grepl("B", dataSelection[i,13])){
    dataSelection_ABCD[i, 14] <- 1
  }else{
    dataSelection_ABCD[i, 14] <- 0
  }
  if(grepl("C", dataSelection[i,13])){
    dataSelection_ABCD[i, 15] <- 1
  }else{
    dataSelection_ABCD[i, 15] <- 0
  }
  if(grepl("D", dataSelection[i,13])){
    dataSelection_ABCD[i, 16] <- 1
  }else{
    dataSelection_ABCD[i, 16] <- 0
  }
}
data_ABCD <- data.frame(colSums(dataSelection_ABCD) / nrow(dataSelection_ABCD))
colnames(data_ABCD) <- c("占比")
write.csv(data_ABCD,"data_ABCD.csv")

