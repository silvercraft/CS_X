### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
n=iris

# 使用dim(), 回傳iris的列數與欄數
dim(n)

# 使用head() 回傳iris的前六列
head(n)

# 使用tail() 回傳iris的後六列
tail(n)

# 使用str() 
str(n)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(n)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)

for(i in 1:9){
  for(j in 1:9){
    print(paste(i,"*",j,"=",i*j))
  }
}

########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
num = sample(10:100,size=10)


# 查看nums

print(num)
# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。

for(i in 51:100){
  if(i %% 2 == 0){
    print(paste("偶數且大於50: 數字",i))
  }
    
  if(i == 66){
    print("太66666666666了")
    break
  }
    
}






########################################################### Task 4

# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年

x=readline("enter a year number")
if((as.integer(x)%%4==0 && as.integer(x)%%100 != 0)||as.integer(x)%%400==0){
  print("輸入了閏年")
} else {
  print("輸入了平年")
}








