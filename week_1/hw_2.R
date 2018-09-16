# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)
que=sample(0:9,4)
print(que)
counter=0
a = 0
while(a != 4){
  for(i in 1:4){
    ans[i]=readline("enter DIFFERENT numbers: ")
  }
  a=0
  b=0
  for(i in 1:4){
    
    if(as.integer(ans[i])==que[i]){
      a=a+1
    } 
    for(j in 1:4){
      if(as.integer(ans[i])==que[j]&&as.integer(ans[i])!=que[i]){
        b=b+1
      }
    }
  }
  print(paste(a,"A",b,"B"))
  counter = counter+1
}
print(paste("You've guessed ",counter," times."))

