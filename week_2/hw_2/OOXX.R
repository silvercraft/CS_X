########################################################## 
# 加分作業：
# 當要完成的目標變複雜後，學習如何將複雜的問題拆解成一個一個小問題來解決
# 練習 R function 的使用

# OOXX 遊戲練習
# 1. 設計一個兩人的OOXX遊戲。
# 2. 遊戲玩家分為A、B。A 先手，使用的符號為'O'; B 後手，使用的符號為'X'
# 3. 遊戲一開始，請輸出以下遊戲提示，並且停留等待玩家A輸入

#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 
rou=0
tur=2
r1=c(0,0,0)
r2=c(0,0,0)
r3=c(0,0,0)
s=c("","","","","","","","","")
map=rbind(r1, r2, r3)
while(0<1){
  wturn=tur%%2
  print(paste("Round ",rou))
  if(wturn==0){
    print("Now is player A's turn!")
    m=readline("Player A input(1~9) :")
    if(m=="exit"){
      print("bye-bye")
      break
    }
    n=as.integer(m)
    if(n<1||n>9){
      print("Invalid input! Please re-enter!")
      next
    }
    if(s[n]=="O"||s[n]=="X"){
      print("This position is already occupied!")
      next
    }
  }else{
    print("Now is player B's turn!")
    m=readline("Player B input(1~9) :")
    if(m=="exit"){
      print("bye-bye")
      break
    }
    n=as.integer(m)
    if(n<1||n>9){
      print("Invalid input! Please re-enter!")
      next
    }
    if(s[n]=="O"||s[n]=="X"){
      print("This position is already occupied!")
      next
    }
  }
  
  for(i in 1:n/4+1){
    for(j in 1:n%%4){
      if(wturn==0){
        map[i,j]=1
        s[n]="O"
      }else{
        map[i,j]=2
        s[n]="X"
      }
    }
  }
  print(paste(s[1],"|",s[2],"|",s[3]))
  print("_____")
  print(paste(s[4],"|",s[5],"|",s[6]))
  print("_____")
  print(paste(s[7],"|",s[8],"|",s[9]))
  if(s[1]=="O"&&s[2]=="O"&&s[3]=="O"){
    print("A wins!")
    break
  }else if(s[4]=="O"&&s[5]=="O"&&s[6]=="O"){
    print("A wins!")
    break
  }else if(s[7]=="O"&&s[8]=="O"&&s[9]=="O"){
    print("A wins!")
    break
  }else if(s[1]=="O"&&s[5]=="O"&&s[9]=="O"){
    print("A wins!")
    break
  }else if(s[3]=="O"&&s[5]=="O"&&s[7]=="O"){
    print("A wins!")
    break
  }else if(s[1]=="O"&&s[4]=="O"&&s[7]=="O"){
    print("A wins!")
    break
  }else if(s[2]=="O"&&s[5]=="O"&&s[8]=="O"){
    print("A wins!")
    break
  }else if(s[3]=="O"&&s[6]=="O"&&s[9]=="O"){
    print("A wins!")
    break
  }else if(s[1]=="X"&&s[5]=="X"&&s[9]=="X"){
    print("B wins!")
    break
  }else if(s[3]=="X"&&s[5]=="X"&&s[7]=="X"){
    print("B wins!")
    break
  }else if(s[1]=="X"&&s[4]=="X"&&s[7]=="X"){
    print("B wins!")
    break
  }else if(s[2]=="X"&&s[5]=="X"&&s[8]=="X"){
    print("B wins!")
  }else if(s[3]=="X"&&s[6]=="X"&&s[9]=="X"){
    print("B wins!")
    break
  }else if(s[4]=="X"&&s[5]=="X"&&s[6]=="X"){
    print("A wins!")
    break
  }else if(s[7]=="X"&&s[8]=="X"&&s[9]=="X"){
    print("A wins!")
    break
  }else if(s[1]=="X"&&s[2]=="X"&&s[3]=="X"){
    print("A wins!")
    break
  }else{
    print("next round!")
  }
  rou=rou+1
  tur=tur+1
  if(rou==9){
    print("End in a draw!")
    break
  }
}




# 4. 玩家們可以輸入的數字範圍為 1~9，依序對應九宮格的九格位置。
#    如果輸入錯誤，請抓錯！輸出以下遊戲提示。

#    Invalid input! Please re-enter! 
#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 

# 5. 待玩家正確輸入完後，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且等待切換到另外一位玩家等待輸入。
#    * 提醒，記得增加'Round'次數，以及切換使用者

#    O| | 
#    _____
#     | | 
#    _____
#     | | 
#    **************
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 6. 當玩家輸入的位置之前已經有'O'或'X'時，請輸出以下遊戲提示。

#    This position is already occupied!
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 7. 當使用者輸入'exit'時，結束遊戲並印出以下遊戲提示 

#    Bye-Bye!!

# 8. 判斷遊戲結束！當三個直排、橫排、或者斜排時，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且輸出勝利的玩家。

#    O|X|O
#    _____
#    X|O|X
#    _____
#    O| | 
#    **************
#    Player A wins!!! 
#

# 9. 當空格皆被填滿且無玩家獲勝時，請輸出以下遊戲提示(當時的遊戲圖形狀況)以及和局遊戲提示。

#   O|O|X
#   _____
#   X|X|O
#   _____
#   O|X|O
#   **************
#   End in a draw!!! 