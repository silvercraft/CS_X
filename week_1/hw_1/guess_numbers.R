# �q�Ʀr�C��
# �򥻥\��
# 1. �мg�@�ӥ�"�q���H������"���P�Ʀr���|���(1A2B�C��)
# 2. ���a�i"����"�q�q���Ҳ��ͪ��Ʀr�A�ô��ܲq�������G(EX:1A2B)
# 3. �@���q��A�t�Υi�۰ʭp�⪱�a�q��������

# �B�~�\��G�C�����a��J���|�ӼƦr��A�ˬd���a����J�O�_���T(���~�ˬd)
que=sample(0:9,4)
print(que)
counter=0
ans=c(-1:-1)
a = 0
while(a != 4){
  i = 1
  while(i < 5){
    ans[i]=readline("enter DIFFERENT numbers: ")
    if(i>1){
      for(j in 1:(i-1)){
        if(as.integer(ans[j])==as.integer(ans[i])){
          print("reenter the number")
          i=i-1
          break
        }
      }
    }
    i<-i+1
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
