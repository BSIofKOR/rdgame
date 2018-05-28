
dplyr::function()

lotto1<-function(x){

  num1<-data.frame(table(luckynumber$X1))

  number1<-num1 %>% arrange(desc(Freq)) %>% head(x)

  sample(number1$Var1,1)

}


lotto2<-function(x){

  num2<-data.frame(table(luckynumber$X2))

  number2<-num2 %>% arrange(desc(Freq)) %>% head(x)

  sample(number2$Var1,1)

}


lotto3<-function(x){

  num3<-data.frame(table(luckynumber$X3))

  number3<-num3 %>% arrange(desc(Freq)) %>% head(x)

  sample(number3$Var1,1)

}

lotto4<-function(x){

  num4<-data.frame(table(luckynumber$X4))

  number4<-num4 %>% arrange(desc(Freq)) %>% head(x)

  sample(number4$Var1,1)

}



lotto5<-function(x){

  num5<-data.frame(table(luckynumber$X5))

  number5<-num5 %>% arrange(desc(Freq)) %>% head(x)

  sample(number5$Var1,1)

}


lotto6<-function(x){

  num6<-data.frame(table(luckynumber$X6))

  number6<-num6 %>% arrange(desc(Freq)) %>% head(x)

  sample(number6$Var1,1)

}


lotto7<-function(x){

  num7<-data.frame(table(luckynumber$Bonus))

  number7<-num7 %>% arrange(desc(Freq)) %>% head(x)

  sample(number7$Var1,1)

}

lotto<-function(x){
  a<- data.frame(lotto1(x), lotto2(x), lotto3(x), lotto4(x), lotto5(x), lotto6(x), lotto7(x))
  a
}


A<-function(x){
No1<-filter(luckynumber,No>= x)
select(No1, 2:8)
}




for (x in x:806) {
  n<-c(No1$"1",No1$"2",No1$"3",No1$"4"No1$"5",No1$"6")
  print(n)}

lotto_evenOdd<-function(x){
evenOdd<-function(x){
    if(x%%2==0){
      print("Even")
    }
  else {print("Odd")}
}


evenOdd(1)
