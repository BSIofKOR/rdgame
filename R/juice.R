

library(dplyr)


lotto1<-function(){

  num1<-data.frame(table(luckynumber$`1`))

  number1<-num1 %>% arrange(desc(Freq)) %>% head(15)

  sample(number1$Var1,1)

}


lotto2<-function(){

  num2<-data.frame(table(luckynumber$`2`))

  number2<-num2 %>% arrange(desc(Freq)) %>% head(15)

  sample(number2$Var1,1)

}


lotto3<-function(){

  num3<-data.frame(table(luckynumber$`3`))

  number3<-num3 %>% arrange(desc(Freq)) %>% head(15)

  sample(number3$Var1,1)

}

lotto4<-function(){

  num4<-data.frame(table(luckynumber$`4`))

  number4<-num4 %>% arrange(desc(Freq)) %>% head(15)

  sample(number4$Var1,1)

}



lotto5<-function(){

  num5<-data.frame(table(luckynumber$`5`))

  number5<-num5 %>% arrange(desc(Freq)) %>% head(15)

  sample(number5$Var1,1)

}


lotto6<-function(){

  num6<-data.frame(table(luckynumber$`6`))

  number6<-num6 %>% arrange(desc(Freq)) %>% head(15)

  sample(number6$Var1,1)

}


lotto7<-function(){

  num7<-data.frame(table(luckynumber$`Bonus`))

  number7<-num7 %>% arrange(desc(Freq)) %>% head(15)

  sample(number7$Var1,1)

}

lotto<-function(){
  a<- data.frame(lotto1(), lotto2(), lotto3(), lotto4(), lotto5(), lotto6(), lotto7())
  a
}
}
