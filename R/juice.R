

<<<<<<< HEAD
library(dplyr)


lotto1<-function(){

  num1<-data.frame(table(luckynumber$`1`))

  number1<-num1 %>% arrange(desc(Freq)) %>% head(15)
=======

lotto1<-function(x){

  num1<-data.frame(table(luckynumber$X1))

  number1<-num1 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number1$Var1,1)

}


<<<<<<< HEAD
lotto2<-function(){

  num2<-data.frame(table(luckynumber$`2`))

  number2<-num2 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto2<-function(x){

  num2<-data.frame(table(luckynumber$X2))

  number2<-num2 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number2$Var1,1)

}


<<<<<<< HEAD
lotto3<-function(){

  num3<-data.frame(table(luckynumber$`3`))

  number3<-num3 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto3<-function(x){

  num3<-data.frame(table(luckynumber$X3))

  number3<-num3 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number3$Var1,1)

}

<<<<<<< HEAD
lotto4<-function(){

  num4<-data.frame(table(luckynumber$`4`))

  number4<-num4 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto4<-function(x){

  num4<-data.frame(table(luckynumber$X4))

  number4<-num4 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number4$Var1,1)

}



<<<<<<< HEAD
lotto5<-function(){

  num5<-data.frame(table(luckynumber$`5`))

  number5<-num5 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto5<-function(x){

  num5<-data.frame(table(luckynumber$X5))

  number5<-num5 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number5$Var1,1)

}


<<<<<<< HEAD
lotto6<-function(){

  num6<-data.frame(table(luckynumber$`6`))

  number6<-num6 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto6<-function(x){

  num6<-data.frame(table(luckynumber$X6))

  number6<-num6 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number6$Var1,1)

}


<<<<<<< HEAD
lotto7<-function(){

  num7<-data.frame(table(luckynumber$`Bonus`))

  number7<-num7 %>% arrange(desc(Freq)) %>% head(15)
=======
lotto7<-function(x){

  num7<-data.frame(table(luckynumber$Bonus))

  number7<-num7 %>% arrange(desc(Freq)) %>% head(x)
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0

  sample(number7$Var1,1)

}

<<<<<<< HEAD
lotto<-function(){
  a<- data.frame(lotto1(), lotto2(), lotto3(), lotto4(), lotto5(), lotto6(), lotto7())
  a
}

#' very useful function
#' this function simply add 1.
#'
#' @param x any number
#' @return \code{x+1}
#'
#' @examples
#' myfun(1)
myfun<-function(x){
  x+1
}




=======
lotto<-function(x){
  a<- data.frame(lotto1(x), lotto2(x), lotto3(x), lotto4(x), lotto5(x), lotto6(x), lotto7(x))
  a
}
>>>>>>> 5526ceac759e177accbd51edc5d1d91ac77036b0
