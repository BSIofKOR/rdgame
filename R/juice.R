
#src

address <- "http://www.nlotto.co.kr/gameResult.do?

method=allWinExel&gubun=byWin&nowPage=&drwNoStart=1&drwNoEnd=9999"

download.file(address, destfile = "test.xls")

Sys.setlocale(category = "LC_ALL", locale = "us")

tables <- XML::readHTMLTable("test.xls",skip.rows= 1)

history_lotto <- tables[[2]]



#level->numeric

year<-as.numeric(as.character(history_lotto$V1))

one<-as.numeric(as.character(history_lotto$V13))

two<-as.numeric(as.character(history_lotto$V14))

three<-as.numeric(as.character(history_lotto$V15))

four<-as.numeric(as.character(history_lotto$V16))

five<-as.numeric(as.character(history_lotto$V17))

six<-as.numeric(as.character(history_lotto$V18))

seven<-as.numeric(as.character(history_lotto$V19))

eight<-as.numeric(as.character(history_lotto$V20))



first<-rep(NA,length(year))

second<-rep(NA,length(year))

third<-rep(NA,length(year))

fourth<-rep(NA,length(year))

fifth<-rep(NA,length(year))

sixth<-rep(NA,length(year))

bonus<-rep(NA,length(year))



for(i in 1:length(year)){

  if(year[i]>=2002){

    first[i]<-two[i]

    second[i]<-three[i]

    third[i]<-four[i]

    fourth[i]<-five[i]

    fifth[i]<-six[i]

    sixth[i]<-seven[i]

    bonus[i]<-eight[i]

  }else{

    first[i]<-one[i]

    second[i]<-two[i]

    third[i]<-three[i]

    fourth[i]<-four[i]

    fifth[i]<-five[i]

    sixth[i]<-six[i]

    bonus[i]<-seven[i]

  }

}



from<-1:length(year)

to<-1:length(year)



winning_number_d.f<-data.frame(first,second,third, fourth, fifth, sixth, bonus)

winning_number_mat<-as.matrix(winning_number_d.f)

rownames(winning_number_mat)<-c(as.numeric(length(year)):1)

t.1<-table(first)

t.2<-table(second)

t.3<-table(third)

t.4<-table(fourth)

t.5<-table(fifth)

t.6<-table(sixth)

t.b<-table(bonus)









num<-1:45

freq_1<-rep(0,45)

freq_2<-rep(0,45)

freq_3<-rep(0,45)

freq_4<-rep(0,45)

freq_5<-rep(0,45)

freq_6<-rep(0,45)

freq_7<-rep(0,45)

for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(first[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_1[i]<-s

}



for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(second[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_2[i]<-s

}

for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(third[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_3[i]<-s

}

for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(fourth[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_4[i]<-s

}



for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(fifth[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_5[i]<-s

}



for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(sixth[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_6[i]<-s

}



for(i in 1:45){

  s<-0

  for(z in 1:length(year)){

    if(bonus[z]==i){

      s<-s+1}else{

        s<-s

      }

  }

  freq_7[i]<-s

}



num_freq<-rep(0,45)

for(i in 1:45){

  num_freq[i]<-sum(freq_1[i],freq_2[i],freq_3[i],freq_4[i],freq_5[i],freq_6[i],freq_7[i])



}



frequency<-data.frame(num, num_freq)







# 1. 모든 회차에서 많이 뽑힌 순서대로 번호 보기



lotto11<-function(x=5){

  num_freq_sort<-frequency[order(-num_freq),]

  num_freq_sort<-as.matrix(num_freq_sort)

  num_freq_sort[1:x, ]

}


#2. 보너스 회차에서 많이 뽑힌 순서대로 번호 보기


lotto22<-function(x=5){

  frequency_bonus<-data.frame(num,freq_7)

  num_freq_sort_bonus<-frequency_bonus[order(-freq_7),]

  num_freq_sort_bonus<-as.matrix(num_freq_sort)

  num_freq_sort_bonus[1:x, ]
}


# 3. 각 번호당 뽑혔던 빈도를 가중치로 하여 가장 뽑힐 확률이 놓은 6개의 숫자 추출



lotto33<-function(n=3){


  for(i in 1:n){

    p=matrix(0)

    sumData=sum(frequency$num_freq)

    for(z in 1:length(frequency$num_freq)){p[z]=(frequency$num_freq[z])/sumData}

    x=runif(length(frequency$num_freq),0,1)

    for(a in 1:length(frequency$num_freq)){p[a]=p[a]*x[a]}

    sortP<-sort(p,decreasing=TRUE)

    cat("당첨번호: ")

    for(m in 1:7){

      for(j in 1:length(p)){

        if(sortP[m]==p[j]){ cat(j,"  "); break }

      }

    }

    cat("\n")

  }

}











# 4. 각 자리수에서 뽑히는 숫자의 빈도를 가중치로 계산하여 숫자 추출




lotto44<-function(n=3){

  a<-matrix(rep(0,7*n),nrow=n)


  for(i in 1:n){
    repeat{

      lotto1<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_1)

        for(z in 1:length(freq_1)){ p[z]=freq_1[z]/sumData }

        x=runif(length(freq_1),0,1)

        for(a in 1:length(freq_1)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)

        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ one<-j }

        }
        one
      }


      one_1<-lotto1(n)



      lotto2<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_2)

        for(z in 1:length(freq_2)){ p[z]=freq_2[z]/sumData }

        x=runif(length(freq_2),0,1)

        for(a in 1:length(freq_2)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)

        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ two<-j }

        }
        two
      }

      two_2 <- lotto2(n)


      lotto3<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_3)

        for(z in 1:length(freq_3)){ p[z]=freq_3[z]/sumData }

        x=runif(length(freq_3),0,1)

        for(a in 1:length(freq_2)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)


        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ three<-j }

        }
        three
      }


      three_3 <- lotto3(n)


      lotto4<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_4)

        for(z in 1:length(freq_4)){ p[z]=freq_4[z]/sumData }

        x=runif(length(freq_4),0,1)

        for(a in 1:length(freq_4)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)

        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ four<-j }

        }
        four
      }


      four_4 <- lotto4(n)


      lotto5<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_5)

        for(z in 1:length(freq_5)){ p[z]=freq_5[z]/sumData }

        x=runif(length(freq_5),0,1)

        for(a in 1:length(freq_5)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)


        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ five<-j }

        }
        five
      }


      five_5 <- lotto5(n)


      lotto6<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_6)

        for(z in 1:length(freq_6)){ p[z]=freq_6[z]/sumData }

        x=runif(length(freq_6),0,1)

        for(a in 1:length(freq_6)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)

        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ six<-j }

        }
        six
      }


      six_6<- lotto6(n)


      lotto7<-function(n=3){

        p=matrix(0)

        sumData=sum(freq_7)

        for(z in 1:length(freq_7)){ p[z]=freq_7[z]/sumData }

        x=runif(length(freq_7),0,1)

        for(a in 1:length(freq_7)){ p[a]=p[a]*x[a] }

        sortP<-sort(p,decreasing=TRUE)

        for(j in 1:length(p)){

          if(sortP[1]==p[j]){ bonus<-j }

        }
        bonus
      }

      bonus_7<- lotto7(n)


      if(length(unique(c(one_1,two_2,three_3,four_4,five_5,six_6,bonus_7)))==7) break;
    }
    a[i,]<- c(one_1, two_2, three_3, four_4, five_5, six_6, bonus_7)
  }

  rownames(a)<-c( 1:nrow(a))
  colnames(a)<-c("first", "second", "third", "fourth", "fifth", "sixth","bonus")
  a
}




# 5. 각 자리수에서 뽑힌 빈도가 놓은 숫자 중 랜덤 추출




lotto55 <- function(n,x){

  a<-matrix(rep(0,7*n),nrow=n)


  for(i in 1:n){
    repeat{

      lottofirst<-function(x){

        num1<-data.frame(table(winning_number_d.f$first))

        number1<-num1 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number1$Var1,1)))

      }
      one <- lottofirst(x)

      lottosecond<-function(x){

        num2<-data.frame(table(winning_number_d.f$second))

        number2<-num2 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number2$Var1,1)))
      }

      two <- lottosecond(x)

      lottothird<-function(x){

        num3<-data.frame(table(winning_number_d.f$third))

        number3<-num3 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number3$Var1,1)))

      }

      three <- lottothird(x)

      lottofourth<-function(x){

        num4<-data.frame(table(winning_number_d.f$fourth))

        number4<-num4 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number4$Var1,1)))

      }

      four <- lottofourth(x)

      lottofifth<-function(x){

        num5<-data.frame(table(winning_number_d.f$fifth))

        number5<-num5 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number5$Var1,1)))

      }

      five <- lottofifth(x)

      lottosixth<-function(x){

        num6<-data.frame(table(winning_number_d.f$sixth))

        number6<-num6 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number6$Var1,1)))

      }

      six <- lottosixth(x)

      lottobonus<-function(x){

        num7<-data.frame(table(winning_number_d.f$bonus))

        number7<-num7 %>% arrange(desc(Freq)) %>% head(x)

        as.numeric(as.character(sample(number7$Var1,1)))

      }

      bonus <- lottobonus(x)
      if(length(unique(c(one,two,three,four,five,six,bonus)))==7) break;

    }
    a[i,]<- c(one, two, three, four, five, six, bonus)
  }
  rownames(a)<-c( 1:nrow(a))
  colnames(a)<-c("first", "second", "third", "fourth", "fifth", "sixth","bonus")
  a
}



# 6. 최근 회차 당첨 결과 확인 가능



the_latest_number<-function(x=5){

  winning_number_mat[1:x,]

}

