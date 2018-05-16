#' Guess the Correct Distribution
#'
#' This function generates some random variables.
#' You just guess the correct distribuiton.
#'
gd <- function(){

  dist_vec <- c("norm", "unif", "exp", "t", "chisq")

  while(T){
    rn <- sample(1:length(dist_vec), 1)
    mn <- sample(1:length(dist_vec), 1)

    if (dist_vec[rn] == "t" | dist_vec[rn] == "chisq"){
      print(get(paste0("r", dist_vec[rn]))(mn, df = 10))
    } else {
      print(get(paste0("r", dist_vec[rn]))(mn))
    }

    cat("Guess the distribution : \n")
    cat("1: norm,  2: unif,  3: exp,  4: t,  5: chisq\n")
    num <- readline("Enter the number (to quit type q) > ")
    if(num == "q") break

    num <- as.numeric(num)

    if (rn == num){
      cat("Corret!\n\n")
    } else{
      cat("Wrong!\n\n")
    }
  }
}
