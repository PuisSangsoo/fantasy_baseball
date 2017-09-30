fantasy <- read.table("FantasyLeague.csv", sep=",", header = TRUE)

score <- function(data, team){
  scores <- matrix(nrow = 2, ncol = 10)
  scorename <- c("zscore","sscore")
  category <- c("R", "HR", "RBI", "SB", "AVG", "W", "SV", "K", "ERA", "WHIP")
  rownames(scores) <- scorename
  colnames(scores) <- category
  c = 1
  for(i in category){
    if (i == "ERA" || i == "WHIP"){
      scores[1,c] = (mean(data[,i]) - data[team,i]) / sd(data[,i])
      scores[2,c] = ((10 * (mean(data[,i]) - data[team,i])) / sd(data[,i])) + 50
    } else {
      scores[1,c] = (data[team,i] - mean(data[,i])) / sd(data[,i])
      scores[2,c] = ((10 * (data[team,i] - mean(data[,i]))) / sd(data[,i])) + 50
    }
    c = c + 1
  } 
  scores
}

score(fantasy, 1)