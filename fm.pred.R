#' Produce the soccer prediction 
#' 
#' @param total_games
#' @param total_goals_90
#' @param team1
#' @param team2
#' @param verbose (default TRUE) verbose output
#' @return nothing
#'
#' @author Ngan-Meng Tan
#' @examples
#'  \dontrun{
#'    fm.pred(total_games, total_goals_90, team1, team2, verbose=FALSE)
#'  }
#' 

# fm.pred = function(total_games, total_goals_90, team1, team2, verbose=FALSE){
  # require(highcharter)

#   Test
# total_games <- 12*10
# #only 90 mins
# total_goals_90 <- 330
# team1 <- data.frame(tot_games = 12, goals_scored = 20, goals_concede=21)
# team2 <- data.frame(tot_games = 12, goals_scored = 8, goals_concede=17)

#=======================================================================================================
## Function 
#=======================================================================================================
fm.pred <- function(total_games, total_goals_90, team1, team2) {
    avg_goals_per_game <- total_goals_90/total_games
    avg_goals_per_game_per_team <- avg_goals_per_game/2
    
    # Team1 vs Team2
    
    # team1$tot_games <- 6
    # team1$goals_scored <- 10
    team1.avg_goals_scored_per_game <- team1$goals_scored/team1$tot_games
    team1.attack_strength <- team1.avg_goals_scored_per_game/avg_goals_per_game_per_team
    # team1$goals_concede <- 4
    team1.avg_goals_concede_per_game <- team1$goals_concede/team1$tot_games
    team1.defend_strength <- team1.avg_goals_concede_per_game/avg_goals_per_game_per_team
    
    #team1.attack_strength > avg_goals_per_game_per_team : good attack
    #team1.defend_strength < avg_goals_per_game_per_team : good defense
    
    # team2$tot_games <- 6
    # team2$goals_scored <- 11
    team2.avg_goals_scored_per_game <- team2$goals_scored/team2$tot_games
    team2.attack_strength <- team2.avg_goals_scored_per_game/avg_goals_per_game_per_team
    # team2$goals_concede <- 4
    team2.avg_goals_concede_per_game <- team2$goals_concede/team2$tot_games
    team2.defend_strength <- team2.avg_goals_concede_per_game/avg_goals_per_game_per_team
    
    
    prob_team1.scoring = team1.avg_goals_scored_per_game *team2.defend_strength
    prob_team2.scoring = team2.avg_goals_scored_per_game *team1.defend_strength
    
    
    #poisson distribution
    team = c("Team1", "Team2") 
    df = data.frame(team)       # df is a data frame
    
    df$`0` <- c(dpois(0, prob_team1.scoring) *100, dpois(0, prob_team2.scoring)  *100)
    df$`1` <- c(dpois(1, prob_team1.scoring) *100, dpois(1, prob_team2.scoring)  *100)
    df$`2` <- c(dpois(2, prob_team1.scoring)*100, dpois(2, prob_team2.scoring)  *100)
    df$`3` <- c(dpois(3, prob_team1.scoring)*100, dpois(3, prob_team2.scoring)  *100)
    df$`4` <- c(dpois(4, prob_team1.scoring)*100, dpois(4, prob_team2.scoring)  *100)
    df$`5` <- c(dpois(5, prob_team1.scoring)*100, dpois(5, prob_team2.scoring)  *100)
    
    # library(reshape2)
    # df1 <- melt(df)
    df1 <- subset(df, select = -c(team))
    score_prob = (crossprod(as.matrix(df1[2,]),as.matrix((df1[1,])))/100)
    colnames(score_prob) <- c("team1_0","team1_1","team1_2","team1_3","team1_4", "team1_5")
    
    team1.win = sum(score_prob[(upper.tri(score_prob, diag = FALSE))])
    team1.team2.draw = sum(diag(score_prob))
    team2.win = sum(score_prob[(lower.tri(score_prob, diag = FALSE))]) 
    
    print(paste0("Team1 : ", team1.win, "%"))
    print(paste0("Draw : ", team1.team2.draw, "%"))
    print(paste0("Team2 : ", team2.win, "%"))
    print(paste0("Total : ", team1.win+ team1.team2.draw + team2.win, "%"))
    res <- data.frame( Team1_WIN = team1.win , Draw = team1.team2.draw, Team2_WIN=team2.win, lambda1=prob_team1.scoring, lambda2=prob_team2.scoring)
    
    #return (list(score_prob,team1.win, team1.team2.draw, team2.win, lambda1=prob_team1.scoring, lambda2=prob_team2.scoring) )
    return(res)
}
#=======================================================================================================

#https://us.soccerway.com/national/england/premier-league/20182019/regular-season/r48730/

epl_table<- read.csv(file="/Users/nganmeng.tan/Documents/Code/epl_table_1718.csv", header=TRUE, sep=",")

total_games <- max(epl_table$MP)*10
total_goals_90 <- sum(epl_table$F)

# team1 <- data.frame(tot_games = 12, goals_scored = 20, goals_concede=21)
# team2 <- data.frame(tot_games = 12, goals_scored = 8, goals_concede=17)
epl_fixture<- read.csv(file="/Users/nganmeng.tan/Documents/Code/epl_fixture.csv", header=TRUE, sep=",")
epl_fixture$Team1_WIN<-NA
epl_fixture$Draw<-NA
epl_fixture$Team2_WIN<-NA


for (row in 1:nrow(epl_fixture)) {
  team1_name <- epl_fixture[row,]$Team1
  team2_name <- epl_fixture[row,]$Team2
  
  team1_stats <- epl_table[(epl_table$Team == as.character(team1_name)),]
  team2_stats <- epl_table[(epl_table$Team == as.character(team2_name)),]
  
  team1 <- data.frame(tot_games = team1_stats$MP, goals_scored = team1_stats$F, goals_concede=team1_stats$A)
  team2 <- data.frame(tot_games = team2_stats$MP, goals_scored = team2_stats$F, goals_concede=team2_stats$A)
  
  res <- fm.pred(total_games, total_goals_90, team1, team2) 
  epl_fixture$Team1_WIN[row] <- res$Team1_WIN
  epl_fixture$Draw[row]<- res$Draw
  epl_fixture$Team2_WIN[row] <- res$Team2_WIN
} 

keeps <- c("Day","Date","Team1","Team2","Team1_WIN", "Draw","Team2_WIN")
pred<-epl_fixture[keeps]

pred
#=======================================================================================================

