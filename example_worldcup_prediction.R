#' Predict the World Cup 2018 Finals - France VS Croatia 
#' 
#' @param total_games
#' @param total_goals_90
#' @param team1
#' @param team2
#' @return nothing
#'
#' @author Ngan-Meng Tan
#   

##------------------------------------------------------------

total_games <- 63
#only 90 mins
total_goals_90 <- 163
avg_goals_per_game <- total_goals_90/total_games
avg_goals_per_game_per_team <- avg_goals_per_game/2

# Croatia vs France

fr_games <- 6
fr_goals_scored <- 10
fr_avg_goals_scored_per_game <- fr_goals_scored/fr_games
fr_attack_strength <- fr_avg_goals_scored_per_game/avg_goals_per_game_per_team
fr_goals_concede <- 4
fr_avg_goals_concede_per_game <- fr_goals_concede/fr_games
fr_defend_strength <- fr_avg_goals_concede_per_game/avg_goals_per_game_per_team

#fr_attack_strength > avg_goals_per_game_per_team : good attack
#fr_defend_strength < avg_goals_per_game_per_team : good defense

cr_games <- 6
cr_goals_scored <- 11
cr_avg_goals_scored_per_game <- cr_goals_scored/cr_games
cr_attack_strength <- cr_avg_goals_scored_per_game/avg_goals_per_game_per_team
cr_goals_concede <- 4
cr_avg_goals_concede_per_game <- cr_goals_concede/cr_games
cr_defend_strength <- cr_avg_goals_concede_per_game/avg_goals_per_game_per_team


prob_fr_scoring = fr_avg_goals_scored_per_game *cr_defend_strength
prob_cr_scoring = cr_avg_goals_scored_per_game *fr_defend_strength


#poisson distribution
team = c("France", "Croatia") 
df = data.frame(team)       # df is a data frame

df$`0` <- c(dpois(0, prob_fr_scoring) *100, dpois(0, prob_cr_scoring)  *100)
df$`1` <- c(dpois(1, prob_fr_scoring) *100, dpois(1, prob_cr_scoring)  *100)
df$`2` <- c(dpois(2, prob_fr_scoring)*100, dpois(2, prob_cr_scoring)  *100)
df$`3` <- c(dpois(3, prob_fr_scoring)*100, dpois(3, prob_cr_scoring)  *100)
df$`4` <- c(dpois(4, prob_fr_scoring)*100, dpois(4, prob_cr_scoring)  *100)
df$`5` <- c(dpois(5, prob_fr_scoring)*100, dpois(5, prob_cr_scoring)  *100)

# library(reshape2)
# df1 <- melt(df)
df1 <- subset(df, select = -c(team))
score_prob = (crossprod(as.matrix(df1[2,]),as.matrix((df1[1,])))/100)
colnames(score_prob) <- c("fr0","fr1","fr2","fr3","fr4", "fr5")

fr_win = sum(score_prob[(upper.tri(score_prob, diag = FALSE))])
fr_cr_draw = sum(diag(score_prob))
cr_win = sum(score_prob[(lower.tri(score_prob, diag = FALSE))]) 

print(paste0("France : ", fr_win, "%"))
print(paste0("Draw : ", fr_cr_draw, "%"))
print(paste0("Croatia : ", cr_win, "%"))
print(paste0("Total : ", fr_win+ fr_cr_draw + cr_win, "%"))

#=====
# > print(paste0("France : ", fr_win, "%"))
# [1] "France : 31.2276695414184%"
# > print(paste0("Draw : ", fr_cr_draw, "%"))
# [1] "Draw : 32.8018426029539%"
# > print(paste0("Croatia : ", cr_win, "%"))
# [1] "Croatia : 35.8993691269447%"
# > print(paste0("Total : ", fr_win+ fr_cr_draw + cr_win, "%"))
# [1] "Total : 99.928881271317%"
