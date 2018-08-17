setwd('~/R Work')
ball <- read.csv('deliveries.csv', header = TRUE, stringsAsFactors = FALSE)
matches <- read.csv('matches.csv', header = TRUE, stringsAsFactors = FALSE)
matches <- subset(matches, select = c('id','season','team1','team2','winner','win_by_runs','win_by_wickets','player_of_match'))

sum(matches$player_of_match == "" | matches$player_of_match == " ")
#There are three matches where player_of_match is blank. Must remove those.
matches <- matches[matches$player_of_match != "",]

#We want 75% train and 25% test matches
sample_size <- floor(0.75 * nrow(matches))
set.seed(2017)
train_index <- sample(seq_len(nrow(matches)), size = sample_size)
mom_train <- matches[train_index,]
mom_test <- matches[-train_index,]
#Now subset into train and test
ball_train <- ball[ball$match_id %in% mom_train$id,]
ball_test <- ball[ball$match_id %in% mom_test$id,]

#Get the batsmen and their record
batsmen_train <- aggregate(ball_train$batsman_runs ~ ball_train$batsman + ball_train$batting_team + ball_train$inning + ball_train$match_id, ball_train, sum)
mom <- subset(matches, select = c('id','winner','player_of_match'))
colnames(batsmen_train) <- c('batsman','team','innings','match_id','runs')
batsmen_train <- merge(batsmen_train,mom,by.x = 'match_id',by.y = 'id')

#Repeat this for test
batsmen_test <- aggregate(ball_test$batsman_runs ~ ball_test$batsman + ball_test$batting_team + ball_test$inning + ball_test$match_id, ball_test, sum)
colnames(batsmen_test) <- c('batsman','team','innings','match_id','runs')
batsmen_test <- merge(batsmen_test,mom,by.x = 'match_id', by.y = 'id')

#Was this player the man of the match?
batsmen_train$is_mom <- 0
batsmen_train$is_mom[batsmen_train$batsman == batsmen_train$player_of_match] <- 1
#Did the player's team win?
batsmen_train$winning_side <- 0
batsmen_train$winning_side[batsmen_train$team == batsmen_train$winner] <- 1

#Repeat this for test
batsmen_test$is_mom <- 0
batsmen_test$is_mom[batsmen_test$batsman == batsmen_test$player_of_match] <- 1
batsmen_test$winning_side <- 0
batsmen_test$winning_side[batsmen_test$team == batsmen_test$winner] <- 1

# #Classify runs into bins
# batsmen_train$run_class[batsmen_train$runs > 100] <- '100+'
# batsmen_train$run_class[batsmen_train$runs < 100 & batsmen_train$runs >= 80] <- '80-100'
# batsmen_train$run_class[batsmen_train$runs < 80 & batsmen_train$runs >= 60] <- '60-80'
# batsmen_train$run_class[batsmen_train$runs < 60 & batsmen_train$runs >= 50] <- '50-60'
# batsmen_train$run_class[batsmen_train$runs < 50 & batsmen_train$runs >= 40] <- '40-50'
# batsmen_train$run_class[batsmen_train$runs < 40 & batsmen_train$runs >= 30] <- '30-40'
# batsmen_train$run_class[batsmen_train$runs < 30 & batsmen_train$runs >= 20] <- '20-30'
# batsmen_train$run_class[batsmen_train$runs < 20 & batsmen_train$runs >= 10] <- '10-20'
# batsmen_train$run_class[batsmen_train$runs < 10 & batsmen_train$runs >= 0] <- '0-10'

# #Repeat this for test
# batsmen_test$run_class[batsmen_test$runs > 100] <- '100+'
# batsmen_test$run_class[batsmen_test$runs <= 100 & batsmen_test$runs > 80] <- '80-100'
# batsmen_test$run_class[batsmen_test$runs <= 80 & batsmen_test$runs > 60] <- '60-80'
# batsmen_test$run_class[batsmen_test$runs <= 60 & batsmen_test$runs > 50] <- '50-60'
# batsmen_test$run_class[batsmen_test$runs <= 50 & batsmen_test$runs > 40] <- '40-50'
# batsmen_test$run_class[batsmen_test$runs <= 40 & batsmen_test$runs > 30] <- '30-40'
# batsmen_test$run_class[batsmen_test$runs <= 30 & batsmen_test$runs > 20] <- '20-30'
# batsmen_test$run_class[batsmen_test$runs <= 20 & batsmen_test$runs > 10] <- '10-20'
# batsmen_test$run_class[batsmen_test$runs <= 10 & batsmen_test$runs >= 0] <- '0-10'

#Add a column to indicate if top scorer of the match
batsmen_train$max_score <- ave(batsmen_train$runs, batsmen_train$match_id, FUN = max)
batsmen_train$is_top_scorer <- 0
batsmen_train$is_top_scorer[batsmen_train$runs == batsmen_train$max_score] <- 1

#Repeat this for test
batsmen_test$max_score <- ave(batsmen_test$runs, batsmen_test$match_id, FUN = max)
batsmen_test$is_top_scorer <- 0
batsmen_test$is_top_scorer[batsmen_test$runs == batsmen_test$max_score] <- 1

#Let's look at the bowlers now
no_runouts <- ball_train[ball_train$dismissal_kind!='run out',]
bowlers_train <- aggregate(no_runouts$player_dismissed ~ no_runouts$bowler + no_runouts$bowling_team + no_runouts$match_id, no_runouts,function(x){sum(x!="")})
bowlers_runs <- aggregate(no_runouts$total_runs ~ no_runouts$bowler + no_runouts$bowling_team + no_runouts$match_id, no_runouts, sum)
colnames(bowlers_train) <- c('bowler','team','match_id','wickets')
colnames(bowlers_runs) <- c('bowler','team','match_id','runs')
bowlers_train <- merge(bowlers_train, bowlers_runs, by=c('match_id','bowler','team'))
bowlers_train <- merge(bowlers_train,mom,by.x = 'match_id',by.y = 'id')
#Is this player the man of the match?
bowlers_train$is_mom <- 0
bowlers_train$is_mom[bowlers_train$bowler == bowlers_train$player_of_match] <- 1

#Did the player's team win?
bowlers_train$winning_side <- 0
bowlers_train$winning_side[bowlers_train$team == bowlers_train$winner] <- 1

#Repeat this for the test set
no_runouts_test <- ball_test[ball_test$dismissal_kind!='run out',]
bowlers_test <- aggregate(player_dismissed ~ bowler + bowling_team + match_id, no_runouts_test,function(x){sum(x!="")})
bowlers_runs_test <- aggregate(total_runs ~ bowler + bowling_team + match_id, no_runouts_test, sum)
colnames(bowlers_test) <- c('bowler','team','match_id','wickets')
colnames(bowlers_runs_test) <- c('bowler','team','match_id','runs')
bowlers_test <- merge(bowlers_test, bowlers_runs_test,by=c('match_id','bowler','team'))
bowlers_test <- merge(bowlers_test, mom, by.x = 'match_id', by.y = 'id')

bowlers_test$is_mom <- 0
bowlers_test$is_mom[bowlers_test$bowler == bowlers_test$player_of_match] <- 1

bowlers_test$winning_side <- 0
bowlers_test$winning_side[bowlers_test$team == bowlers_test$winner] <- 1

batsmen_train$innings <- NULL
batsmen_test$innings <- NULL

#Now let's merge all the players together
players_train <- merge(batsmen_train, bowlers_train, by.x=c(1,2,3,5,6,7,8), by.y = c(1,2,3,6,7,8,9), all = TRUE)
players_test <- merge(batsmen_test, bowlers_test, by.x = c(1,2,3,5,6,7,8), by.y = c(1,2,3,6,7,8,9), all = TRUE)

#Let's treat the NA's
#If a guy has NA runs scored, he didn't bat, =0
players_train$runs.x[is.na(players_train$runs.x)] <- 0
# players_train$run_class[is.na(players_train$run_class)] <- '0-10'
#NA in is_top_scorer means he didn't bat, NA in wickets means he didn't bowl
players_train$is_top_scorer[is.na(players_train$is_top_scorer)] <- 0
players_train$wickets[is.na(players_train$wickets)] <- 0
players_train$runs.y[is.na(players_train$runs.y)] <- 0

#Repeat for test
players_test$runs.x[is.na(players_test$runs.x)] <- 0
# players_test$run_class[is.na(players_test$run_class)] <- '0-10'
#NA in is_top_scorer means he didn't bat, NA in wickets means he didn't bowl
players_test$is_top_scorer[is.na(players_test$is_top_scorer)] <- 0
players_test$wickets[is.na(players_test$wickets)] <- 0
players_test$runs.y[is.na(players_test$runs.y)] <- 0

#Let's include the top wicket takers lest they feel left out
players_test$max_wickets <- ave(players_test$wickets, players_test$match_id, FUN = max)
players_test$is_top_wicket_taker <- 0
players_test$is_top_wicket_taker[players_test$wickets == players_test$max_wickets] <- 1

players_train$max_wickets <- ave(players_train$wickets, players_train$match_id, FUN = max)
players_train$is_top_wicket_taker <- 0
players_train$is_top_wicket_taker[players_train$wickets == players_train$max_wickets] <- 1

#Now, let's do some EDA first without jumping into anything
mom_train_summary <- aggregate(is_mom ~ winning_side + is_top_scorer + is_top_wicket_taker, players_train, sum)
mom_test_summary <- aggregate(is_mom ~ winning_side + is_top_scorer + is_top_wicket_taker, players_test, sum)
sum(mom_train_summary$is_mom)
sum(mom_test_summary$is_mom)

prf <- function()
{
  tp <- length(which(players_test$is_mom == 1 & players_test$prediction_mom == 1))
  tn <- length(which(players_test$is_mom == 0 & players_test$prediction_mom == 0))
  fp <- length(which(players_test$is_mom == 0 & players_test$prediction_mom == 1))
  fn <- length(which(players_test$is_mom == 1 & players_test$prediction_mom == 0))
  prec <- tp/(tp + fp)
  rec <- tp/(tp + fn)
  f1 <- (2*prec*rec)/(prec +rec)
  print(paste0("Precision: ", prec))
  print(paste0("Recall: ",rec)) 
  print(paste0("F1 score: ", f1))
}

players_train$match_runs <- ave(players_train$runs.x, players_train$match_id, FUN = sum)
players_test$match_runs <- ave(players_test$runs.x, players_test$match_id, FUN = sum)
players_train$match_run_share <-players_train$runs.x/players_train$match_runs
players_test$match_run_share <-players_test$runs.x/players_test$match_runs

players_train$match_wickets <- ave(players_train$wickets, players_train$match_id, FUN = sum)
players_train$match_wicket_share <- players_train$wickets/players_train$match_wickets
players_test$match_wickets <- ave(players_test$wickets, players_test$match_id, FUN = sum)
players_test$match_wicket_share <- players_test$wickets/players_test$match_wickets

#Let's include player strike rates and economies
balls_played_train <- aggregate(ball ~ match_id + batsman + batting_team, ball_train, length)
balls_played_test <- aggregate(ball ~ match_id + batsman + batting_team, ball_test, length)

players_train <- merge(players_train, balls_played_train,by.x = c(1,2,3), by.y = c(1,2,3), all = TRUE)
players_train$ball[is.na(players_train$ball)] <- 0

players_test <- merge(players_test, balls_played_test,by.x = c(1,2,3), by.y = c(1,2,3), all = TRUE)
players_test$ball[is.na(players_test$ball)] <- 0

players_train$sr <- (players_train$runs.x / players_train$ball)*100
players_test$sr <- (players_test$runs.x / players_test$ball)*100
players_train$sr[is.nan(players_train$sr)] <- 0
players_test$sr[is.nan(players_test$sr)] <- 0

#Maybe the guys who finish off the job have an advantage
last_players_train <- aggregate(batsman ~ match_id + inning, ball_train, function(x){tail(x)[1]})
last_ns_train <- aggregate(non_striker ~ match_id + inning, ball_train, function(x){tail(x)[1]})
last_batsmen_train <- merge(last_players_train, last_ns_train, by = c(1,2))
last_batsmen_train <- last_batsmen_train[last_batsmen_train$inning == 2,]

last_players_test <- aggregate(batsman ~ match_id + inning, ball_test, function(x){tail(x)[1]})
last_ns_test <- aggregate(non_striker ~ match_id + inning, ball_test, function(x){tail(x)[1]})
last_batsmen_test <- merge(last_players_test, last_ns_test, by = c(1,2))
last_batsmen_test <- last_batsmen_test[last_batsmen_test$inning == 2,]

players_train <- merge(players_train, last_batsmen_train, by = 'match_id', all = TRUE)
players_test <- merge(players_test, last_batsmen_test, by = 'match_id', all = TRUE)

players_train$finisher_flag <- 0
players_train$finisher_flag[players_train$win_by_wickets > 0 & (players_train$batsman.x == players_train$batsman.y| players_train$batsman.x == players_train$non_striker)] <- 1
players_test$finisher_flag <- 0
players_test$finisher_flag[players_test$win_by_wickets > 0 & (players_test$batsman.x == players_test$batsman.y| players_test$batsman.x == players_test$non_striker)] <- 1

overs_bowled_train <- aggregate(over ~ match_id + bowler, ball_train, function(x) {length(unique(x))})
players_train <- merge(players_train, overs_bowled_train, by.x = c(1,2), by.y=c(1,2),all = TRUE)
overs_bowled_test <- aggregate(over ~ match_id + bowler, ball_test, function(x) {length(unique(x))})
players_test <- merge(players_test, overs_bowled_test, by.x = c(1,2), by.y=c(1,2), all = TRUE)

#Find out the economy rate for every bowler
players_train$economy <- players_train$runs.y/players_train$over
players_train$economy[is.na(players_train$economy)] <- 0

players_test$economy <- players_test$runs.y/players_test$over
players_test$economy[is.na(players_test$economy)] <- 0

players_train$bat_flag <- 0
players_train$bat_flag[players_train$runs.x >= 15] <- 1
players_train$bowl_flag <- 0
players_train$bowl_flag[players_train$wickets > 1] <- 1

players_test$bat_flag <- 0
players_test$bat_flag[players_test$runs.x >= 15] <- 1
players_test$bowl_flag <- 0
players_test$bowl_flag[players_test$wickets > 1] <- 1

#A dataframe to look at the ones the model got wrong
wrong <- players_test[players_test$is_mom != players_test$prediction_mom,]
#######################################################################################################
#And now, the models
library(caret)
# define training control
train_control<- trainControl(method="repeatedcv", number=10, repeats = 10, savePred=TRUE)

players_train$is_mom <- as.factor(players_train$is_mom)
players_test$is_mom <- as.factor(players_test$is_mom)
#logistic regression model
logistic_mom_finish <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                             + is_top_scorer + is_top_wicket_taker
                             + match_run_share + match_runs + finisher_flag
                             + bat_flag + bowl_flag,data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_finish, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Neural network with these same flags
library(neuralnet)
nn_finish <- neuralnet(as.numeric(as.character(is_mom)) ~ winning_side +runs.x + wickets + runs.y
                             + is_top_scorer + is_top_wicket_taker
                             + match_run_share + match_runs + finisher_flag
                             + bat_flag + bowl_flag,data=players_train,hidden=5, threshold = 0.1, lifesign = "full", stepmax = 1e6, linear.output = FALSE, act.fct = "logistic")
h <- compute(nn_finish, players_test[c(7,8,11,12,10,14,16,15,24,30,31)])
#Now, let's assign these as probabilities and see how well it performs
players_test$pred_prob <- h$net.result
players_test$max_match_prob <- ave(players_test$pred_prob, players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()
#Doesn't do as well as the logistic regression model. Hmmm.
######################################################################################################
#Some EDA plots to visualise the data better.
#Histogram of runs scored by batsmen who got the man of the match
batsmen_mom <- rbind(batsmen_train, batsmen_test)
batsmen_mom <- batsmen_mom[batsmen_mom$is_mom == 1,]

hist(x = batsmen_mom$runs)

bowlers_mom <- rbind(bowlers_train, bowlers_test)
bowlers_mom <- bowlers_mom[bowlers_mom$is_mom == 1,]

barplot(table(bowlers_mom$wickets), main = 'Wickets taken by the man of the match', xlab = 'Wickets',
        ylab = 'Number of bowlers', axes = TRUE, col = 'blue')

players_mom <- rbind(players_test, players_train)
players_mom <- players_mom[players_mom$is_mom == 1,]

fourfoldplot(table(players_mom$is_top_scorer, players_mom$is_top_wicket_taker), color = c("#008080", "#00FFFF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

library(ggplot2)
df = data.frame(table(players_mom$is_top_scorer, players_mom$is_top_wicket_taker))
colnames(df) <- c('Top Scorer', 'Top Wicket Taker', 'Number of Players')
ggplot(data =  df, mapping = aes(x = df$`Top Scorer`, y = df$`Top Wicket Taker`)) +
  geom_tile(aes(fill = df$`Number of Players`), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df$`Number of Players`)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")+
  ggtitle('Was the Man of the Match the top scorer or the top wicket taker?')+xlab('Top Scorer')+ylab('Top Wicket Taker')

