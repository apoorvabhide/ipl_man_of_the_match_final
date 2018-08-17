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

#Here's something interesting for ya: 97.8% of all MoM's are from the winning side in the training set
#It's also 98% in the test set, so there's that, too.

#So let's do something very basic: Let's use those three flags to predict using logistic regression
library(caret)
# define training control
train_control<- trainControl(method="repeatedcv", number=10, repeats = 10, savePred=TRUE)

players_train$is_mom <- as.factor(players_train$is_mom)
players_test$is_mom <- as.factor(players_test$is_mom)
# train the model 
logistic_mom_basic<- train(is_mom ~ winning_side 
              + is_top_scorer
              + is_top_wicket_taker, data=players_train, trControl=train_control, method="glm", family=binomial())

# print cv scores
summary(logistic_mom_basic)
varImp(logistic_mom_basic)

#Alright, let's try to predict on the test set
players_test$prediction_mom <- predict(logistic_mom_basic, players_test)
table(players_test$is_mom, players_test$prediction_mom)

# Output:
#   0    1
# 0 3140   41
# 1   86   74

#Define a function that gives precision, recall and F1-score
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

prf()
#Let's use another logistic model, this time using the actual runs and wickets too
logistic_mom_flag<- train(is_mom ~ winning_side +runs.x + wickets
                           + is_top_scorer
                           + is_top_wicket_taker, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$prediction_mom <- predict(logistic_mom_flag, players_test)
table(players_test$is_mom, players_test$prediction_mom)

# Output:
#   0    1
# 0 3145   36
# 1   73   87

prf()
#Okay, now let's fit a simple decision tree on runs and wickets, and see its performance
library(party)
dt_basic <- ctree(is_mom ~ winning_side + runs.x + wickets, players_train)
players_test$prediction_mom <- predict(dt_basic, players_test)
table(players_test$is_mom, players_test$prediction_mom)

# Output:
#   0    1
# 0 3124   57
# 1   68   92
prf()
#Let's include the flags in the decision tree as well
dt_flag <- ctree(is_mom ~ winning_side + is_top_scorer + runs.x + wickets + is_top_wicket_taker, players_train)
players_test$prediction_mom <- predict(dt_flag, players_test)
table(players_test$is_mom, players_test$prediction_mom)
#Oh wow, the accuracy actually fell.

# Output:
#   0    1
# 0 3155   26
# 1   81   79
prf()
#The logistic regression model definitely looks much better. I have to develop it more somehow.
#What other factors could be important in becoming man of the match?

#Let's also try the runs conceded, though it's unlikely it'll have much of an effect
logistic_mom_all <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                          + is_top_scorer
                          + is_top_wicket_taker, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$prediction_mom <- predict(logistic_mom_all, players_test)
table(players_test$is_mom, players_test$prediction_mom)
#Sliightly better, though not by much.

# Output:
#   0    1
# 0 3141   40
# 1   70   90
prf()
#Maybe MoM's being batsmen or bowlers depends on who was batting first or second
results_train <- mom_train[c(1,6,7)]
results_test <- mom_test[c(1,6,7)]

players_train <- merge(players_train, results_train, by.x = "match_id", by.y = "id", all = TRUE)
players_test <- merge(players_test, results_test, by.x = "match_id", by.y = "id", all = TRUE)
#Fit the model
logistic_mom_result <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                          + is_top_scorer + is_top_wicket_taker
                          + win_by_runs + win_by_wickets, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_result, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
table(players_test$is_mom, players_test$prediction_mom)
#Nope. It wasn't actually better. logistic_mom_all is better than logistic_mom_result.
prf()

#Best performance so far comes from logistic_mom_all.
#Get the probabilities from logistic_mom_all and predict the max from every match as the MoM
players_test$pred_prob <- predict(logistic_mom_result, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1

#Let's see how that worked out
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Random Forest
library(randomForest)
set.seed(2017)
rf_all <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                          + is_top_scorer
                          + is_top_wicket_taker, data=players_train, trControl=train_control, method="rf", family=binomial(), ntree = 500)

  players_test$pred_prob <- predict(rf_all, players_test, type = "prob")
  players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
  players_test$prediction_mom <- 0
  players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
  table(players_test$is_mom, players_test$prediction_mom)
  prf()
#Random forest doesn't do well enough.
#Trying SVM
svm_all <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                + is_top_scorer
                + is_top_wicket_taker, data=players_train, trControl=train_control, method="svmLinear", family=binomial(), ntree = 500)

#Let's train a neural network instead
library(neuralnet)
set.seed(2017)
nnet_all <-neuralnet(as.numeric(as.character(is_mom)) ~ winning_side +runs.x + wickets + runs.y
                             + is_top_scorer
                             + is_top_wicket_taker, data=players_train, hidden=6, threshold = 0.1, lifesign = "full", stepmax = 1e6, linear.output = FALSE, act.fct = "logistic")
plot(nnet_all)
h <- compute(nnet_all, players_test[c(7,8,11,12,10,14)])
#Now, let's assign these as probabilities and see how well it performs
players_test$pred_prob <- h$net.result
players_test$max_match_prob <- ave(players_test$pred_prob, players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()
#That's better than logistic regression. Now I just gotta tune this.
nn_results <- data.frame()
# this_result <- cbind(4,2,prf())
# nn_results <- rbind(nn_results, this_result)
#Trying with a single hidden layer and Neurons between 2 & 7
# i                V2
# 1 2 F1 score: 0.68125
# 2 3     F1 score: 0.7
# 3 4 F1 score: 0.68125
# 4 5 F1 score: 0.69375
# 5 6 F1 score: 0.73125
# 6 7 F1 score: 0.66875
# set.seed(2017)
# for(i in 2:11)
# {
#   nnet_all <-neuralnet(as.numeric(as.character(is_mom)) ~ winning_side +runs.x + wickets + runs.y
#                        + is_top_scorer
#                        + is_top_wicket_taker, data=players_train, hidden=c(i), threshold = 0.1, lifesign = "full", stepmax = 1e6, linear.output = FALSE, act.fct = "logistic")
#   h <- compute(nnet_all, players_test[c(7,8,11,12,10,14)])
#   #Now, let's assign these as probabilities and see how well it performs
#   players_test$pred_prob <- h$net.result
#   players_test$max_match_prob <- ave(players_test$pred_prob, players_test$match_id, FUN = max)
#   players_test$prediction_mom <- 0
#   players_test$prediction_mom[players_test$pred_prob == players_test$max_match_prob] <- 1
#   this_result <- cbind(i,prf())
#   nn_results <- rbind(nn_results, this_result)
# }

#Let's add a % of match runs and % of match wickets
players_train$match_runs <- ave(players_train$runs.x, players_train$match_id, FUN = sum)
players_test$match_runs <- ave(players_test$runs.x, players_test$match_id, FUN = sum)
players_train$match_run_share <-players_train$runs.x/players_train$match_runs
players_test$match_run_share <-players_test$runs.x/players_test$match_runs

players_train$match_wickets <- ave(players_train$wickets, players_train$match_id, FUN = sum)
players_train$match_wicket_share <- players_train$wickets/players_train$match_wickets
players_test$match_wickets <- ave(players_test$wickets, players_test$match_id, FUN = sum)
players_test$match_wicket_share <- players_test$wickets/players_test$match_wickets

nnet_all_share <-neuralnet(as.numeric(as.character(is_mom)) ~ winning_side +runs.x + wickets + runs.y
                     + is_top_scorer
                     + is_top_wicket_taker
                     + match_run_share + match_wicket_share, data=players_train, hidden=5, threshold = 0.1, lifesign = "full", stepmax = 1e6, linear.output = FALSE, act.fct = "logistic")
plot(nnet_all_share)
h <- compute(nnet_all, players_test[c(7,8,11,12,10,14,20,22)])
#Now, let's assign these as probabilities and see how well it performs
players_test$pred_prob <- h$net.result
players_test$max_match_prob <- ave(players_test$pred_prob, players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()


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

#Let's repeat the logistic regression with strike rate
logistic_mom_sr <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                             + is_top_scorer + is_top_wicket_taker
                             + match_run_share + match_wicket_share
                             + win_by_runs + win_by_wickets
                             + sr, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_sr, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Let's repeat the nn with sr
nnet_all_sr <-neuralnet(as.numeric(as.character(is_mom)) ~ winning_side +runs.x + wickets + runs.y
                           + is_top_scorer
                           + is_top_wicket_taker
                           + match_run_share + match_wicket_share
                           + sr, data=players_train, hidden=5, threshold = 0.1, lifesign = "full", stepmax = 1e6, linear.output = FALSE, act.fct = "logistic")
plot(nnet_all_sr)
h <- compute(nnet_all_sr, players_test[c(7,8,11,12,10,14,19,21,23)])
#Now, let's assign these as probabilities and see how well it performs
players_test$pred_prob <- h$net.result
players_test$max_match_prob <- ave(players_test$pred_prob, players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()
save(nnet_all_sr, file = "~/R Work/man-of-the-match-nnet.rda")
load(file = "~/R Work/man-of-the-match-nnet.rda")

wrong <- players_test[(players_test$is_mom == 1 & players_test$prediction_mom == 0) | (players_test$is_mom == 0 & players_test$prediction_mom == 1),]
aggregate(is_top_scorer ~ is_mom + prediction_mom, wrong, sum)
aggregate(is_top_wicket_taker ~ is_mom + prediction_mom, wrong, sum)


logistic_mom_score <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                         + is_top_scorer + is_top_wicket_taker
                         + match_run_share + match_wicket_share
                         + win_by_runs + win_by_wickets
                         + match_runs, data=players_train, trControl=train_control, method="glm", family=binomial())

players_test$pred_prob <- predict(logistic_mom_score, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Add a flag for all-round performance - >=30 runs and at least one wicket
players_train$bat_flag <- 0
players_train$bat_flag[players_train$runs.x >= 30] <- 1
players_train$bowl_flag <- 0
players_train$bowl_flag[players_train$wickets > 0] <- 1

players_test$bat_flag <- 0
players_test$bat_flag[players_test$runs.x >= 30] <- 1
players_test$bowl_flag <- 0
players_test$bowl_flag[players_test$wickets > 0] <- 1


logistic_mom_allround <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                            + is_top_scorer + is_top_wicket_taker
                            + match_run_share + match_runs
                            + bat_flag + bowl_flag, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_allround, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

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

#Find out the economy rate and divide it into classes
players_train$economy <- players_train$runs.y/players_train$over
players_train$economy[is.na(players_train$economy)] <- 0

players_test$economy <- players_test$runs.y/players_test$over
players_test$economy[is.na(players_test$economy)] <- 0

logistic_mom_finish <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                               + is_top_scorer + is_top_wicket_taker
                               + match_run_share + match_runs + finisher_flag
                               + economy, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_finish, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Another random Forest
rf_finish <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                   + is_top_scorer + is_top_wicket_taker
                   + match_run_share + match_runs + finisher_flag
                   + economy, data=players_train, trControl=train_control, method="rf", family=binomial(), ntree = 500)

players_test$pred_prob <- predict(rf_finish, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()

#Get number of dot balls played or conceded
zero_train <- ball_train[ball_train$batsman_runs == 0 & ball_train$total_runs == 0,]
dot_train <- aggregate(batsman_runs ~ batsman + match_id, zero_train, length)
zero_test <- ball_test[ball_test$batsman_runs == 0 & ball_test$total_runs == 0,]
dot_test <- aggregate(batsman_runs ~ batsman + match_id, zero_test, length)

players_train <- merge(players_train, dot_train, by.y = c('match_id', 'batsman'), by.x = c('match_id', 'batsman.x'), all = TRUE)
players_test <- merge(players_test, dot_test, by.y = c('match_id', 'batsman'), by.x = c('match_id', 'batsman.x'), all = TRUE)
colnames(players_train)[31] <- 'dots'
colnames(players_test)[31] <- 'dots'
players_train$dots[is.na(players_train$dots)] <- 100
players_test$dots[is.na(players_test$dots)] <- 100

logistic_mom_finish <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                             + is_top_scorer + is_top_wicket_taker
                             + match_run_share + match_runs
                             + economy, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$pred_prob <- predict(logistic_mom_finish, players_test, type = "prob")
players_test$max_match_prob <- ave(players_test$pred_prob[2], players_test$match_id, FUN = max)
players_test$prediction_mom <- 0
players_test$prediction_mom[players_test$pred_prob[2] == players_test$max_match_prob] <- 1
#Let's see how well it does
table(players_test$is_mom, players_test$prediction_mom)
prf()
  
#Get 'fluency': number of consecutive balls played without a dot
