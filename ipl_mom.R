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
train_control<- trainControl(method="cv", number=10)

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

#Okay, now let's fit a simple decision tree on runs and wickets, and see its performance
library(party)
dt_basic <- ctree(is_mom ~ winning_side + runs.x + wickets, players_train)
players_test$prediction_mom <- predict(dt_basic, players_test)
table(players_test$is_mom, players_test$prediction_mom)

# Output:
#   0    1
# 0 3124   57
# 1   68   92

#Let's include the flags in the decision tree as well
dt_flag <- ctree(is_mom ~ winning_side + is_top_scorer + runs.x + wickets + is_top_wicket_taker, players_train)
players_test$prediction_mom <- predict(dt_flag, players_test)
table(players_test$is_mom, players_test$prediction_mom)
#Oh wow, the accuracy actually fell.

# Output:
#   0    1
# 0 3155   26
# 1   81   79

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

#Maybe MoM's being batsmen or bowlers depends on who was batting first or second
results_train <- mom_train[c(1,6,7)]
results_test <- mom_test[c(1,6,7)]

players_train <- merge(players_train, results_train, by.x = "match_id", by.y = "id", all = TRUE)
players_test <- merge(players_test, results_test, by.x = "match_id", by.y = "id", all = TRUE)
#Fit the model
logistic_mom_result <- train(is_mom ~ winning_side +runs.x + wickets + runs.y
                          + is_top_scorer + is_top_wicket_taker
                          + win_by_runs + win_by_wickets, data=players_train, trControl=train_control, method="glm", family=binomial())
players_test$prediction_mom <- predict(logistic_mom_result, players_test)
table(players_test$is_mom, players_test$prediction_mom)
#Nope. It wasn't actually better. logistic_mom_all is better than logistic_mom_result.

