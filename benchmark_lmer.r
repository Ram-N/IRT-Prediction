setwd("C:/Documents and Settings/u163202/My Documents/Personal/R/Grockit")

# read in valid_test.csv

v.test = read.csv("valid_test.csv", header=TRUE, comment.char = "", colClasses = c('integer','integer','integer','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL'))
#correct,user_id,question_id, (3)
#question_type,group_name,track_name,subtrack_name, (4)
#tag_string, (1)
#round_started_at,answered_at,deactivated_at, (3)
#game_type,num_players, (2)
#date_of_test,question_set_id (2)
#0,100000,3951,0,0,2,10,104 124 171 206,2010-11-04 03:54:44,2010-11-04 03:54:45,2010-11-04 03:54:46,7,1,NULL,3051

# read in valid_test.csv

test = read.csv("test.csv", header=TRUE, comment.char = "", colClasses = c('integer','integer','integer','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL'))
#user_id,question_id,question_type,group_name,track_name,subtrack_name,tag_string,round_started_at,answered_at,deactivated_at,game_type,num_players,date_of_test,question_set_id
#0,5186,0,0,2,10,62 91 127 206,2011-08-26 16:58:47,2011-08-26 16:59:01,2011-08-26 16:59:03,7,1,NULL,3049




# only keep correct and incorrect outcomes
training = training[training$outcome == 1 | training$outcome == 2,]
# read in test data
test = read.csv("test.csv", header=TRUE, as.is=TRUE)

# import lme4 for lmer
require('lme4')

# create a simple logit function for computing probabilities
logit <- function(x) {
  (1 + exp(-x))^-1
}

## create a function for Capped Binomial Deviance
#-[Y*LOG10(E) + (1-Y)*LOG10(1-E)]
#Y is the outcome E is the prediction/prob/guess.
#If E>0.99 E=0.99
#If E<0.01 E = 0.01

cbdavg <- function(y,e) {
#   don't want 0 and 1 in the predictions. trim it.
    e[e>0.99] <- 0.99
    e[e<0.01] <- 0.01

    cbd = -1*(y*(log10(e))+(1-y)*log10(1-e))
    mean(cbd)
}

#Prob of getting it right. qd = question difficulty (0:1)
#ss = student strength (0:1)
## Linear from 1 to ss. then from ss to 0.
probright <- function(ss,qd) {
    if(qd>ss)
        return((1-qd)/(2*(1-ss)))
    else
        return(1-(qd/(2*ss))) }



##-------------------------------------

setwd("C:/Documents and Settings/u163202/My Documents/Personal/R/Grockit")
list.files()

getOption("max.print")
options(max.print=5000)


# read in training data
training = read.csv("valid_training_0.csv", header=TRUE, comment.char = "", colClasses = c('integer','integer','integer','integer','NULL','NULL','integer','integer','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL'))

#find out the num unique students and question_numbers
students <- unique(training$user_id)
quests <- unique(training$question_id)
length(students)
length(quests)


## By each user, we calculate the correct and take its mean
tapply(training$correct,INDEX = training$user_id,FUN=mean)

# using aggregate to calc each user's correct %
stud.agg <- aggregate(x=training$correct, by=list(training$user_id),mean)

rowsum(training$correct,group=training$user_id)

# the table command is a nice way to group by question_id for values of correct
qtabl <- table(training[,c("question_id","correct")])
stabl <- table(training[,c("user_id","correct")])

#from training df, find how many questions each user has answered
as.data.frame(table(training$user_id))

#xtabs takes the columns of interest for data and displays it as a df
stud.correct <- xtabs(~user_id+correct,training)
 per <- stud.correct[,1]/(stud.correct[,2]+stud.correct[,1])

#attaching a numeric student column to percentiles
perdf <- as.data.frame(cbind(sort(students),per))


for(q in unique(test$qid)) {
    print(qdf$qper[qdf$q_id==q] ) }

predictions = rep(0.5, nrow(v.test))
for (row_id in (1:nrow(v.test))) {
  #print(sprintf("Predicting for row %0.0f.",row_id))
  user_id =  v.test[row_id,"user_id"]
  question_id = v.test[row_id,"question_id"]
  smatch <- 0
  qmatch <- 0

  if( match(user_id,stdf$sid,nomatch=0)) #if it is present
     {
         ss <- stdf$ss[stdf$sid == user_id]
         smatch <- 1
     }
  else
     ss <- 0.5

  if( match(question_id,qdf$q_id,nomatch=0)) #if it is present
  {
      qd <- 1 - qdf$qper[qdf$q_id == question_id]
      qmatch <- 1
  }
  else
      qd <- 0.5
  # get the logit of the sum -- if any of the parameters have no estimates/are NA, we effectively use 0 as their estimate
  if(qmatch & smatch)
      predictions[row_id] = probright(ss,qd)

}


##--------------------------------------------------------------------------

## fit models

# fit a basic Rasch-style model for each track separately
track_models = list()
for (track in unique(training$track_name)) {

  print(sprintf("Starting model for track %s.",track))
  rasch = lmer(correct ~ 1 + (1|user_id) + (1|question_id), data=training[training$track_name==track,c("correct","user_id","question_id")], family=binomial, REML=FALSE)

  # get the constant term
  constant = fixef(rasch)["(Intercept)"]

  # get the estimated per-question 'random' effects into a named vector
  question_est = as.vector(t(ranef(rasch)[['question_id']]))
  names(question_est) = rownames(ranef(rasch)[['question_id']])

  # get the estimated per-user 'random' effects into a named vector
  user_est = as.vector(t(ranef(rasch)[['user_id']]))
  names(user_est) = rownames(ranef(rasch)[['user_id']])

  # store these coefficients for later use
  track_models[[as.character(track)]] = list(constant=constant, question_est=question_est, user_est=user_est)
  print(sprintf("Finished with model for %s.",track))
}

## make predictions

# use those model parameters to predict the probability of each test example, using the appropriate track model
predictions = rep(0.5, nrow(test))
for (row_id in (1:nrow(test))) {
  #print(sprintf("Predicting for row %0.0f.",row_id))
  user_id =  test[row_id,"user_id"]
  question_id = test[row_id,"question_id"]
  track = test[row_id, "track_name"]
  #print(sprintf("  using track %s",track))
  model_info = track_models[[as.character(track)]]
  # get the logit of the sum -- if any of the parameters have no estimates/are NA, we effectively use 0 as their estimate
  predictions[row_id] = logit(sum(c(model_info[["constant"]], model_info[["question_est"]][as.character(question_id)], model_info[["user_est"]][as.character(user_id)]), na.rm=TRUE))
}

## output

# output the predictions as a csv to upload to kaggle
submission_df = data.frame(user_id = test$user_id, outcome=predictions)
write.csv(submission_df, file="submission_lmer.csv", row.names=FALSE)
