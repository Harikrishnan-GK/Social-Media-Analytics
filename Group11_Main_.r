#########################################################################

############   SOCIAL MEDIA ANALYTICS - GROUP PROJECT  ##################

#########################################################################


######  4. Predictions of online engagement for (company) tweets  ######


#########       BY             ########

###  Harikrishnan GopalaKrishnan    ###
###  Nixia Sancy John               ###
###  Swasthik Vellingiri Kowsalya   ### 
###  Thiquynh anh NGUYEN            ###
 

########################################################################
#########################################################################


options(scipen = 99)


#################################
##       LOADING PACKAGES      ##
#################################

packages <- c('tidyjson','stringr','httr','jsonlite','dplyr',
              'tidyr','rtweet','SnowballC','slam','tm','Matrix',
              'tidytext','hunspell','purrr','emoji','stringr','textdata',
              'sparklyr','data.table','mltools','lubridate','bit64','caret','leaps','glmnet','corrplot','car')

loadpackages <- function(x){
  if (!require(x, character.only=TRUE)) install.packages(x, repos = "http://cran.us.r-project.org")
  require(x, character.only=TRUE)
  library(x,character.only = TRUE)
}

loaded <- lapply(packages, loadpackages)

library(plotly)

################################################################

############   TWEETS EXTRACTION FROM TWITTER  #################

################################################################



###############################       Setting work directory to 
## SETTING WORKING DIRECTORY ##       load the bearer token for tweets extraction
###############################


setwd('C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Social media analytics/group assignment/Group 11/Group 11')
source("token.R")


###############################
## GETTING userid AND TWEETS ##       Bearer token can also be manually inputted
###############################       if the source file is not loaded

username <- "Marvel"
twt_Bearer_Token = "AAAAAAAAAAAAAAAAAAAAAM%2BOYAEAAAAAzMtgHvjOuPQ7KgHu6cagueIdURw%3DJbmI3SnAj18QQr4YEU6PK65KdRn019O9lEyl93BfU2KCsNOlr0"

get_userid_url <- modify_url(
  url = "https://api.twitter.com",
  path = c("2","users","by","username",username),
  query = list(
    user.fields = "public_metrics"
  )
)

resuserid <- GET(url = get_userid_url,add_headers(Authorization = paste0("Bearer ",twt_Bearer_Token)))
user_id <- fromJSON(httr::content(resuserid,"text"))


##############################
## GETTING FIRST 100 TWEETS ##
##############################


tweets_recent_data = list() #initiate list for adding new page tweets

#URL to get 100 tweets

get_tweets_url = modify_url(
  url = "https://api.twitter.com",
  path = c("2","users",user_id$data$id,"tweets"),
  query = list(
    max_results = 100,
    tweet.fields = "created_at,public_metrics,context_annotations,conversation_id,entities,id,geo,in_reply_to_user_id,lang,referenced_tweets,reply_settings,source,text,withheld"
  )
)

restweets <- GET(url = get_tweets_url,add_headers(Authorization = paste0("Bearer ",twt_Bearer_Token)))
tweets_recent <- fromJSON(httr::content(restweets,"text"))
meta <- fromJSON(httr::content(restweets,"text"))$meta

tweets_recent_data[[1]] <- tweets_recent$data


###########################################
## LOOP TO USE NEXTTOKEN FOR MORE TWEETS ##
###########################################


i = 1            #Initialize variable i to use in loop

while (!is.na(tweets_recent$meta$next_token)) {           #While loop to get all tweets
  i = i + 1
  get_tweets_url = modify_url(
    url = "https://api.twitter.com",
    path = c("2","users",user_id$data$id,"tweets"),
    query = list(
      max_results = 100,
      tweet.fields = "created_at,public_metrics,context_annotations,conversation_id,entities,id,geo,in_reply_to_user_id,lang,referenced_tweets,reply_settings,source,text,withheld",
      pagination_token = tweets_recent$meta$next_token
    )
  )

  restweets <- GET(url = get_tweets_url,add_headers(Authorization = paste0("Bearer ",twt_Bearer_Token)))
  tweets_recent <- fromJSON(httr::content(restweets,"text"))
  tweets_recent_data[[i]] <- tweets_recent$data
  meta <- fromJSON(httr::content(restweets,"text"))$meta
} 

#Saving tweets to RData for future use. Reduces accessing Twitter API more number of times
#save(tweets_recent_data,file = "./data/tweets_220125.RData")
#save(tweets_recent_data,file = "tweets_220201.RData")

load("./data/tweets_220125.RData")


################################################################

##################   CREATING BASE TABLE  ######################

################################################################


df_2 <- bind_rows(tweets_recent_data, .id = "column_label")

# Unnest all columns

df_3 <- df_2 %>%
  select(-entities,-context_annotations) %>%
  unnest(cols = everything(),names_sep = '_')

# remove entities for seperate processing

entity <- df_2 %>%
  select(id,entities)

# Unnest entity

entity_2 <- entity %>%
  unnest(cols = everything(),names_sep = '_') 

for (i in colnames(entity_2)) {
  if(i == "id"){
    entity_3 <- unnest(entity_2,cols = i,names_sep = '_')
  }else{
    entity_3 <- unnest(entity_3,cols = i,names_sep = '_')
  }
}

# Unnest annotations

annotations <- df_2 %>%
  select(id,context_annotations)


annotations_2 <- annotations %>%
     unnest(cols = everything(),names_sep = '_') %>%
        unnest(cols = everything(),names_sep = '_')


#Merge with main table

df_4 <- left_join(df_3,entity_3,by = "id")
#df_4 <- left_join(df_4,annotations_2,by = "id")
df_4$referenced_tweets_type <- ifelse(is.na(df_4$referenced_tweets_type),'original_Tweet',df_4$referenced_tweets_type)

#Keep only unique rows

df_5 <- as.data.frame(unique(df_4)) %>%
          filter(referenced_tweets_type != "retweeted")


#################################################
## PROCESSING TABLE AND FEATURES CREATION      ##
#################################################


# Length of the sentence

df_5$length_of_the_sentence <- nchar(df_5$text)

# Presence of hashtags

df_5$hashtags <-str_extract_all(df_5$text, "#\\S+") # to extract the hashtags into seperate columns

# Changing the values of text without hashtags (column(0)) to NA 

#Ref: https://stackoverflow.com/questions/44766349/how-to-convert-character0-to-na-in-a-list

df_5$hashtags <- purrr::modify_if(
  df_5$hashtags, 
  ~ length(.) == 0, 
  ~ NA_character_
)

# Creating the Column for the presence of hashtags in the text (if hashtags are present - "Yes",not present(NA)- "No")

df_5$hashtags_present <- ifelse(is.na(df_5$hashtags), "No", "Yes")

## Number of hashtags

df_5$Number_of_hashtags <- str_count(df_5$text, pattern = "#")

###EXtract the Emoticons 
# Ref:https://rdrr.io/cran/emoji/man/emoji_extract.html

df_5$Emoticons <- emoji_extract(df_5$text)

#Presence of emoticons

df_5$Emoticons_present <- ifelse(is.na(df_5$Emoticons), "No", "Yes")

# add column presence_url to dataframe

df_5$presence_url <- ifelse(is.na(df_5$entities_urls_url),0,1)

# add column presence_mention to dataframe

df_5$presence_mention <- ifelse(is.na(df_5$entities_mentions_id),0,1)

# add column number_mention to dataframe

df_5$number_mention <- 0

for (i in 1:nrow(df_5)){
  df_5$number_mention[i] <- str_count(df_5$text[i], "\\@")
}

# add column first_person_plural_pronoun to dataframe

toMatch <- c("we", "us", "our", "ourselves")

for (i in 1:nrow(df_5)){
  df_5$first_person_plural_pronoun[i] <- sum(str_count(df_5$text[i], paste0("\\b(", paste(toMatch, collapse="|"), ")\\b")))
}


# add column second_person_plural_pronoun to dataframe

toMatch1 <- c("you", "your", "yours", "yourself", "yourselves")

for (i in 1:nrow(df_5)){
  df_5$second_person_plural_pronoun[i] <- sum(str_count(df_5$text[i], paste0("\\b(", paste(toMatch1, collapse="|"), ")\\b")))
}

save(df_5,file = "./data/df_5.RData")
###########################################
##         SENTIMENT ANALYSIS            ##
###########################################


#removing punctuation and numbers with regular expressions
df_pre1 <- df_5 %>%
              select(id,text,created_at) %>%
                distinct(id,text,created_at) 
df_pre1 <- mutate(df_pre1,message = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#  Tokenization (+ going to lowercase)
df_pre2 <- df_pre1 %>% select(id,created_at,text) %>%
                        mutate(text = tolower(text)) %>%
                        unnest_tokens(output = "word", 
                                     input = text,  
                                     token = "words", 
                                     drop=FALSE,to_lower=TRUE) 


#Sentiment Anaysis with "nrc"
df_sentiment_nrc <- inner_join(df_pre2,get_sentiments("nrc")) %>%
                         rename(sentiment_nrc = sentiment)

sentiments_nrc <- df_sentiment_nrc  %>%
                    group_by(id) %>%
                      summarise(postive = sum(sentiment_nrc == "positive"),
                                trust = sum(sentiment_nrc == "trust"),
                                joy = sum(sentiment_nrc == "joy"),
                                surprise = sum(sentiment_nrc == "surprise"),
                                negative = sum(sentiment_nrc == "negative"),
                                anticipation = sum(sentiment_nrc == "anticipation"),
                                anger = sum(sentiment_nrc == "anger"),
                                disgust = sum(sentiment_nrc == "disgust"),
                                fear = sum(sentiment_nrc == "fear"),
                                sadness = sum(sentiment_nrc == "sadness"))


sentiment_nrc <- df_sentiment_nrc %>%
                  group_by(sentiment_nrc) %>%
                    summarise(count = n())

#Sentiment Anaysis with "afinn"

df_sentiment_afinn <- inner_join(df_pre2,get_sentiments("afinn")) %>%
                          rename(sentiment_value = value)

sentiments_afinn <- df_sentiment_afinn %>%
                      distinct(id,word,.keep_all = TRUE) %>%
                        group_by(id) %>%
                        summarise(mean_sentiment = mean(sentiment_value),
                                  overall_sentiment = sum(sentiment_value)) 

save(sentiments_afinn,file = "./data/sentiments_afinn.RData")
save(sentiment_nrc,file = "./data/sentiment_nrc.RData")

#Manually creating a dictionary to find achievement words

achievement_dictionary <- c("accomplishment","performance","feat","effort","success","advancement",
                            "milestone","attainment","record","reaching","accomplishments","excellence",
                            "masterpiece","achieved","contribution","achieving","successes","merit","achieve",
                            "honor","deserves","improvement","prowess","progress","satisfaction","goal","result",
                            "event","beachhead","foothold","exploit","credit","action","going","sledding","arrival",
                            "squeak","freeing","liberation","release","pass","walk","haymaking","enlisting",
                            "recruitment","smooth","achievements","remarkable","cakewalk","masterstroke","squeaker",
                            "outstanding","exceptional","award","artistic","honors","extraordinary","recognition",
                            "attainments","greatest","skill","dedication","academic","demonstrates","exemplary",
                            "distinction","talent","tremendous","awards","importance","scholarship","emphasis",
                            "significance","talents","accomplished","attaining","accomplishing","impressive",
                            "lifetime","astonishing","excellent","worthy","experience","highlight","mastery",
                            "incredible","recognizing","breakthrough","accomplish","honored","creativity",
                            "sportsmanship","achieves","contributions","attain","triumph","best","technological",
                            "phenomenal","breadth","educational","admirable","skills","demonstrate","amazing",
                            "unparalleled","commitment","culmination","bravery","objective","outcomes","stature",
                            "qualities","unique","undertaking","consideration","feats","awarded","critical","victory",
                            "succeed","acclaim","attained","considerable","earning","abilities","noteworthy",
                            "greatness","quality","courage","demonstrated","completion","prize","results","opportunity",
                            "resilience","successful","history","overall","distinguished","significant","fulfilling",
                            "realization","landmark","relevance","earned","originality","score","accolades","outcome",
                            "innovation","progression","vision","asset","step","equality","completing","enjoyment",
                            "development","efficiency","excellency","fulfillment","aim","securing","materialization",
                            "reach","thing","fulfill","continuation","creation","implementation","quest","fulfil",
                            "embodiment","fruition","work","implementing","occurrence","respect","completed","headway",
                            "emergence","target","outputs","introduction","conclusion","affront","deliver","impairment",
                            "execution","support","ensuring","fulfilment","meet","obtaining","realizations","pursuit",
                            "track record","accomplished fact","fait accompli","close call","close shave","narrow escape",
                            "base on balls","face saver","face saving","materialisation","deliverables","realizable",
                            "finalization","outturn","trophy","loot","challenge","teeth","trophies","title","reward",
                            "mount","appreciation","option","education","prominence","happiness","legacy","compliment",
                            "bracelet","ending","status","ironman","venture","commendation","ramification","dlc","buff",
                            "wealth","perfection","skin","bragging","moment","album","story","game","home","currency",
                            "music","editing","coat","math","champions","collection","preparation","addict","area",
                            "social","stats","commedation","showoff")

#https://stackoverflow.com/questions/60942627/exact-match-from-list-of-words-from-a-text-in-r
achievement_dictionary_pattern <- paste0('\\b', achievement_dictionary, '\\b', collapse = "|")

df_text <- df_5 %>%
              select(id,text) %>%
                    unique()

#the reason 1 is added below is to specify index.False + 1 = 1, which points to 0's position. True + 1 = 2, 
#which points to 1's position

df_text$achievement_word <- c(0, 1)[str_count(df_text$text, achievement_dictionary_pattern) + 1] 


#Manually creating a dictionary to find super heroes in the tweet

top100superheroes_dictionary <- c("SpiderMan","Wolverine","Thor","Cyclops","Iron Man","Daredevil","Hulk","Silver Surfer","Captain America","Thing","Luke Cage","Human Torch","Iron Fist","Nick Fury","Gambit","Hawkeye","Blade","Thunderbolt Ross","Carol Danvers","Sentry","Spider-Woman","Prowler","One of the most underrated and underused comic characters there is.","Nightcrawler","Punisher","Invisible Woman","Iceman","Kaine","Flash Thompson","Ben Reilly","Professor X","Spider-Man ","Eddie Brock","Black Cat","MrFantastic","Hank Pym","Storm","Doctor Strange","Colossus","Moon Knight","Angel","Hercules","Kitty Pryde","Miles Morales","Nova","Namor","Beast","Black Panther","Rogue","Captain Marvel","Molten Man","Cloak","Dagger","MrImmortal","She-Hulk","Black Widow","Ghost Rider (Blaze)","Bucky Barnes","Captain Britain","Union Jack","Silver Sable","Vision","Cable","Bishop","Havok","Wonder Man","Ant-Man (Lang)","Black Bolt","Quicksilver","Spider-Girl (Parker)","Toxin","Scarlet Witch","Odin","Black Knight","Wasp","Jean Grey","Mockingbird","Lockheed","Noh-Varr","Dum Dum Dugan","X-","Ka-Zar","Wiccan","Madrox","Elektra","Swordsman","Rick Jones","Hulkling","Patriot","Banshee","Beta Ray Bill","Cassie Lang","Howard the Duck","Speedball","Phil Coulson","War Machine","White Tiger","Prodigy","Squirrel Girl","Jubilee","Emma Frost","Falcon","winter soldier")

df_superhero_pattern <- paste0('\\b', top100superheroes_dictionary, '\\b', collapse = "|")
df_text$heros_mention <- c(0, 1)[str_count(df_text$text, df_superhero_pattern) + 1] 

#Check if spider-man is mentioned

is_spiderman <- c("SpiderMan","Spider-Man")
is_spiderman_pattern <- paste0('\\b', is_spiderman, '\\b', collapse = "|")
df_text$is_spiderman <- c(0,1)[str_detect(df_text$text,is_spiderman_pattern) + 1]

#Manually creating a dictionary to find days and months in the tweet

daysmonths_dictionary = c("Sunday ","Monday ","Tuesday ","Wednesday ","Thursday ","Friday ","Saturday","January","February","March","April","May","June","July","August","September","October","November","December","weekend","Weekday")
daysmonths_dictionary_pattern <- paste0('\\b',daysmonths_dictionary,'\\b', collapse = "|")
df_text$daysmonths_mention <-c(0,1)[str_detect(df_text$text, daysmonths_dictionary_pattern) +1]

########### Annotations ##########################

movie_tv_pattern <- c("movie","tv")
is_movie <- annotations_2 %>%
                select(id,context_annotations_domain_name) %>%
                  mutate(context_annotations_domain_name = tolower(context_annotations_domain_name)) %>%
                    filter(str_detect(context_annotations_domain_name,paste0('\\b',movie_tv_pattern,'\\b', collapse = "|"))) %>%
                        rename(is_movie_tv = context_annotations_domain_name)

is_movie <- is_movie[!duplicated(is_movie$id),]

nbr_annotations <- annotations_2 %>%
                      group_by(id) %>%
                        summarise(nbr_annotations = n())


                  ############################################
                  ###         BASE TABLE CREATION          ###
                  ############################################

# Get unique tweet_ids for base table


create_base_table <- function(){
rm(base_table)
base_table <- df_5 %>%
                select(id) %>%
                  unique()

#Merge public metrics

base_table <- df_5 %>%
                select(id,lang,source,text,created_at,public_metrics_retweet_count,public_metrics_reply_count,public_metrics_like_count,public_metrics_quote_count,length_of_the_sentence) %>%
                  distinct() %>%
                    rename(nbr_retweet = public_metrics_retweet_count,nbr_like = public_metrics_like_count,nbr_reply = public_metrics_reply_count, nbr_quote = public_metrics_quote_count) %>%
                    inner_join(base_table,by="id")

#merge other metrics

# Column Variables

colvars = names(df_5)

# Get the first ID

start_loc_ad_v = match("hashtags_present",colvars)

# Get the second ID

end_loc_ad_v = match("second_person_plural_pronoun",colvars)

# Subset range

additional_variables <- df_5 %>%
                          select(id,colvars[start_loc_ad_v]:colvars[end_loc_ad_v]) %>%
                            group_by(id) %>%
                              unique()

base_table <- inner_join(base_table,additional_variables,by="id") 

# add column Tweets include a picture to dataframe

tweets_picture <- df_5 %>%
                    select(id,entities_urls_images) %>%
                        mutate(tweets_picture = ifelse(entities_urls_images != 'NULL',1,0)) %>%
                            group_by(id) %>%
                              summarise(nbr_tweet_pictures = sum(tweets_picture))


base_table <- inner_join(base_table,tweets_picture,by = "id")

base_table <- base_table %>%
                select(-Emoticons,-hashtags_present)


              ##############################################
              ###   MERGING SENTIMENTS WITH BASE TABLE   ###
              ##############################################

# Merge NRC Sentiments

base_table <- base_table %>%
                left_join(sentiments_nrc,by="id")

base_table[is.na(base_table)] <- 0

# Merge AFINN Sentimes

base_table <- base_table %>%
        left_join(sentiments_afinn,by="id")

base_table[is.na(base_table)] <- 0

#Achievement words and hero mention

base_table <- base_table %>%
                left_join(df_text[,c("id","achievement_word","heros_mention","is_spiderman")],by="id")


                ##############################################
                ###   MERGING ANNOTATIONS WITH BASETABLE   ###
                ##############################################


base_table <- left_join(base_table,is_movie,by="id")

base_table$is_movie_tv <- ifelse(is.na(base_table$is_movie_tv) | base_table$is_movie_tv == 'NULL',0,1)

base_table <- left_join(base_table,nbr_annotations,by="id")


                ##############################################
                ###     MERGING ENTITIES WITH BASETABLE    ###
                ##############################################


merge_entity <- entity_3 %>%
                  group_by(id) %>%
                  summarise(entities_annotations_start = mean(entities_annotations_start),
                         entities_annotations_end = mean(entities_annotations_end),
                         entities_annotations_probability = mean(entities_annotations_probability),
                         entities_hashtags_start = mean(entities_hashtags_start),
                         entities_hashtags_end = mean(entities_hashtags_end),
                         entities_mentions_start = mean(entities_mentions_start),
                         entities_mentions_end = mean(entities_mentions_end),
                         entities_urls_start = mean(entities_urls_start),
                         entities_urls_end = mean(entities_urls_end))
base_table <- left_join(base_table,merge_entity,by="id")
base_table[is.na(base_table)] <- 0


######################################################################

#Extracting the date and finding the day of the week 

base_table$date <- as.Date(base_table$created_at)
base_table$dayofweek = wday(base_table$date)

base_table$day = ifelse(base_table$dayofweek > 5, "Weekend","Weekday")

#Finding the presence of Question marks and Cash tags

base_table$question = as.integer(str_detect(base_table$text, "[?]"))
base_table$cashtag = as.integer(str_detect(base_table$text, "[$]"))

#Calculating days since tweeted

date_extracted = Sys.Date()
base_table$Days_Since_Posted = ymd(date_extracted) - ymd(base_table$date)

#Encode Source

base_table$source <- as.factor(base_table$source)
source_encode <- base_table %>%
                  select(id,source) %>%
                    mutate(source = as.factor(source))
                      
source_encode <- one_hot(as.data.table(source_encode))
base_table <- inner_join(base_table,source_encode,by="id")

base_table[is.na(base_table)] <- 0

#Treat outliers before calculating for engagement_rate

base_table[base_table$nbr_quote > (3* quantile(base_table$nbr_quote,probs = 0.75)),"nbr_quote"] <- mean(base_table$nbr_quote)
base_table[base_table$nbr_retweet > (3* quantile(base_table$nbr_retweet,probs = 0.75)),"nbr_retweet"] <- mean(base_table$nbr_retweet)
base_table[base_table$nbr_reply > (3* quantile(base_table$nbr_reply,probs = 0.75)),"nbr_reply"] <- mean(base_table$nbr_reply)
base_table[base_table$nbr_like > (3* quantile(base_table$nbr_like,probs = 0.75)),"nbr_like"] <- mean(base_table$nbr_like)


##############################################
###      CALCULATING TARGET VARIABLE       ###
##############################################


base_table <- base_table %>%
  mutate(engagement_rate = ((1* nbr_quote + 0.75* nbr_retweet +0.5* nbr_reply+ 0.25*nbr_like)/(user_id$data$public_metrics$followers_count)) * 100)
 #         mutate(engagement_rate = nbr_quote + nbr_retweet + nbr_reply + nbr_like)
return(base_table)

}

base_table <- create_base_table()

save(base_table,file="./data/base_Table.RData")


################################################################

##################      MODEL BUILDING    ######################

################################################################


base_table$Emoticons_present <- ifelse(base_table$Emoticons_present == "Yes",1,0)
base_table$day <- ifelse(base_table$day == "Weekend",1,0)
base_table$Days_Since_Posted <- as.integer(str_replace(base_table$Days_Since_Posted," days",""))

model_table <- base_table %>%
                select(-id,-lang,-source,-text,-created_at,-date,-nbr_retweet,-nbr_like,-nbr_reply,-nbr_quote,-cashtag)

save(model_table,file="./data/model_table.RData")
load(".data/base_Table.RData")

## Modelling 
#https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r

#Best Subset selection
##regfit <- regsubsets(engagement_rate ~.,model_table,nvmax = 5)
###summary(regfit)


#Split data into Train and Test

set.seed(123)
index = sample(1:nrow(model_table), 0.7*nrow(model_table)) 

train_set <- model_table[index,]
test_set = model_table[-index,]

x_train <- train_set[,-length(train_set)]
y_train <- train_set[,length(train_set)]

x_test <- test_set[,-length(test_set)]
y_test <- test_set[,length(test_set)]


####################################
###      LINEAR REGRESSION       ###
####################################

lm_mod <- lm(engagement_rate ~ .,train_set)
lm_pred_train <- predict(lm_mod, newdata = x_train)
lm_pred_test <- predict(lm_mod, newdata = x_test)
summary(lm_mod)

####################################
###      RIDGE REGRESSION        ###
####################################

lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(as.matrix(x_train), y_train, alpha = 0, lambda = lambdas)
optimal_lambda_r <- cv_ridge$lambda.min

ridge_mod <- glmnet(x_train,y_train,alpha = 0,nlambda = 25,family = "gaussian",lambda = lambdas)
ridge_pred_train <- predict(ridge_mod,s=optimal_lambda_r,newx = as.matrix(x_train))
ridge_pred_test <- predict(ridge_mod,s=optimal_lambda_r,newx = as.matrix(x_test))

####################################
###      LASSO REGRESSION        ###
####################################

cv_lasso <- cv.glmnet(as.matrix(x_train), y_train, alpha = 1, lambda = lambdas)
optimal_lambda_l <- cv_ridge$lambda.min

lasso_mod <- glmnet(x_train,y_train,alpha = 1,family = "gaussian")
lasso_pred_train <- predict(lasso_mod,s=optimal_lambda_l,newx = as.matrix(x_train))
lasso_pred_test <- predict(lasso_mod,s=optimal_lambda_l,newx = as.matrix(x_test))


##################################
###    EVALUATING MODELS       ###
##################################

### Linear regression evaluation

#Step 1 - create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = summary(model)$r.squared
  adj_r2 = summary(model)$adj.r.squared
  return(data.frame(
    lr_R2 = summary(model)$r.squared,
    lr_Adjusted_r = summary(model)$adj.r.squared,
    lr_RMSE = sqrt(sum(resids2)/N)
  ))
}

# Eval Metrics for Ridge and Lasso

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = RMSE(pred = predicted,obs = true)
  
  # Model performance metrics
  
  data.frame(
    RMSE = RMSE,
    R2 = R_square
  )
}

# Step 2 - predicting and evaluating the model on train data

eval_lm_train <- eval_metrics(lm_mod, train_set, lm_pred_train, target = 'engagement_rate')
eval_ridge_train <- eval_results(y_train, ridge_pred_train, x_train) %>% rename(ridge_R2 = R2, ridge_RMSE = RMSE)
eval_lasso_train <- eval_results(y_train, lasso_pred_train, x_train) %>% rename(lasso_R2 = R2, lasso_RMSE = RMSE)

# Step 3 - predicting and evaluating the model on test data

eval_lm_test <- eval_metrics(lm_mod, test_set, lm_pred_test, target = 'engagement_rate')
eval_ridge_test <- eval_results(y_test, ridge_pred_test, x_test) %>% rename(ridge_R2 = R2, ridge_RMSE = RMSE)
eval_lasso_test <- eval_results(y_test, lasso_pred_test, x_test) %>% rename(lasso_R2 = R2, lasso_RMSE = RMSE)

#Combine results

train_eval <- as.data.frame(c(eval_lm_train,eval_ridge_train,eval_lasso_train))
test_eval <- as.data.frame(c(eval_lm_test,eval_ridge_test,eval_lasso_test))


##################################
###    CORRELATION PLOTS       ###
##################################


cor <- cor(model_table)
corrplot(cor)

##################################
###      PLOTS FOR MODEL       ###
##################################

plot(lm_mod)



################################################################

##################      PLOTS FOR SHINY    ######################

################################################################


###########################################
##    WORD CLOUD CREATION FOR SHINY      ##
###########################################

#setwd('C:/Users/tnguyen17/OneDrive - IESEG/Documents/Courses/SMA/Group project')

# load the text

text <- sapply(df_5[,13],function(x) iconv(x, 'utf8', 'ascii',""))
tweet_text <- VCorpus(VectorSource(text))

# inspect the corpus

tweet_text[[1]]
as.character(tweet_text[[1]])

# Existing transformer functions
# We can apply these transformer functions to the entire corpus using a 'mapper', tm_map

tweet_text <- tm_map(tweet_text, removePunctuation)
tweet_text <- tm_map(tweet_text, removeNumbers)
tweet_text <- tm_map(tweet_text, stripWhitespace)

# Create new transformation functions

tweet_text <- tm_map(tweet_text,content_transformer(tolower))

gsubtransfo <- content_transformer(function(x,from, to) gsub(from, to, x))
tweet_text <- tm_map(tweet_text, gsubtransfo, "@\\w+",  "")

# Remove stopwords

forremoval <- stopwords('english')
tweet_text <- tm_map(tweet_text, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 

# Stemming
#tweet_text <- tm_map(tweet_text,stemDocument)

# Make the document-term matrix. 

dtm <- TermDocumentMatrix(tweet_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# create word cloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


###########################################
##            OTHER PLOTS                ##
###########################################

options(repr.plot.width=4, repr.plot.height=4)

load("./data/base_Table.RData")

##################################
###      PLOTS FOR MODEL       ###
##################################

# % Percentage of Tweets with Number of Hashtags

fig1 = base_table %>%
  group_by(Number_of_hashtags) %>%
  summarise(Count = n())

fig1_plot <- plot_ly(fig1, labels =~Number_of_hashtags, values = ~Count, type = 'pie',textinfo='label+percent')
fig1_plot <- fig1_plot %>% layout(title = '% Percentage of Tweets with Number of Hashtags',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig1_plot


### Average Engagement rate for hastags

fig_1.1 = base_table %>%
  group_by(Number_of_hashtags) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))  

fig_1.2 <- plot_ly(data=fig_1.1, x=~Number_of_hashtags,
                   y = ~Average_Engagement_Score) %>% 
  add_trace(type = "bar",
            x = ~Number_of_hashtags,
            y = ~Average_Engagement_Score,
            marker = list(color = c("silver",
                                    "red",
                                    "silver",
                                    "red",
                                    "silver",
                                    "silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Engagement Rate based on Number of Hashtags",
         xaxis = list(title = "Number of Hashtags",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))
fig_1.2


# % of Tweets with and without URL in Tweets

presence_url = base_table %>%
  group_by(presence_url) %>%
  summarise(count = n()) %>% 
  plot_ly(labels = ~presence_url, values = ~count,textinfo='label+percent') %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "% of Tweets with and without URL in Tweets",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

presence_url


# Engagement Rate based on presence of URL in Tweets

presence_url_mean = base_table %>%
  group_by(presence_url) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

p6 <- plot_ly(data=presence_url_mean, x=~presence_url,
              y = ~Average_Engagement_Score) %>% 
  add_trace(type = "bar",
            x = ~presence_url,
            y = ~Average_Engagement_Score,
            marker = list(color = c("silver",
                                    "red",
                                    "silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Engagement Rate based on presence of URL in Tweets",
         xaxis = list(title = "Presence of URL",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))
p6


# % of Tweets with and without emoticons in Tweets

presence_emoticons_plt = base_table %>%
  group_by(Emoticons_present) %>%
  summarise(count = n()) %>% 
  plot_ly(labels = ~Emoticons_present, values = ~count,textinfo='label+percent') %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "% of Tweets with and without Emoticons in Tweets",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

presence_emoticons_plt


# Engagement Rate based on presence of URL in Tweets

presence_Emoticons_mean = base_table %>%
  group_by(Emoticons_present) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

p6 <- plot_ly(data=presence_Emoticons_mean, x=~Emoticons_present,
              y = ~Average_Engagement_Score) %>% 
  add_trace(type = "bar",
            x = ~Emoticons_present,
            y = ~Average_Engagement_Score,
            marker = list(color = c("silver",
                                    "silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Engagement Rate based on presence of Emoticons in Tweets",
         xaxis = list(title = "Presence of Emoticons",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))
p6


# "Engagement Rate based on the number of mentions in Tweets"

presence_mentions_mean = base_table %>%
  group_by(number_mention) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

p8 <- plot_ly(data=presence_mentions_mean, x=~number_mention,
              y = ~Average_Engagement_Score) %>% 
  add_trace(type = "bar",
            x = ~number_mention,
            y = ~Average_Engagement_Score,
            marker = list(color = c("silver","red","silver","silver","silver","silver","silver","silver","silver","silver","silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Engagement Rate based on the number of mentions in Tweets",
         xaxis = list(title = "Number of @ mentions",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))
p8

# DAYS SINCE TWEETED vs ENGAGEMENT RATE

engagement_range = base_table %>%
  group_by(Days_Since_Posted) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

p9 <- plot_ly(engagement_range, x = ~Days_Since_Posted, y = ~Average_Engagement_Score, name = 'trace 0', type = 'scatter', mode = 'lines') %>% 
  layout(title = "Average Engagement Rate based on Days Since tweeted",
         xaxis = list(title = "Days Since Tweeted",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))

p9



## NUMBER OF TWEETS BASED ON THE DAYS OF THE WEEK 

dayofweek_tweets <- base_table %>%
  group_by(dayofweek) %>%
  summarise(Number_tweets = n())
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 1] <- "1 - Sunday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 2] <- "2 - Monday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 3] <- "3 - Tuesday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 4] <- "4 - Wednesday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 5] <- "5 - Thursday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 6] <- "6 - Friday"
dayofweek_tweets$dayofweek[dayofweek_tweets$dayofweek == 7] <- "7 - Saturday"
p10 <- plot_ly(data=dayofweek_tweets, x=~dayofweek,
              y = ~Number_tweets) %>%
  add_trace(type = "bar",
            x = ~dayofweek,
            y = ~Number_tweets,
            marker = list(color = c("silver","silver","red","silver","red","silver","silver"),
                          opacity = rep(0.7, 7))) %>%
  layout(title = "Number of Tweets based on Days of the week",
         xaxis = list(title = "Days of the week",
                      zeroline = FALSE),
         yaxis = list(title = "Number of Tweets",
                      zeroline = FALSE))
p10


## Average Engagement score BASED ON THE DAYS OF THE WEEK 

dayofweek_engagement = base_table %>%
  group_by(dayofweek) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 1] <- "1 - Sunday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 2] <- "2 - Monday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 3] <- "3 - Tuesday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 4] <- "4 - Wednesday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 5] <- "5 - Thursday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 6] <- "6 - Friday"
dayofweek_engagement$dayofweek[dayofweek_engagement$dayofweek == 7] <- "7 - Saturday"

p11 <- plot_ly(data=dayofweek_engagement, x=~dayofweek,
               y = ~Average_Engagement_Score) %>% 
  add_trace(type = "bar",
            x = ~dayofweek,
            y = ~Average_Engagement_Score,
            marker = list(color = c("red","red","red","silver","silver","silver","silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Average Engagement Score based on days of the week",
         xaxis = list(title = "Days of the week",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement",
                      zeroline = FALSE))
p11


## Source from which the tweets are posted

source_count = base_table %>%
  group_by(source) %>%
  summarise(count = n())
plot_ly(data=source_count, x=~source,
        y = ~count) %>% 
  add_trace(type = "bar",
            x = ~source,
            y = ~count,
            marker = list(color = c("silver","red","silver","silver","silver","silver","red"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Source from which the tweets are posted",
         xaxis = list(title = "Source",
                      zeroline = FALSE),
         yaxis = list(title = "Count of Tweets",
                      zeroline = FALSE))

## PLOTS FOR SENTIMENT ANALYSIS

#Distribution of Sentiment scores present in tweets- AFINN DICTIONARY

base_table %>% plot_ly(x=~overall_sentiment) %>% 
  add_trace(type = "histogram",
            x = ~overall_sentiment,
            marker = list(color = c("black","black","black","black","black","black","black","red","black","black","red"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Distribution of Sentiment Score in Tweets- afinn",
         xaxis = list(title = "Sentiment Score",
                      zeroline = FALSE))


##Average Engagement Rate based on Sentiment Score - AFINN DICTIONARY

base_table %>% 
  group_by(overall_sentiment) %>% 
  summarise(avg_engagement = mean(engagement_rate)) %>% 
  plot_ly(x=~overall_sentiment,
          y = ~avg_engagement) %>% 
  add_trace(type = "bar",
            x = ~overall_sentiment,
            y = ~avg_engagement,
            marker = list(color = c("silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","silver","red","silver","silver","silver","silver","silver","red"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Average Engagement Rate based on Sentiment Score - afinn",
         xaxis = list(title = "Sentiment score",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))


#Code required for below plots

library(tidyr)
library(dplyr)
library(readr)

sentiment_data = base_table

keep <- c("id","postive","trust","joy","surprise","negative","anticipation","anger","disgust","fear","sadness","engagement_rate")
sentiment_data = sentiment_data[,colnames(sentiment_data)  %in% keep]

sentiment_data_count = sentiment_data %>% 
  pivot_longer(!c(id,engagement_rate), names_to = "sentiments_", values_to = "count")
sentiment_data_count = as.data.frame(sentiment_data_count)


# Number of Sentiments present in Tweets - nrc dictionary

sentiment_data_count %>% 
  group_by(sentiments_) %>%
  summarise(sentiment_count = sum(count)) %>% 
  plot_ly(labels = ~sentiments_, values = ~sentiment_count,textinfo='label+percent') %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "% of Sentiments present in Tweets - nrc",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


### Average Engagement Rate per Sentiments

sentiment_data_count %>% 
  group_by(sentiments_) %>% 
  filter(!count == 0) %>% 
  summarise(sentiment_engage = mean(engagement_rate)) %>%
  plot_ly(x=~sentiments_,
          y = ~sentiment_engage) %>% 
  add_trace(type = "bar",
            x = ~sentiments_,
            y = ~sentiment_engage,
            marker = list(color = c("silver","red","silver","red","silver","silver","silver","silver","silver","silver"),
                          opacity = rep(0.7, 7))) %>% 
  layout(title = "Average Engagement Rate per Sentiments - nrc",
         xaxis = list(title = "Sentiments",
                      zeroline = FALSE),
         yaxis = list(title = "Average Engagement Rate",
                      zeroline = FALSE))



##################################
###      END OF THE SCRIPT    ###
##################################

#  Script for shiny and Markdown are available in main folder. 
#  PDF output of Markdown is also available in the main folder.
