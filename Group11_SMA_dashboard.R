
###########################################################################################
#     Prediction of Online engagement for Marvel Entertainment Tweets                     #
###########################################################################################





###########################################################################################
#                                       Team-Group11                                      #
###########################################################################################

###### Nixia Sancy JOHN              ##########
###### Harikrishnan GOPALAKRISHNAN   ##########
###### Swasthik Vellingiri KOWSALYA  ##########
###### Thi Quynh Anh NGUYEN          ##########

###########################################################################################
#                                Working Directory                                        #
###########################################################################################
#Set the working directory

setwd("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Social media analytics/group assignment/Group 11/Group 11")


###########################################################################################
#                                       Libraries                                         #
###########################################################################################
library(shinyWidgets)
library(shiny)
#library(shinySignals)
library(shinydashboard)
library(plotly)
library(tidyr)
library(dplyr)
library(readr)

###########################################################################################
#                     Importing Datasets and Dataframes                                   #
###########################################################################################


load("./data/base_Table.RData")
load("./data/model_Table.RData")
load("./data/df_5.RData")




#################################################################################
##############################World-Cloud Plot###################################
#################################################################################
# Load library

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("httr")

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

#################################################################################
##############################Overiew Plot#######################################
#################################################################################


packages <- c('tidyjson','stringr','httr','jsonlite','dplyr',
              'tidyr','rtweet','SnowballC','slam','tm','Matrix',
              'tidytext','hunspell','purrr','emoji','stringr','textdata',
              'sparklyr','data.table','mltools','lubridate','bit64','caret','leaps','glmnet','corrplot','car')

loadpackages <- function(x){
  if (!require(x, character.only=TRUE)) install.packages(x,character.only=TRUE,dependencies = TRUE, repos = "http://cran.us.r-project.org")
  require(x, character.only=TRUE)
  library(x,character.only = TRUE)
}

loaded <- lapply(packages, loadpackages)


#Set working directory
#source("token.R")

#Get UserID
username <- "Marvel"

BearerToken <- "AAAAAAAAAAAAAAAAAAAAAAR3YAEAAAAA1ZK3od4UD4%2B%2FF3GjlKu2NmOGpHE%3D2RnwTCAbfJA4qRxtfv9DjNgPtlRluU2w4iCcQ9XI7gax33b2A1"

get_userid_url <- modify_url(
  url = "https://api.twitter.com",
  path = c("2","users","by","username",username),
  query = list(
    user.fields = "public_metrics"
  )
)

resuserid <- GET(url = get_userid_url,add_headers(Authorization = paste0("Bearer ",BearerToken)))
user_id <- fromJSON(httr::content(resuserid,"text"))


followers_count <- user_id$data$public_metrics$followers_count 
following_count <- user_id$data$public_metrics$following_count
tweet_count <- user_id$data$public_metrics$tweet_count
listed_count <- user_id$data$public_metrics$listed_count


#################################################################################
##############################Other Plots########################################
#################################################################################

#Engagement Rate based on Number of Hashtags
df = base_table %>%
  group_by(Number_of_hashtags) %>%
  summarise(Average_Engagement_Score = mean(engagement_rate))

noh<- unique(base_table$Number_of_hashtags)
noh <- sort(noh)



#Sentiment
sentiment_data = base_table
keep <- c("id","positive","trust","joy","surprise","negative","anticipation","anger","disgust","fear","sadness","engagement_rate")
sentiment_data = sentiment_data[,colnames(sentiment_data)  %in% keep]

sentiment_data_count = sentiment_data %>% 
  pivot_longer(!c(id,engagement_rate), names_to = "sentiments_", values_to = "count")
sentiment_data_count = as.data.frame(sentiment_data_count)




###########################################################################################
#                        User Interface                                                   #
###########################################################################################




ui <- dashboardPage(skin = 'red',
      
                    
                  
    
    dashboardHeader(title = "Marvel Entertainment"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(" Overview", tabName = "Overview_info"),
            menuItem(" Twitter Stats", tabName = "Stats_info"),
            menuItem("Model", tabName = "Model_info"),
            menuItem('Profiling',tabName="Profiling_Info"),
            menuItem('Sentiments',tabName="Sentiments_info")
        )
    ),
    
    
    dashboardBody(
                    tags$img(
                      src = "https://p4.wallpaperbetter.com/wallpaper/947/702/550/daredevil-marvel-comics-wallpaper-preview.jpg",
                      style = 'position: absolute',
                      width = 1700,
                      height = 900,

                    ),
      
        tabItems(
          
          
          tabItem("Overview_info",
                  
                  
                  fluidRow(
                    box(height = 600,width = 1,background = 'black')
                    ),
                    
                  
                  fluidRow(
                    width=6,
                    box(
                      height=270,
                      h1("App Description:"),
                      h1("This app provides the overall evaluation and interpretation of
features that drive the engagement of twitter users with the Marvel Entertainment Company")
                     
                      
                    ),
                    box(
                      height=270,
                      h2(valueBoxOutput("followers_count_box")),
                      h2(valueBoxOutput("following_count_box")),
                      h2(valueBoxOutput("tweet_count_box")),
                      h2(valueBoxOutput("listed_count_box")),
                      
                      
                    )
                  ),
                  
          ),
          
          
          
            
            
            
            
            
            tabItem("Stats_info",


                    fluidRow(
                        box(width = 12,
                            status = "info",
                            solidHeader = TRUE,
                            skin='red',
                            title = "Word Cloud",
                            sliderTextInput(
                                inputId = "wc",
                                label = "Choose a frequency value:", 
                                choices = seq(from = 1,
                                              to = 5000,
                                              by = 500),
                                grid = TRUE),
                                plotOutput("WCplot") 
                             ),
                    ),
                    
                    fluidRow(
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Number of Tweets based on Days of the week",
                          textOutput(
                            outputId = "Day"
                          ),
                          plotlyOutput("Dayplot")
                      ),
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Sources from which the tweets are posted",
                          textOutput(
                            outputId = "Source"
                          ),
                          plotlyOutput("Sourceplot")
                      )
                      
                      
                    )
            ),
                        
                 

            tabItem("Model_info",

                    fluidRow(
                      box(height = 650,width = 1,background = 'black')
                    ),

                    fluidRow(
                      width =6,
                      box(
                        
                        h1("Root Mean Square Error of the Train Models"),
                        h2(valueBoxOutput("lr_RMSE_train")),
                        h2(valueBoxOutput("ridge_RMSE_train")),
                        h2(valueBoxOutput("lasso_RMSE_train"))
                    
                        
                      ),
                      
                      box(
                        
                        h1("Root Mean Square Error of the Test Models"),
                        h2(valueBoxOutput("lr_RMSE_test")),
                        h2(valueBoxOutput("ridge_RMSE_test")),
                        h2(valueBoxOutput("lasso_RMSE_test"))
                        
                        
                      ),
                      
                      
                    ),

            

            ),
            tabItem("Profiling_Info",



                        fluidRow(
                          
                          box(width=6,
                              status = "info",
                              solidHeader = TRUE,
                              title = "Percentage of Tweets with Number of Hashtags",
                              textOutput(
                                outputId = "PT"
                              ),
                              plotlyOutput("PT1plot")
                          ),
                          
                            box(width=6,
                            status = "info",
                            solidHeader = TRUE,
                            title = "Engagement Rate based on Number of hashtags",
                            textOutput(
                            outputId = "ER"
                            ),
                            plotlyOutput("ER1plot")
                            ),



                    ),
                    
                    fluidRow(
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Average Engagement Score based on Tweeted Days",
                          textOutput(
                            outputId = "PT2"
                          ),
                          plotlyOutput("PT2plot")
                      ),
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Average Engagement Rate based on Days Since tweeted",
                          textOutput(
                            outputId = "ER2"
                          ),
                          plotlyOutput("ER2plot")
                      ),
                      
                      
                      
                    ),
                    
                    
                    fluidRow(
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Percentage of Tweets with and without URL in Tweets",
                          textOutput(
                            outputId = "PT3"
                          ),
                          plotlyOutput("PT3plot")
                      ),
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Engagement Rate based on presence of URL in Tweets",
                          textOutput(
                            outputId = "ER3"
                          ),
                          plotlyOutput("ER3plot")
                      ),
                      
                      
                      
                    )
                    
                    
            ),
            tabItem("Sentiments_info",


                    fluidRow(
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Average Engagement Rate based on Sentiment Score - AFINN DICTIONARY",
                          textOutput(
                            outputId = "AF1"
                          ),
                          plotlyOutput("AF1plot")
                      ),
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Distribution of Sentiment scores present in tweets- AFINN DICTIONARY",
                          textOutput(
                            outputId = "AF2"
                          ),
                          plotlyOutput("AF2plot")
                      ),
                      
                      
                      
                    ),
                    
                    fluidRow(
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Number of Sentiments present in Tweets - NRC dictionary",
                          textOutput(
                            outputId = "NRC1"
                          ),
                          plotlyOutput("NRC1plot")
                      ),
                      
                      box(width=6,
                          status = "info",
                          solidHeader = TRUE,
                          title = "Average Engagement Rate per Sentiments - NRC dictionary",
                          textOutput(
                            outputId = "NRC2"
                          ),
                          plotlyOutput("NRC2plot")
                      )
                      
                      
                      
                    )
                    
                    
                 )


            )


        )
    
   )
        
    
    

###########################################################################################
#                                  Server                                                 #
###########################################################################################

server <- function(input, output) {
    wordcloud_rep <- repeatable(wordcloud)
    
    output$WCplot<-renderPlot({
        c<-d%>% filter(freq <= input$wc)
                set.seed(1234)
                wordcloud_rep(words = c$word, freq = c$freq, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(12, "Dark2"))
        
    })

    output$ER1plot<-renderPlotly({
        
        
        p5 <- plot_ly(data=df , x=~Number_of_hashtags,
                      y = ~Average_Engagement_Score) %>%
            add_trace(type = "bar",
                      x = ~Number_of_hashtags,
                      y = ~Average_Engagement_Score,
                      marker = list(color = c("red",
                                              "silver",
                                              "red",
                                              "silver",
                                              "red",
                                              "silver"),
                                    opacity = rep(0.7, 7))) %>%
            layout(title = "Engagement Rate based on Number of Hashtags",
                   xaxis = list(title = "Number of Hashtags",
                                zeroline = FALSE),
                   yaxis = list(title = "Average Engagement Rate",
                                zeroline = FALSE))
        p5

    })
    
    output$PT1plot<-renderPlotly({
      
      
      fig1 = base_table %>%
        group_by(Number_of_hashtags) %>%
        summarise(Count = n())
      
      
      
      fig1_plot <- plot_ly(fig1, labels =~Number_of_hashtags, values = ~Count, type = 'pie')
      fig1_plot <- fig1_plot %>% layout(title = '% Percentage of Tweets with Number of Hashtags',
                                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      
      fig1_plot
      
      
    })
    
    output$ER2plot<-renderPlotly({
      
      
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
      
    })
    
    output$PT2plot<-renderPlotly({
      
      
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
        layout(title = "Average Engagement Score based on Tweeted Days",
               xaxis = list(title = "Days of the week",
                            zeroline = FALSE),
               yaxis = list(title = "Average Engagement",
                            zeroline = FALSE))
      p11
      
      
    })
    
    output$ER3plot<-renderPlotly({
      
      
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
      
    })
    
    output$PT3plot<-renderPlotly({
      
      
      presence_url = base_table %>%
        group_by(presence_url) %>%
        summarise(count = n()) %>% 
        plot_ly(labels = ~presence_url, values = ~count,textinfo='label+percent') %>% 
        add_pie(hole = 0.6) %>% 
        layout(title = "% of Tweets with and without URL in Tweets",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      presence_url
      
    })
    output$Dayplot<-renderPlotly({
      
      
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
      p9 <- plot_ly(data=dayofweek_tweets, x=~dayofweek,
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
      p9
      
    })
    output$AF1plot<-renderPlotly({
      
      
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
      
    })
    output$AF2plot<-renderPlotly({
      
      
      base_table %>% plot_ly(x=~overall_sentiment) %>% 
        add_trace(type = "histogram",
                  x = ~overall_sentiment,
                  marker = list(color = c("black","black","black","black","black","black","black","red","black","black","red"),
                                opacity = rep(0.7, 7))) %>% 
        layout(title = "Distribution of Sentiment Score in Tweets- afinn",
               xaxis = list(title = "Sentiment Score",
                            zeroline = FALSE))
      
    })
    output$NRC1plot<-renderPlotly({
      
      sentiment_data_count %>% 
        group_by(sentiments_) %>%
        summarise(sentiment_count = sum(count)) %>% 
        plot_ly(labels = ~sentiments_, values = ~sentiment_count,textinfo='label+percent') %>% 
        add_pie(hole = 0.6) %>% 
        layout(title = "% of Sentiments present in Tweets - nrc",  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))      

      
    })
    output$NRC2plot<-renderPlotly({
      
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
      
      
    })
    output$Sourceplot<-renderPlotly({
      
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
        layout(title = "Sources from which the tweets are posted",
               xaxis = list(title = "Source",
                            zeroline = FALSE),
               yaxis = list(title = "Count of Tweets",
                            zeroline = FALSE))     
      
      
    })
    
    output$followers_count_box <- renderValueBox({
      
      valueBox(
        subtitle ="Followers Count",
        paste(as.character(followers_count)),
        color = "red"
      )
    })
    
    output$following_count_box <- renderValueBox({
      
      valueBox(
        subtitle ="Following Count",
        paste(as.character(following_count)),
        color = "red"
      )
    })
    
    output$tweet_count_box <- renderValueBox({
      
      valueBox(
        subtitle ="Tweets Count",
        paste(as.character(tweet_count)),
        color = "red"
      )
    })
    
    output$listed_count_box <- renderValueBox({
      
      valueBox(
        subtitle ="Listed Count",
        paste(as.character(listed_count)),
        color = "red"
      )
    })
    
    output$Corrplot<-renderPlot({
      cor <- cor(model_table)
      corrplot(cor, method="color")
      
    })
    output$lr_RMSE_train <- renderValueBox({
      
      valueBox(
        subtitle ="Linear Regression",
        paste(as.character(0.0032)),
        color = "red"
      )
    })
    output$lasso_RMSE_train <- renderValueBox({
      
      valueBox(
        subtitle ="Lasso Regression",
        paste(as.character(0.00361)),
        color = "red"
      )
    })
    output$ridge_RMSE_train <- renderValueBox({
      
      valueBox(
        subtitle ="Ridge Regression",
        paste(as.character(0.00325)),
        color = "red"
      )
    })
    output$lr_RMSE_test <- renderValueBox({
      
      valueBox(
        subtitle ="Linear Regression",
        paste(as.character(0.00348)),
        color = "red"
      )
    })
    output$lasso_RMSE_test <- renderValueBox({
      
      valueBox(
        subtitle ="Lasso Regression",
        paste(as.character(0.00366)),
        color = "red"
      )
    })
    output$ridge_RMSE_test <- renderValueBox({
      
      valueBox(
        subtitle ="Ridge Regression",
        paste(as.character(0.00347)),
        color = "red"
      )
    })
    

    
}

###########################################################################################
#                                  Run the Application                                    #
###########################################################################################
shinyApp(ui = ui, server = server)

