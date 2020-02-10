#We first import the packages we need
rm(list = ls())

library(shiny)
library(shinydashboard)
library(ggplot2)
library(rsconnect)
library(leaflet)
library(tidyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(httr)
library(jsonlite)
library(lubridate)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(igraph)
library(ggraph)
library(scales)

#Then we load the file we need to print the dashboard

load("fullhashsentiment.Rdata")
load("fullhashsentimentaff.Rdata")
load("Hashtag.Rdata")
load("HashtagLDA.Rdata")
load("HashtagLDASouthWest.Rdata")
load("HashtagLDASouthWestAir.Rdata")
load("HashtagLDASouthwestairlines.Rdata")
load("HashtagUntoken.Rdata")
load("tweets.Rdata")
load("tweets_to_southwest_summary.Rdata")
load("top_comparison_terms.Rdata")
load("SouthSentimentSummary.Rdata")
load("comtok_count.Rdata")
load("summarySentimenthw.Rdata")
load("top_tweet_terms.Rdata")
load("hw_source.Rdata")
load("GlobalTweets.Rdata")
load("GlobalTweetsToken.Rdata")
load("GlobalSentimentAff.Rdata")
load("GlobalSentiment.Rdata")
load("HashtagMap.Rdata")
load("GlobalTweetsMap.Rdata")
load("GlobalTweetsTokenFrequentWords.Rdata")






#We initiate UI which will be the UI of the dashboard

ui <- dashboardPage(skin = "purple",
                    #We define a title
                    dashboardHeader(title = "South West Airlines Twitter Dashboard"),
                    #And we put in a sidebar differents menus link to each to one specific graphic
                    dashboardSidebar(
                        sidebarMenu(style = "position: fixed; overflow: visible;",
                            menuItem("Global analysis", tabName = "Global", icon = icon("fab fa-twitter")),
                            menuItem("Hashtag analysis", tabName = "hashtag", icon = icon("fas fa-hashtag")),
                            menuItem("Mention analysis", tabName = "Mention", icon = icon("fas fa-at")),
                            menuItem("Competitors analysis", tabName = "Competitors", icon = icon("fas fa-users")),
                            menuItem("Unmentioned analysis", tabName = "Unmentioned", icon = icon("fas fa-comments"))
                        )
                    ),
                    #Now in the body, we will define the value inside each item menu
                    dashboardBody(
                        tabItems(
                            #First tab content containing age distribution
                            tabItem(tabName = "Global",
                                    h2("Global analysis on all the tweets we collected (because of memory usage we can only print one graph)"),
                                    fluidRow(
                                       box(leafletOutput("mymapGlobal"), width = 12, title = "Overall tweets over the world")
                                       ),
                                    fluidRow(
                                      box(plotOutput("FrequentGlobal"), width = 12, title = "Most frequent words overall")
                                    ),
                                ),
                            
                            tabItem(tabName = "hashtag",
                                    h2 ("Focus on the tweets using hashtags linked to the brand:"),
                                    fluidRow(
                                        box(plotOutput("hashtagtrend"),width = 8, height = "450px"),
                                        
                                        box(
                                            selectInput("Time", "Temporality:", 
                                                        choices=c('month','week','day','hour')),
                                            selectInput("hashtagchoosed", "Hashtag:", 
                                                        choices=c("#southwestair","#southwestairlines","#southwest")),
                                            selectInput("dict", "Sentiment dictionary:", 
                                                        choices=c("afinn","bing")), width = 4, height = "450px"
                                        )
                                        
                                    ),
                                    fluidRow(
                                        box(
                                            selectInput("HashtagMap", "Choose the Hashtag:", 
                                                        choices= c('#southwest','#southwestair','#southwestairlines'))),
                                        
                                        p(),
                                        
                                        box(leafletOutput("mymap"), width = 12, title = "Map of tweets per hashtag")
                                        
                                    ),
                                    fluidRow(
                                        box(plotOutput("ComonWord"), width = 8,  height = "450px"),
                                        box(
                                            checkboxInput(inputId = "Timed",
                                                          label = strong("Show data by temporality (month without ticking other options)"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "WeekTick",
                                                          label = strong("Show data by month & week"),
                                                          value = FALSE),
                                            checkboxInput(inputId = "DayTick",
                                                          label = strong("Show data by month & day"),
                                                          value = FALSE),
                                            selectInput("MonthComon","Select the month:", 
                                                        choices=c(1,2)),
                                            sliderInput("DayComon",
                                                        "Select the day:",
                                                        min = 1,  max = 31, value = 1),
                                            sliderInput("WeekComon",
                                                        "Select the week:",
                                                        min = 1,  max = 5, value = 1), width = 4,  height = "450px"
                                        )
                                        
                                    ),
                                    
                                    fluidRow(
                                        box(plotOutput("sentcomonword"),width = 9,  height = "480px", title = "Words that have
                                            the most positive and negative impact"),
                                        box(selectInput("hashtagchoosedSentcomon", "Hashtag:", 
                                                        choices=c("All","#southwestair","#southwestairlines","#southwest")),
                                            width = 3, height = "480px")
                                        
                                    ),
                                    
                                    fluidRow(
                                        box(plotOutput("topichashtag"), width = 9, height = "480px", title = "
                                            Topic Modeling to create 4 differents topics group"),
                                        box(selectInput("hashtagchoosedTopic", "Hashtag:", 
                                                        choices=c("All","#southwestair","#southwestairlines","#southwest")),
                                            width = 3, height = "480px")
                                        
                                    ),
                                    fluidRow(
                                        box(plotOutput("HashtagCount"), width = 8, height = "450px"),
                                        box( selectInput("TemporalityCount", "Temporality:", 
                                                         choices=c("month","week","day","hour")),
                                             p("Red = All hashtags"),
                                             p("Blue =  #southwestairlines"),
                                             p("Green = #southwestair"),
                                             p("Orange = #southwest"),width = 4, height = "450px")
                                    )
                                    
                            ),
                            
                tabItem(tabName = "Mention",
                                    h2 ("Focus on the tweets mentionning the brand account:"),
                                    fluidRow(
                                      box(
                                        plotOutput("MentionCount"), width =  12)),
                        fluidRow(
                          box(plotOutput("summaryreplies"),width =  12, title = "Replies summary of sentiment")
                                        ),
                        fluidRow(
                          box(plotOutput("mentionwordcloud"),width = 12, title = "Word cloud")
                                        )
                         ),
                tabItem(tabName = "Competitors",
                        h2("Focus on competitors tweets talking about South West Airlines"),
                        fluidRow(
                          box(plotOutput("competitorstopic"), width = 12, title = "Topic analysis of competitors")
                          ),
                        fluidRow(
                          box(plotOutput("competitorsLink"),width = 8, title = "Link between competitors and South West"),
                          box( selectInput("Word", "Choose the word:", 
                                           choices=c("delta", "united", "jetblue", "americanair","fly","All")), width = 4)
                        )
                        ),
                tabItem(tabName = "Unmentioned",
                h2("Focus on tweets talking about the brand without using # or @"),
                fluidRow(
                  box(plotOutput("hwsumsent"), width = 12, title = "Summary sentiment")
                ),
                fluidRow(
                  box(plotOutput("unmentionTopic"), width = 12, title = "Topic analysis")
                ),
                fluidRow(
                  box(plotOutput('sourceperTime'), width = 8, title = "Tweets per sources"),
                  box(
                    selectInput("TimeUnmention", "Temporality:", 
                                choices=c("month","week","day","hour")), width = 4)
                  ),
                fluidRow(
                  box(plotOutput('QuoteSource'),width = 12, title = "Quoted or not")
                )
            )
       )
    )
)

#Now we will define the function to make the graphs interactive
server = function(input, output) {
    
    output$hashtagtrend <- renderPlot({
        dicmethod = input$dict
        Timelapse = input$Time
        AnalysentHash = input$hashtagchoosed
        if (dicmethod == "bing"){
            FullHashSentiment = FullHashSentiment[FullHashSentiment$Hashtag == AnalysentHash,]
            FullHashSentimentDate <- FullHashSentiment %>%
                group_by_at(Timelapse) %>%
                count(sentiment) %>%                # count the positives and negatives per id (status)
                spread(sentiment, n, fill = 0) %>%      # we want overall sentiment, and here we have text (positive and negative), making it difficult to direclty calculate sentiment. 
                mutate(sentiment = positive - negative)
            
            ggplot(FullHashSentimentDate,aes_string(Timelapse))+
                geom_line(aes(y = sentiment, colour = "Sentiment")) +
                geom_line(aes(y = negative, colour = "negative")) +
                geom_line(aes(y = positive, colour = "positive")) +
                ggtitle(paste("Analysis per",Timelapse,"of the",AnalysentHash, "hashtag.")) +
                xlab(paste('By',Timelapse)) + ylab("Nb of sentiment type")
        } 
        else if (dicmethod == "afinn") {
            
            FullHashSentimentAff = FullHashSentimentAff[FullHashSentimentAff$Hashtag == AnalysentHash,]
            
            FullHashSentimentAff = FullHashSentimentAff %>% 
                group_by(user_id) %>%
                summarize(Time = round(mean(!! rlang::sym(Timelapse)))
                          , value = sum(value))
            
            FullHashSentimentAff <- FullHashSentimentAff %>%
                group_by(Time) %>%
                summarize(sentiment = sum(value),
                          positive = sum(value[value>0]),
                          negative = sum(value[value<0]))
            
            
            ggplot(FullHashSentimentAff,aes(Time))+
                geom_line(aes(y = sentiment, colour = "Sentiment")) +
                geom_line(aes(y = negative, colour = "negative")) +
                geom_line(aes(y = positive, colour = "positive"))+
                ggtitle(paste("Analysis per",Timelapse,"of the",AnalysentHash, "hashtag.")) +
                xlab(paste('By',Timelapse)) + ylab("Nb of sentiment type")
        }
    })
    
    output$mymap <- renderLeaflet({
      HashtagMap = Hashtag[!is.na(Hashtag$lng),]
      leaflet(data = HashtagMap)%>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(lng = Hashtag$lng[Hashtag$Hashtag == input$HashtagMap],
                       lat = Hashtag$lat[Hashtag$Hashtag == input$HashtagMap])
    })
    
    output$ComonWord <- renderPlot({
        if (input$Timed) {
            if(input$DayTick){
                HashGlobAnalysis <- Hashtag[Hashtag$day == input$DayComon 
                                            & Hashtag$month == input$MonthComon, ] %>%
                    count(user_id,word)
            } else if (input$WeekTick) {
                HashGlobAnalysis <- Hashtag[Hashtag$day == input$WeekComon
                                            & Hashtag$month == input$MonthComon, ] %>%
                    count(user_id,word)
                
            }
            else {
                HashGlobAnalysis <- Hashtag[Hashtag$month == input$MonthComon, ] %>%
                    count(user_id,word)
            }
        }
        else { 
            HashGlobAnalysis <- Hashtag %>%
                count(user_id,word)
            
        }
        
        
        sum(Hashtag$word == 'southwestair')
        sum(HashGlobAnalysis$word == "southwestair")
        
        HashGlobAnalysis %>%
            count(word, sort = TRUE) %>%
            top_n(15) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(x = word, y = n)) +
            geom_col() +
            xlab(NULL) +
            coord_flip() +
            labs(y = "Count",
                 x = "Unique words",
                 title = "Most frequent words found in the hashtags",
                 subtitle = "Stop words removed from the list")
        
        #Now, we try the same analysis without the words that are related to job market
        HashGlobAnalysisWithoutJobs = subset(HashGlobAnalysis,!(HashGlobAnalysis$word %in% c('job','apply','hiring','wanted','looking')))
        
        HashGlobAnalysisWithoutJobs %>%
            count(word, sort = TRUE) %>%
            top_n(15) %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(x = word, y = n)) +
            geom_col() +
            xlab(NULL) +
            coord_flip() +
            labs(y = "Count",
                 x = "Unique words",
                 title = "Most frequent words found in the hashtags",
                 subtitle = "Stop words removed from the list")
    })
    
    output$sentcomonword <- renderPlot({
        
        
        if (input$hashtagchoosedSentcomon == "All"){
            HashComonWord = FullHashSentiment
        }
        else if (input$hashtagchoosedSentcomon == "#southwest") {
            HashComonWord = FullHashSentiment[FullHashSentiment$Hashtag == "#southwest",]
        }
        else if (input$hashtagchoosedSentcomon == "#southwestair") {
            HashComonWord = FullHashSentiment[FullHashSentiment$Hashtag == "#southwestair",]
        }
        else if (input$hashtagchoosedSentcomon == "#southwestairlines") {
            HashComonWord = FullHashSentiment[FullHashSentiment$Hashtag == "#southwestairlines",]
        }
        
        HashComonWord <- HashComonWord %>%  count(word,sentiment,sort=TRUE) %>%
            group_by(sentiment) %>%
            top_n(10) %>%  
            arrange(n) %>%
            as.data.frame(stringsAsFactors=FALSE)
        
        HashComonWord = HashComonWord[!is.na(HashComonWord$sentiment),]
        
        par(oma=c(0,0,0,0),mfrow=c(2,1))
        
        HashComonWord %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment",
                 x = NULL) +
            coord_flip()
        
    })
    
    output$topichashtag <- renderPlot({
        
        
        if (input$hashtagchoosedTopic == "All"){
            HashtagTopic = HashtagLDA
        }
        else if (input$hashtagchoosedTopic == "#southwest") {
            HashtagTopic = HashtagLDASouthWest
        }
        else if (input$hashtagchoosedTopic == "#southwestair") {
            HashtagTopic = HashtagLDASouthWestAir
        }
        else if (input$hashtagchoosedTopic == "#southwestairlines") {
            HashtagTopic = HashtagLDASouthwestairlines
        }
        
        HashtagTopic <- tidy(HashtagTopic, matrix = "beta")
        
        TopHashtagTopic <- HashtagTopic %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        TopHashtagTopic %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            scale_x_reordered()
        
    })
    
    output$HashtagCount <- renderPlot({
        
        Timelapse = input$TemporalityCount
        
        HashtagTrendCount = HashtagUntoken %>%
            group_by_at(Timelapse) %>%
            count(user_id)
        
        HashtagTrendCount = HashtagTrendCount %>%
            group_by_at(Timelapse) %>%
            summarize(tweets_all = sum(n))
        
        HashtagTrendCountSouthWest = HashtagUntoken[HashtagUntoken$Hashtag == "#southwest",] %>%
            group_by_at(Timelapse) %>%
            count(user_id)
        
        HashtagTrendCountSouthWest = HashtagTrendCountSouthWest %>%
            group_by_at(Timelapse) %>%
            summarize(tweets_southwest = sum(n))
        
        HashtagTrendCountSouthWestAir = HashtagUntoken[HashtagUntoken$Hashtag == "#southwestair",] %>%
            group_by_at(Timelapse) %>%
            count(user_id)
        
        HashtagTrendCountSouthWestAir = HashtagTrendCountSouthWestAir %>%
            group_by_at(Timelapse) %>%
            summarize(tweets_southwestair = sum(n))
        
        HashtagTrendCountSouthWestAirlines = HashtagUntoken[HashtagUntoken$Hashtag == "#southwestairlines",] %>%
            group_by_at(Timelapse) %>%
            count(user_id)
        
        HashtagTrendCountSouthWestAirlines = HashtagTrendCountSouthWestAirlines %>%
            group_by_at(Timelapse) %>%
            summarize(tweets_southwestairlines = sum(n))
        
        HashTrendFinal = merge(HashtagTrendCount,HashtagTrendCountSouthWest, by = Timelapse, all.x = TRUE)    
        HashTrendFinal = merge(HashTrendFinal,HashtagTrendCountSouthWestAir, by = Timelapse, all.x = TRUE)
        HashTrendFinal = merge(HashTrendFinal,HashtagTrendCountSouthWestAirlines, by = Timelapse, all.x = TRUE)
        
        
        #Add title to graphics & way to print hashtag
        
        ggplot(HashTrendFinal, aes_string(x=Timelapse)) + 
            geom_line(aes(y = tweets_all), color = "darkred") + 
            geom_line(aes(y = tweets_southwestairlines), color="steelblue")+
            geom_line(aes(y = tweets_southwestair), color="green")+
            geom_line(aes(y = tweets_southwest), color="orange") +
            ggtitle(paste("Count of tweets per",Timelapse, "per hashtag")) +
            xlab(paste('By',Timelapse)) + ylab("Nb of tweets")
    })
    
    output$MentionCount <- renderPlot({
      
      tweets %>% # gives you a bar chart of the most frequent words found in the tweets
        count(word, sort = TRUE) %>%
        top_n(15) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(x = word, y = n)) +
        geom_col(fill="blue") +
        xlab(NULL) +
        coord_flip() +
        labs(y = "Count",
             x = "Unique words",
             title = "Most frequent words found in the replies made by southwest",
             subtitle = "Stop words removed from the list")
      
    })
    
    output$summaryreplies <- renderPlot({
      
      
      tweets_to_southwest_summary%>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
      
    })

    output$mentionwordcloud <- renderPlot({

      

      wordcloud(words = SouthSentimentSummary$word, freq = SouthSentimentSummary$n, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"), scale = c(4,2))

    })
    
    output$competitorstopic <- renderPlot({
      
      top_comparison_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
      
      
    })
    
    output$competitorsLink <- renderPlot({
      
      if (input$Word == "All"){
        words_1 <- c("delta", "united", "jetblue", "americanair","fly")
        
      } else {
        words_1 <- input$Word
      }
      
      
      #bigram network
      bigram_network <- comtok_count %>%
        filter(word1 %in% words_1) %>%
        graph_from_data_frame()
      
      set.seed(2017)
      
      ggraph(bigram_network, layout = "fr") +
        geom_edge_link() +
        geom_node_point(color = "lightgreen", size = 2) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)
      
    })
    
    output$hwsumsent <- renderPlot({
      
      summarySentimenthw %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()
    })
    
    output$unmentionTopic <- renderPlot({
      
      top_tweet_terms %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()
      
    })
    
    output$sourceperTime <- renderPlot({
      
      
      if (input$TimeUnmention == "hour") {
        hw_source%>%
          count(source,hour=hour(with_tz(created_at,"EST")))%>%
          mutate(percent=n/sum(n))%>%
          ggplot(hw_source,mapping=aes(x=hour,y=percent,color=source))+
          geom_line()+
          scale_y_continuous(labels=percent_format())+
          labs(x="Hour of day", y="% of posts", color="")+
          ggtitle('Time of the day people tweet')
      }
      else if (input$TimeUnmention == "day") {
        hw_source%>%
          count(source,day=day(created_at))%>%
          mutate(percent=n/sum(n))%>%
          ggplot(hw_source,mapping=aes(x=day,y=percent,color=source))+
          geom_line()+
          scale_y_continuous(labels=percent_format())+
          labs(x="Days in months", y="% of posts", color="")+
          ggtitle('Days in months people tweet')
      }
      else if (input$TimeUnmention == "week") {
        hw_source%>%
          count(source,wday=wday(created_at))%>%
          mutate(percent=n/sum(n))%>%
          ggplot(hw_source,mapping=aes(x=wday,y=percent,color=source))+
          geom_line()+
          scale_y_continuous(labels=percent_format())+
          labs(x="Days in a week", y="% of posts", color="")+
          ggtitle('Days in a week people tweet')
      }
      else if (input$TimeUnmention == "month") {
        hw_source%>%
          count(source,month=month(created_at))%>%
          mutate(percent=n/sum(n))%>%
          ggplot(hw_source,mapping=aes(x=month,y=percent,color=source))+
          geom_line()+
          scale_y_continuous(labels=percent_format())+
          labs(x="Months of the year", y="% of posts", color="")+
          ggtitle('Months of the year people tweet')
      }
      
    })
    
    output$QuoteSource <- renderPlot({
  
    hw_source%>%
      count(source,is_quote)%>% 
      ggplot(hw_source,mapping=aes(x=source, y=n, fill=is_quote)) +
      geom_bar(stat ="identity", position ="dodge") +
      guides(fill=FALSE) +
      labs(x ="Data source", y ="Number of tweets", fill ="") +
      ggtitle('Whether tweets are quoted')
    })
    
    output$mymapGlobal <- renderLeaflet({
      leaflet(data = GlobalTweetsMap)%>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(~lng, ~lat)
    })
    
    output$FrequentGlobal <- renderPlot({
      ggplot(data = GlobalTweetsTokenFrequentWords, aes(x = word, y = n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip() +
      labs(y = "Count",
           x = "Unique words",
           title = "Most frequent words found in the tweets we collected",
           subtitle = "Stop words removed from the list")
  })
    
}

#We launch the shiny application to have our dashboard
shinyApp(ui, server)
