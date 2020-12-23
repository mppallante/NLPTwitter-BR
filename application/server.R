# Pacotes -----------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(jsonlite)
library(data.table)
library(tidyr)
library(dplyr)
library(plotly)
library(echarts4r)
library(shinyWidgets)
library(formattable)
library(readxl)
library(shinybusy)
library(stopwords)
library(quanteda)
library(syuzhet)
library(reshape2)
library(stm)

# Server Side -------------------------------------------------------------

server <- function(input, output, session) {
    # Base de Dados
    # Tweets - Brasil
    show_modal_spinner(spin = "semipolar", text = "Processando")
    tweets_Brasil <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Brasil.json")
    # Tweets - Brasil Covid-19
    tweets_Covid19 <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Covid19.json")
    # Tweets - Brasil Bolsonaro
    tweets_Bolsonaro <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Bolsonaro.json")
    # Tweets - Brasil São Paulo
    tweets_SP <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_SP.json")
    
    # Gráficos
    output$cloudTweet <- renderEcharts4r({
        tweets <- switch(input$tema, 
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro    
                         },
                         "COVID-19"={
                             tweets_Covid19    
                         },
                         "São Paulo"={
                             tweets_SP    
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP, 
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        df <- dfm_remove(df, pattern = c("#*","@*","https*",emojis$code,"pra", phrase(input$tema)))
        features_dfm <- textstat_frequency(df, n = 500)
        # Nuvem de palavras
        features_dfm %>% 
            e_color_range(frequency, color) %>% 
            e_charts() %>% 
            e_cloud(feature, frequency, color, shape = "square", sizeRange = c(20, 60)) %>% 
            e_tooltip() %>% 
            e_theme("infographic") %>%
            e_legend(show = F) %>% 
            e_title("Tweets", "Frequência de palavras")
    })
    output$topPalavras <- renderEcharts4r({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        df <- dfm_remove(df, pattern = c("#*","@*","https*",emojis$code ,"pra", phrase(input$tema)))
        features_dfm <- textstat_frequency(df, n = 25)
        # Gráfico
        features_dfm %>%
            e_charts(feature) %>%
            e_bar(frequency, name = "Frequência") %>%
            e_title("Tweets", "TOP 25 Palavras") %>%
            e_tooltip() %>%
            e_theme("infographic") %>%
            e_legend(show = F) %>%
            e_labels()
    })
    output$wordsNetwork <- renderPlot({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        tag_dfm <- dfm_remove(df, pattern = c("#*","@*","https*",emojis$code ,"pra", phrase(input$tema)))
        toptag <- names(topfeatures(tag_dfm, 25))
        tag_fcm <- fcm(tag_dfm)
        topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
        tn <- textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5, edge_color = "gold")
        tn
    })
    output$topTags <- renderEcharts4r({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        df <- dfm_select(df, pattern = c("#*"))
        features_dfm <- textstat_frequency(df, n = 25)
        # Gráfico
        features_dfm %>%
            e_charts(feature) %>%
            e_bar(frequency, name = "Frequência") %>%
            e_title("Tweets", "TOP 25 Tags") %>%
            e_tooltip() %>%
            e_theme("infographic") %>%
            e_legend(show = F) %>%
            e_labels()
    })
    output$tagsNetwork <- renderPlot({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        tag_dfm <- dfm_select(df, pattern = c("#*"))
        toptag <- names(topfeatures(tag_dfm, 25))
        tag_fcm <- fcm(tag_dfm)
        topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
        tn <- textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5, edge_color = "gold")
        tn
    })
    output$topMenções <- renderEcharts4r({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        df <- dfm_select(df, pattern = c("@*"))
        features_dfm <- textstat_frequency(df, n = 25)
        # Gráfico
        features_dfm %>%
            e_charts(feature) %>%
            e_bar(frequency, name = "Frequência") %>%
            e_title("Tweets", "TOP 25 Menções") %>%
            e_tooltip() %>%
            e_theme("infographic") %>%
            e_legend(show = F) %>%
            e_labels()
    })
    output$MençõesNetwork <- renderPlot({
        tweets <- switch(input$tema,
                         "Brasil"={
                             tweets_Brasil
                         },
                         "Bolsonaro"={
                             tweets_Bolsonaro
                         },
                         "COVID-19"={
                             tweets_Covid19
                         },
                         "São Paulo"={
                             tweets_SP
                         },
                         {
                             print('Erro Interno!')
                         }
        )
        # Tokenizer
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        tag_dfm <- dfm_select(df, pattern = c("@*"))
        toptag <- names(topfeatures(tag_dfm, 25))
        tag_fcm <- fcm(tag_dfm)
        topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
        tn <- textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5, edge_color = "gold")
        tn
    })
    output$feeling <- renderEcharts4r({
        show_modal_spinner(spin = "semipolar", text = "Processando")
        rstats_tweets <- switch(input$tema,
                                "Brasil"={
                                    tweets_Brasil
                                },
                                "Bolsonaro"={
                                    tweets_Bolsonaro
                                },
                                "COVID-19"={
                                    tweets_Covid19
                                },
                                "São Paulo"={
                                    tweets_SP
                                },
                                {
                                    print('Erro Interno!')
                                }
        )
        # Analise de sentimentos
        felling <- get_nrc_sentiment(rstats_tweets$text, language = "portuguese")
        names(felling) <- c("Raiva", "Ansiedade", "Desgosto", "Receio", "Alegria", "Tristeza",
                            "Surpresa", "Confiança", "Negativo", "Positivo")
        # Preparação dos dados
        felling <- felling %>%
            #group_by(Serviço) %>%
            summarise("Raiva" = round(sum(Raiva)),
                      "Ansiedade" = round(sum(Ansiedade)),
                      "Desgosto" = round(sum(Desgosto)),
                      "Receio" = round(sum(Receio)),
                      "Alegria" = round(sum(Alegria)),
                      "Tristeza" = round(sum(Tristeza)),
                      "Surpresa" = round(sum(Surpresa)),
                      "Confiança" = round(sum(Confiança)),
                      "Negativo" = round(sum(Negativo)),
                      "Positivo" = round(sum(Positivo)))
        felling$ID <- "Geral"
        felling <- melt(felling,
                        id.vars = "ID",
                        variable.names = "Sentimentos",
                        value.name = "Volumetria")
        remove_modal_spinner()
        # Gráfico
        felling %>%
            e_charts(variable) %>%
            e_bar(Volumetria, name = "Volumetria") %>%
            e_title("Volumetria de palavras sobre os sentimentos") %>%
            e_tooltip() %>%
            e_theme("infographic") %>%
            e_legend(show = F) %>%
            e_labels()
    })
    output$topics <- renderPlot({
        show_modal_spinner(spin = "semipolar", text = "Processando")
        rstats_tweets <- switch(input$tema,
                                "Brasil"={
                                    tweets_Brasil
                                },
                                "Bolsonaro"={
                                    tweets_Bolsonaro
                                },
                                "COVID-19"={
                                    tweets_Covid19
                                },
                                "São Paulo"={
                                    tweets_SP
                                },
                                {
                                    print('Erro Interno!')
                                }
        )
        STOP <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
        df <- dfm(rstats_tweets$text,
                  remove = STOP,
                  remove_punct = T,
                  tolower = T,
                  case_insensitive = T,
                  stem = F)
        df <- dfm_remove(df, pattern = c("#*","@*","https*",emojis$code,"pra", phrase(input$tema)))
        df <- dfm_trim(df, min_termfreq = 4, max_docfreq = 10)
        my_lda_fit10 <- stm(df, K = 10, verbose = FALSE)
        remove_modal_spinner()
        plot(my_lda_fit10)
    })
    
    remove_modal_spinner()
}

