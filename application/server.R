# Pacotes -----------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(thematic)
library(waiter)
library(jsonlite)
library(plotly)
library(dplyr)
library(reshape2)
library(tm)
library(rtweet)
library(igraph)
library(ggraph)
library(ggplot2)
library(networkD3)
library(udpipe)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(stm)
library(BTM)
library(textplot)
library(concaveman)
library(syuzhet)
library(stopwords)
library(echarts4r)
library(visNetwork)

# Server Side -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Configurações -----------------------------------------------------------
  
  # Configurações do Loading (Tela de Carregamento - Gráficos)
  w <- Waiter$new(
    id = c(
      # Análise de Texto
      "cloudTweet","topPalavras","topics","wordsNetwork","feeling",
      # Análise de Tags
      "topTags","tagsNetwork",
      #Análise de  Menções
      "topMenções","MençõesNetwork"
    ), 
    # Estilo do Spin
    html = spin_whirly(), 
    # Cor de fundo
    color = transparent(alpha = .75))
  
  # Tema - Auto Color
  useAutoColor()
  
  ##### Base de Dados #####
  
  # Dados do Twitter sobre o Brasil
  tweets_Brasil <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Brasil.json")
  # Dados do Twitter sobre a Covid-19 no Brasil
  tweets_Covid19 <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Covid19.json")
  # Dados do Twitter sobre o Bolsonaro
  tweets_Bolsonaro <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_Bolsonaro.json")
  # Dados do Twitter sobre o Estado de São Paulo
  tweets_SP <- fromJSON("https://raw.githubusercontent.com/mppallante/NLPTwitter-BR/master/json/tweets_SP.json")
  
  ##### Controle de Filtro #####
  
  text <- reactive({
    text <- switch(input$tema, 
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
  })
  
  ##### Análise de Texto #####
  
  # Nuvem de Palavras
  output$cloudTweet <- renderEcharts4r({
    # Carregamento
    w$show()
    # StopWords
    STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
    STOP <- stopwords(language = "pt", source = "stopwords-iso")
    EMOJI <- emojis$code
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(STOP_TWITTER, STOP, EMOJI, phrase(input$tema),"#*", "@*","Jair","Covid")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    features_dfm <- textstat_frequency(df, n = 1000)
    # Nuvem de palavras
    features_dfm %>%
      e_color_range(frequency, color) %>%
      e_charts() %>%
      e_cloud(feature, frequency, color, shape = "cirque") %>%
      e_tooltip() %>%
      e_theme("infographic") %>%
      e_legend(show = F)
  })
  # Top 15 Ocorrências de Palavras
  output$topPalavras <- renderEcharts4r({
    # Carregamento
    w$show()
    # StopWords
    STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
    STOP <- stopwords(language = "pt", source = "stopwords-iso")
    EMOJI <- emojis$code
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(STOP_TWITTER, STOP, EMOJI, phrase(input$tema), "#*", "@*","Jair","Covid")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    features_dfm <- textstat_frequency(df, n = 15)
    # Gráfico
    features_dfm %>%
      arrange(frequency) %>%
      e_charts(feature) %>%
      e_bar(frequency, name = "Frequência") %>%
      e_tooltip() %>%
      e_theme("infographic") %>%
      e_legend(show = F) %>%
      e_labels(position = "right") %>%
      e_flip_coords()
  })
  # Modelo de Tópicos de Bitermos
  output$topics <- renderPlot({
    # Carregamento
    w$show()
    # StopWords
    STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
    STOP <- stopwords(language = "pt", source = "stopwords-iso")
    EMOJI <- emojis$code
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(STOP_TWITTER, STOP, EMOJI, phrase(input$tema), "#*", "@*")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    # Transforma em objeto igraph
    tn <- as.igraph(fcm(df), min_freq = 1, omit_isolated = FALSE)
    wc <- cluster_walktrap(tn)
    members <- membership(wc)
    # Transforma em formato compativel com networkD3
    graph_d3 <- igraph_to_networkD3(tn, group = members)
    links <- as.data.frame(graph_d3$links)
    nodes <- as.data.frame(graph_d3$nodes)
    target <- NA
    source <- NA
    clusts <- NA
    for (i in 1:length(links$source)) {
      target[i] <- nodes$name[[links$target[i]]]
      source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
      clusts[i] <- ifelse(links$source[i] == 0, NA, nodes$group[[links$source[i]]])
    }
    graph_df = data.frame(
      source = source,
      target = target,
      clusts = factor(clusts)
    )
    graph_df <- graph_df %>%
      group_by(doc_id = clusts, lemma = source) %>%
      summarise(freq = n()) %>%
      na.omit()
    graph_df <- graph_df[, c("doc_id", "lemma")]
    # Modelagem de topicos
    model <- BTM(graph_df, k = 10, window = 5, iter = 2500, background = T, detailed = T)
    plot(model)
  })
  # Rede de Palavras
  output$wordsNetwork <- renderPlot({
    # Carregamento
    w$show()
    # StopWords
    STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
    STOP <- stopwords(language = "pt", source = "stopwords-iso")
    EMOJI <- emojis$code
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(STOP_TWITTER, STOP, EMOJI, phrase(input$tema), "#*", "@*","Jair","Covid")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    # Identifica as maiores frequencias
    toptag <- names(topfeatures(df, 100))
    tag_fcm <- fcm(df)
    topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
    w$hide()
    # Rede de palavras
    textplot_network(topgat_fcm, min_freq = .75, edge_alpha = 1, edge_size = 1, edge_color = "#FFA500")
  })
  # Rede de Palavras
  # output$wordsNetwork <- renderVisNetwork({
  #     # Carregamento
  #     w$show()
  #     # StopWords
  #     STOP_TWITTER <- stopwordslangs$word[which(stopwordslangs$lang == "pt")]
  #     STOP <- stopwords(language = "pt", source = "stopwords-iso")
  #     EMOJI <- emojis$code
  #     # Tokens & DFM
  #     df <- text()$text %>%
  #         tokens(remove_punct = TRUE,
  #                remove_symbols = TRUE,
  #                remove_numbers = TRUE,
  #                remove_url = TRUE,
  #                remove_separators = TRUE,
  #                split_hyphens = TRUE) %>%
  #         tokens_remove(c(STOP_TWITTER, STOP, EMOJI, phrase(input$tema), "#*", "@*")) %>%
  #         dfm(tolower = TRUE) %>%
  #         dfm_group(text()$user_id)
  #     # Identifica as maiores frequencias
  #     toptag <- names(topfeatures(df, 30))
  #     tag_fcm <- fcm(df)
  #     topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
  #     # Transforma em objeto igraph
  #     tn <- as.igraph(topgat_fcm, min_freq = 1, omit_isolated = FALSE)
  #     wc <- cluster_walktrap(tn)
  #     members <- membership(wc)
  #     # Transforma em formato compativel com networkD3
  #     graph_d3 <- igraph_to_networkD3(tn, group = members)
  #     links <- as.data.frame(graph_d3$links)
  #     nodes <- as.data.frame(graph_d3$nodes)
  #     # Cria o dataframe final com as associações
  #     target <- NA
  #     source <- NA
  #     for (i in 1:length(links$source)) {
  #         target[i] <- nodes$name[[links$target[i]]]
  #         source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
  #     }
  #     graph_df = data.frame(
  #         source = source,
  #         target = target
  #     )
  #     graph_df <- graph_df %>%
  #         group_by(source, target) %>%
  #         summarise(freq = n()) %>%
  #         na.omit()
  #     nodes <- graph_df %>%
  #         group_by(id = source, label = source) %>%
  #         summarise(value = n()) %>%
  #         select(id, label) %>%
  #         as.data.frame()
  #     edges <- graph_df %>%
  #         group_by(from = source, to = target) %>%
  #         summarise(width = sum(freq)) %>%
  #         mutate(width = formattable::normalize(width, min = 0, max = 10)) %>%
  #         as.data.frame()
  #     # Create graph for Louvain
  #     graph <- graph_from_data_frame(edges, directed = FALSE)
  #     # Louvain Comunity Detection
  #     cluster <- cluster_louvain(graph)
  #     cluster_df <- data.frame(as.list(membership(cluster)))
  #     cluster_df <- as.data.frame(t(cluster_df))
  #     cluster_df$label <- rownames(cluster_df)
  #     # Create group column
  #     nodes <- left_join(nodes, cluster_df, by = "label")
  #     colnames(nodes)[3] <- "group"
  #     # Gráfico
  #     visNetwork(nodes, edges) %>%
  #         visEdges(arrows = "to") %>%
  #         visOptions(highlightNearest = FALSE) %>%
  #         visInteraction(navigationButtons = TRUE)
  # })
  # Análise de Sentimentos
  output$feeling <- renderEcharts4r({
    # Carregamento
    w$show()
    # Modelo de sentimentos
    felling <- get_nrc_sentiment(text()$text, language = "portuguese")
    names(felling) <- c("Raiva", "Ansiedade", "Desgosto", "Receio", "Alegria", "Tristeza",
                        "Surpresa", "Confiança", "Negativo", "Positivo")
    # Tratamento dos dados
    felling <- felling %>%
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
    # Cores do gráfico
    felling$color <-  c("#D3D3D3","#D3D3D3",
                        "#D3D3D3","#D3D3D3",
                        "#D3D3D3","#D3D3D3",
                        "#D3D3D3","#D3D3D3",
                        "#B22222","#32CD32")
    # Gráfico sobre os Sentimentos
    felling %>%
      arrange(Volumetria) %>%
      e_charts(variable) %>%
      e_bar(Volumetria, name = "Freqência") %>%
      e_tooltip() %>%
      e_theme("infographic") %>%
      e_legend(show = F) %>%
      e_labels(position = "right")  %>%
      e_flip_coords() %>%
      e_add("itemStyle", color)
  })
  
  ##### Análise de Tags #####
  
  # Top 15 Ocorrências de Tags
  output$topTags <- renderEcharts4r({
    # Carregamento
    w$show()
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(phrase(input$tema), paste0("#",phrase(input$tema)))) %>%
      tokens_select(pattern = c("#*")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    features_dfm <- textstat_frequency(df, n = 15)
    # Gráfico
    features_dfm %>%
      arrange(frequency) %>%
      e_charts(feature) %>%
      e_bar(frequency, name = "Frequência") %>%
      e_tooltip() %>%
      e_theme("infographic") %>%
      e_legend(show = F) %>%
      e_labels(position = "right") %>%
      e_flip_coords()
  })
  # Rede de Palavras sobre as Tags
  output$tagsNetwork <- renderPlot({
    # Carregamento
    w$show()
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(phrase(input$tema), paste0("#",phrase(input$tema)))) %>%
      tokens_select(pattern = c("#*")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    # Identifica as maiores frequencias
    toptag <- names(topfeatures(df, 100))
    tag_fcm <- fcm(df)
    topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
    w$hide()
    # Rede de palavras
    textplot_network(topgat_fcm, min_freq = .75, edge_alpha = 1, edge_size = 1, edge_color = "#FFA500")
  })
  # Rede de Palavras sobre as Tags
  # output$tagsNetwork <- renderVisNetwork({
  #     # Carregamento
  #     w$show()
  #     # Tokens & DFM
  #     df <- text()$text %>%
  #         tokens(remove_punct = TRUE,
  #                remove_symbols = TRUE,
  #                remove_numbers = TRUE,
  #                remove_url = TRUE,
  #                remove_separators = TRUE,
  #                split_hyphens = TRUE) %>%
  #         tokens_remove(c(phrase(input$tema), paste0("#",phrase(input$tema)))) %>%
  #         tokens_select(pattern = c("#*")) %>%
  #         dfm(tolower = TRUE) %>%
  #         dfm_group(text()$user_id)
  #     # Identifica as maiores frequencias
  #     toptag <- names(topfeatures(df, 30))
  #     tag_fcm <- fcm(df)
  #     topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
  #     # Transforma em objeto igraph
  #     tn <- as.igraph(topgat_fcm, min_freq = 1, omit_isolated = FALSE)
  #     wc <- cluster_walktrap(tn)
  #     members <- membership(wc)
  #     # Transforma em formato compativel com networkD3
  #     graph_d3 <- igraph_to_networkD3(tn, group = members)
  #     links <- as.data.frame(graph_d3$links)
  #     nodes <- as.data.frame(graph_d3$nodes)
  #     # Cria o dataframe final com as associações
  #     target <- NA
  #     source <- NA
  #     for (i in 1:length(links$source)) {
  #         target[i] <- nodes$name[[links$target[i]]]
  #         source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
  #     }
  #     graph_df = data.frame(
  #         source = source,
  #         target = target
  #     )
  #     graph_df <- graph_df %>%
  #         group_by(source, target) %>%
  #         summarise(freq = n()) %>%
  #         na.omit()
  #     nodes <- graph_df %>%
  #         group_by(id = source, label = source) %>%
  #         summarise(value = n()) %>%
  #         select(id, label) %>%
  #         as.data.frame()
  #     edges <- graph_df %>%
  #         group_by(from = source, to = target) %>%
  #         summarise(width = sum(freq)) %>%
  #         mutate(width = formattable::normalize(width, min = 0, max = 10)) %>%
  #         as.data.frame()
  #     # Create graph for Louvain
  #     graph <- graph_from_data_frame(edges, directed = FALSE)
  #     # Louvain Comunity Detection
  #     cluster <- cluster_louvain(graph)
  #     cluster_df <- data.frame(as.list(membership(cluster)))
  #     cluster_df <- as.data.frame(t(cluster_df))
  #     cluster_df$label <- rownames(cluster_df)
  #     # Create group column
  #     nodes <- left_join(nodes, cluster_df, by = "label")
  #     colnames(nodes)[3] <- "group"
  #     # Gráfico
  #     visNetwork(nodes, edges) %>%
  #         visEdges(arrows = "to") %>%
  #         visOptions(highlightNearest = FALSE) %>%
  #         visInteraction(navigationButtons = TRUE)
  # })
  
  ##### Análise de Menções #####
  
  # Top 15 Ocorrências de Menções
  output$topMenções <- renderEcharts4r({
    # Carregamento
    w$show()
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(phrase(input$tema), paste0("@",phrase(input$tema)))) %>%
      tokens_select(pattern = c("@*")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    features_dfm <- textstat_frequency(df, n = 15)
    # Gráfico
    features_dfm %>%
      arrange(frequency) %>%
      e_charts(feature) %>%
      e_bar(frequency, name = "Frequência") %>%
      e_tooltip() %>%
      e_theme("infographic") %>%
      e_legend(show = F)%>%
      e_labels(position = "right") %>%
      e_flip_coords()
  })
  # Rede de Palavras sobre as Menções
  output$MençõesNetwork <- renderPlot({
    # Carregamento
    w$show()
    # Tokens & DFM
    df <- text()$text %>%
      tokens(remove_punct = TRUE,
             remove_symbols = TRUE,
             remove_numbers = TRUE,
             remove_url = TRUE,
             remove_separators = TRUE,
             split_hyphens = TRUE) %>%
      tokens_remove(c(phrase(input$tema), paste0("@",phrase(input$tema)))) %>%
      tokens_select(pattern = c("@*")) %>%
      dfm(tolower = TRUE) %>%
      dfm_group(text()$user_id)
    # Identifica as maiores frequencias
    toptag <- names(topfeatures(df, 100))
    tag_fcm <- fcm(df)
    topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
    w$hide()
    # Rede de palavras
    textplot_network(topgat_fcm, min_freq = .75, edge_alpha = 1, edge_size = 1, edge_color = "#FFA500")
  })
  # Rede de Palavras sobre as Menções
  # output$MençõesNetwork <- renderVisNetwork({
  #     # Carregamento
  #     w$show()
  #     # Tokens & DFM
  #     df <- text()$text %>%
  #         tokens(remove_punct = TRUE,
  #                remove_symbols = TRUE,
  #                remove_numbers = TRUE,
  #                remove_url = TRUE,
  #                remove_separators = TRUE,
  #                split_hyphens = TRUE) %>%
  #         tokens_remove(c(phrase(input$tema), paste0("@",phrase(input$tema)))) %>%
  #         tokens_select(pattern = c("@*")) %>%
  #         dfm(tolower = TRUE) %>%
  #         dfm_group(text()$user_id)
  #     # Identifica as maiores frequencias
  #     toptag <- names(topfeatures(df, 30))
  #     tag_fcm <- fcm(df)
  #     topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
  #     # Transforma em objeto igraph
  #     tn <- as.igraph(topgat_fcm, min_freq = 1, omit_isolated = FALSE)
  #     wc <- cluster_walktrap(tn)
  #     members <- membership(wc)
  #     # Transforma em formato compativel com networkD3
  #     graph_d3 <- igraph_to_networkD3(tn, group = members)
  #     links <- as.data.frame(graph_d3$links)
  #     nodes <- as.data.frame(graph_d3$nodes)
  #     # Cria o dataframe final com as associações
  #     target <- NA
  #     source <- NA
  #     clusts <- NA
  #     for (i in 1:length(links$source)) {
  #         target[i] <- nodes$name[[links$target[i]]]
  #         source[i] <- ifelse(links$source[i] == 0, NA, nodes$name[[links$source[i]]])
  #         clusts[i] <- ifelse(links$source[i] == 0, NA, nodes$group[[links$source[i]]])
  #     }
  #     graph_df = data.frame(
  #         source = source,
  #         target = target,
  #         clusts = factor(clusts)
  #     )
  #     graph_df <- graph_df %>%
  #         group_by(source, target, clusts) %>%
  #         summarise(freq = n()) %>%
  #         na.omit()
  #     nodes <- graph_df %>%
  #         group_by(id = source, label = source) %>%
  #         summarise(value = n()) %>%
  #         select(id, label) %>%
  #         as.data.frame()
  #     edges <- graph_df %>%
  #         group_by(from = source, to = target) %>%
  #         summarise(width = sum(freq)) %>%
  #         mutate(width = formattable::normalize(width, min = 0, max = 10)) %>%
  #         as.data.frame()
  #     # Create graph for Louvain
  #     graph <- graph_from_data_frame(edges, directed = FALSE)
  #     # Louvain Comunity Detection
  #     cluster <- cluster_louvain(graph)
  #     cluster_df <- data.frame(as.list(membership(cluster)))
  #     cluster_df <- as.data.frame(t(cluster_df))
  #     cluster_df$label <- rownames(cluster_df)
  #     # Create group column
  #     nodes <- left_join(nodes, cluster_df, by = "label")
  #     colnames(nodes)[3] <- "group"
  #     # Gráfico
  #     visNetwork(nodes, edges) %>%
  #         visEdges(arrows = "to") %>%
  #         visOptions(highlightNearest = FALSE) %>%
  #         visInteraction(navigationButtons = TRUE)
  # })
  
}

