# Pacotes -----------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinybusy)
library(bs4Dash)
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

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
    old_school = FALSE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    enable_preloader = TRUE,
    loading_duration = 5,
    loading_background = "#1C1C1C",
    # Nome do Dashboard
    title = "NLP Twitter - BRASIL",
    # Menu Superior
    navbar = bs4DashNavbar(
        skin = 'light'
    ),
    # Menu Lateral Esquerdo
    sidebar = bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "NLP Twitter (Brasil)",
        src = 'https://media-exp1.licdn.com/dms/image/C561BAQGEbzpXZ34-gQ/company-background_10000/0?e=2159024400&v=beta&t=o3vOn3Ye-qpqlDH64A1of1_aRAQ8TunahPQ4ZWuISRI',
        brandColor = "gray-light",
        bs4SidebarMenu(
            # Páginas do Dashboard
            bs4SidebarMenuItem(
                startExpanded = T,
                tabName = "tweets_twitter",
                icon = "twitter",
                text = "Análise de tweets"
            ),
            # Sobre a aplicação
            bs4SidebarHeader("Informações"),
            bs4SidebarMenuItem(
                tabName = "about",
                icon = "info",
                text = "Aplicação"
            )
        )
    ),
    # Footer
    footer = bs4DashFooter(
        copyrights = a(
            href = "https://mppallante.wixsite.com/mppallante", 
            target = "_blank", "©MPPallante. Todos os direitos reservados."
        ),
        right_text = lubridate::year(Sys.time())
    ), 
    # Corpo do Dahboard
    body = bs4DashBody(
        bs4TabItems(
            # Página Inicial
            bs4TabItem(
                tabName = 'tweets_twitter',
                fluidPage(
                    # Filtro
                    bs4Card(title = 'Filtro', height = "auto", status = 'navy', 
                            width = NULL, closable = F, maximizable = F, collapsible = F,
                            pickerInput(inputId = "tema", 
                                        label = "Assuntos:", 
                                        choices = c("Brasil","COVID-19","Bolsonaro","São Paulo"), 
                                        width = "100%", 
                                        inline = F)),
                    # Nuvem de Palavras
                    bs4Card(title = 'Nuvem de Palavras', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            echarts4rOutput(outputId = "cloudTweet", width = "100%", height = "100%")),
                    # Top 15 Ocorrências de Palavras
                    bs4Card(title = 'Top 15 Ocorrências de Palavras', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            echarts4rOutput(outputId = 'topPalavras', width = "100%", height = "100%"))
                ),
                fluidRow(
                    # Modeagem de Tópicos
                    column(width = 6,
                           bs4Card(title = 'Modeagem de Tópicos', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = 'topics', width = "100%", height = "100%"))),
                    # Rede de Palavras
                    column(width = 6,
                           bs4Card(title = 'Rede de Palavras', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "wordsNetwork", width = "100%", height = "100%")))
                ),
                fluidPage(
                    # Análise de Sentimentos
                    bs4Card(title = 'Análise de Sentimentos', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            echarts4rOutput(outputId = 'feeling', width = "100%", height = "100%"))
                ),
                # Tags
                fluidRow(
                    column(width = 6,
                           # Top 15 Ocorrências de Palavras
                           bs4Card(title = 'Top 15 Ocorrências sobre Tags', height = 400, status = 'warning',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   echarts4rOutput(outputId = 'topTags', width = "100%", height = "100%"))),
                    column(width = 6,
                           # Rede de Palavras sobre as Tags
                           bs4Card(title = 'Rede de Palavras sobre as Tags', height = 400, status = 'warning',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "tagsNetwork", width = "100%", height = "100%")))
                ),
                # Menções
                fluidRow(
                    column(width = 6,
                           # Top 15 Ocorrências de Palavras
                           bs4Card(title = 'Top 15 Ocorrências sobre Menções', height = 400, status = 'olive',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   echarts4rOutput('topMenções', width = "100%", height = "100%"))),
                    column(width = 6,
                           # Rede de Palavras sobre as Menções
                           bs4Card(title = 'Rede de Palavras sobre as Menções', height = 400, status = 'olive',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "MençõesNetwork", width = "100%", height = "100%")))
                )
            ),
            # Sobre
            bs4TabItem(
                tabName = 'about',
                fluidPage(
                    bs4Jumbotron(
                        title = "NLP Twitter (BRASIL) - GITHUB",
                        lead = "Desenvolvido para análise de textos do Twitter.",
                        status = "primary",
                        btn_name = 'GITHUB',
                        href = "https://github.com/mppallante/NLPTwitter-BR"
                    )
                )
            )
        )
    ) 
)

