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
library(rtweet)

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
    old_school = FALSE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    enable_preloader = TRUE,
    loading_duration = 5,
    loading_background = "#1C1C1C",
    # Nome do Dashboard
    title = "Guia para Criar - NLP Twitter",
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
            href = "https://www.guiaparacriar.com.br/", 
            target = "_blank", "© Guia para Criar. Todos os direitos reservados."
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
                    bs4Card(title = 'Configuração', height = "auto", status = 'navy', 
                            width = NULL, closable = F, maximizable = F, collapsible = F,
                            pickerInput(inputId = "tema", 
                                        label = "Temas:", 
                                        choices = c("Brasil","Bolsonaro","COVID-19", "São Paulo"), 
                                        width = "100%", 
                                        inline = F)),
                    # Gráficos
                    bs4Card(title = 'Nuvem de Palavras', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            echarts4rOutput(outputId = "cloudTweet", width = "100%", height = "100%"))
                ),
                # Palavras
                fluidRow(
                    column(width = 6,
                           bs4Card(title = 'TOP 25 Palavras', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   echarts4rOutput(outputId = 'topPalavras', width = "100%", height = "100%"))),
                    column(width = 6,
                           bs4Card(title = 'Análise de Redes - Palavras', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "wordsNetwork", width = "100%", height = "100%")))
                ),
                # Tags
                fluidRow(
                    column(width = 6,
                           bs4Card(title = 'TOP 25 Tags', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   echarts4rOutput(outputId = 'topTags', width = "100%", height = "100%"))),
                    column(width = 6,
                           bs4Card(title = 'Análise de Redes - Tags', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "tagsNetwork", width = "100%", height = "100%")))
                ),
                # Menções
                fluidRow(
                    column(width = 6,
                           bs4Card(title = 'TOP 25 Menções', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   echarts4rOutput('topMenções', width = "100%", height = "100%"))),
                    column(width = 6,
                           bs4Card(title = 'Análise de Redes - Menções', height = 400, status = 'primary',
                                   width = NULL, closable = F, maximizable = T, collapsible = F,
                                   plotOutput(outputId = "MençõesNetwork", width = "100%", height = "100%")))
                ),
                fluidPage(
                    bs4Card(title = 'Análise de Tópicos', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            plotOutput(outputId = 'topics', width = "100%", height = "100%")),
                    bs4Card(title = 'Análise de Sentimentos', height = 400, status = 'primary',
                            width = NULL, closable = F, maximizable = T, collapsible = F,
                            echarts4rOutput(outputId = 'feeling', width = "100%", height = "100%"))
                )
            ),
            # Sobre
            bs4TabItem(
                tabName = 'about',
                fluidPage(
                    bs4Jumbotron(
                        title = "Guia para Criar",
                        lead = "Inteligência Analítica Para Todos!",
                        status = "primary",
                        btn_name = 'Acessar -  GUIA PARA CRIAR',
                        href = "https://www.guiaparacriar.com.br/"
                    )
                )
            )
        )
    ) 
)

