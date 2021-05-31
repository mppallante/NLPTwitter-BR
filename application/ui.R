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

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
    
    # Opções
    fullscreen = TRUE,
    help = FALSE,
    dark = FALSE,
    scrollToTop = FALSE,
    
    # Navbar (Menu Superior) 
    header = bs4DashNavbar(
        disable = FALSE, 
        fixed = TRUE,
        border = TRUE,
        compact = FALSE,
        skin = "light",
        status = "white",
        sidebarIcon = shiny::icon("bars"),
        controlbarIcon = shiny::icon("th"),
        # Cabeçalho do Dashboard
        title = dashboardBrand(
            title = "©MPPallante",
            color = "primary",
            image = "https://lh3.googleusercontent.com/ogw/ADGmqu_hZZbh1ioBDSRRb8W85PrmMbB07wcshDOJcM8V9g=s83-c-mo", 
            href = "https://mppallante.wixsite.com/mppallante",
            opacity = 0.8
        ),
        # Caixa de Mensagens
        rightUi = tagList(
            dropdownMenu(
                headerText = "Você tem 1 notificação",
                badgeStatus = "danger",
                type = "messages",
                icon = shiny::icon("bell"),
                messageItem(
                    inputId = "triggerAction1",
                    from = HTML("<strong>Desenvolvedor</strong>"),
                    message = HTML("Atualização realizada!
                         <br>Layout:2.0
                         <br>R: 4.1.0
                         <br>Rstudio: 1.4.1106"),
                    image = "https://lh3.googleusercontent.com/ogw/ADGmqu_hZZbh1ioBDSRRb8W85PrmMbB07wcshDOJcM8V9g=s83-c-mo",
                    time = "Hoje",
                    color = "navy",
                    icon = shiny::icon("code")
                )
            )
        )
    ),
    
    # Sidebar (Menu Lateral)
    sidebar = bs4DashSidebar(
        # Opções
        id = "sidebar",
        disable = FALSE,
        fixed = TRUE,
        collapsed = FALSE,
        minified = TRUE,
        expandOnHover = TRUE,
        width = NULL,
        elevation = 4,
        skin = "light",
        status = "primary",
        customArea = NULL,
        # Segundo Titulo
        sidebarUserPanel(
            name = HTML("<strong>NLP TWITTER BRASIL</strong>")
        ),
        # Menu
        sidebarMenu(
            sidebarHeader("APLICAÇÃO"),
            # Página 1
            menuItem(
                text = "Análise de tweets",
                tabName = "tweets_twitter",
                icon = shiny::icon("twitter"),
                selected = TRUE
            ),
            # Página 2
            sidebarHeader("INFORMAÇÕES"),
            menuItem(
                text = "Aplicação",
                tabName = "about",
                icon = shiny::icon("info")
            )
        )
    ),
    
    # Controlbar (Menu de Controles)
    controlbar = dashboardControlbar(
        # Opções
        id = "controlbar",
        disable = FALSE,
        pinned = FALSE,
        collapsed = TRUE,
        overlay = FALSE,
        width = 250,
        skin = "light",
        controlbarMenu(
            # Opções
            id = "controlbarMenu",
            type = "pills",
            selected = "Controles",
            #  Menu de Controles
            controlbarItem(
                title = "Controles"
                
            ),
            # Menu de temas
            controlbarItem(
                title = "Temas",
                skinSelector()
            )
        )
    ),
    
    # Main Body (Corpo Principal)
    body = bs4DashBody(
        bs4TabItems(
            # Página 1 - Análise de tweets
            bs4TabItem(
                use_waiter(),
                tabName = "tweets_twitter",
                # Filtro
                bs4Card(
                    title = "Configuração", 
                    closable = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    maximizable = FALSE,
                    solidHeader = TRUE, 
                    elevation = 4,
                    width = 12,
                    height = "auto",
                    status = "navy",
                    # Filtro 
                    pickerInput(inputId = "tema", 
                                label = "Assuntos:", 
                                choices = c("Brasil","COVID-19","Bolsonaro","São Paulo"), 
                                width = "100%", 
                                inline = F)
                ),
                # Nuvem de Palavras
                bs4Card(
                    title = "Nuvem de Palavras", 
                    closable = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    maximizable = TRUE,
                    solidHeader = TRUE, 
                    elevation = 4,
                    width = 12,
                    height = 400,
                    status = "primary",
                    # Gráfico 
                    echarts4rOutput(outputId = "cloudTweet", width = "100%", height = "100%")
                ),
                # Top 15 Ocorrências de Palavras
                bs4Card(
                    title = "Top 15 Ocorrências de Palavras", 
                    closable = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    maximizable = TRUE,
                    solidHeader = TRUE, 
                    elevation = 4,
                    width = 12,
                    height = 400,
                    status = "primary",
                    # Gráfico 
                    echarts4rOutput(outputId = "topPalavras", width = "100%", height = "100%")
                ),
                fluidRow(
                    # Modeagem de Tópicos
                    bs4Card(
                        title = "Modeagem de Tópicos",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "primary",
                        # Gráfico
                        plotOutput(outputId = "topics", width = "100%", height = "100%")
                    ),
                    # Rede de Palavras
                    bs4Card(
                        title = "Rede de Palavras",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "primary",
                        # Gráfico
                        #visNetworkOutput(outputId = "wordsNetwork", width = "100%", height = "100%")
                        plotOutput(outputId = "wordsNetwork", width = "100%", height = "100%")
                    )
                ),
                # Análise de Sentimentos
                bs4Card(
                    title = "Análise de Sentimentos", 
                    closable = FALSE,
                    collapsible = FALSE,
                    collapsed = FALSE,
                    maximizable = TRUE,
                    solidHeader = TRUE, 
                    elevation = 4,
                    width = 12,
                    height = 400,
                    status = "primary",
                    # Gráfico 
                    echarts4rOutput(outputId = "feeling", width = "100%", height = "100%")
                ),
                fluidRow(
                    # Top 15 Ocorrências sobre Tags
                    bs4Card(
                        title = "Top 15 Ocorrências sobre Tags",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "purple",
                        # Gráfico
                        echarts4rOutput(outputId = "topTags", width = "100%", height = "100%")
                    ),
                    # Rede de Palavras sobre as Tags
                    bs4Card(
                        title = "Rede de Palavras sobre as Tags",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "purple",
                        # Gráfico
                        #visNetworkOutput(outputId = "tagsNetwork", width = "100%", height = "100%")
                        plotOutput(outputId = "tagsNetwork", width = "100%", height = "100%")
                    )
                ),
                fluidRow(
                    # Top 15 Ocorrências de Menções
                    bs4Card(
                        title = "Top 15 Ocorrências de Menções",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "purple",
                        # Gráfico
                        echarts4rOutput(outputId = "topMenções", width = "100%", height = "100%")
                    ),
                    # Rede de Palavras sobre as Menções
                    bs4Card(
                        title = "Rede de Palavras sobre as Menções",
                        closable = FALSE,
                        collapsible = FALSE,
                        collapsed = FALSE,
                        maximizable = TRUE,
                        solidHeader = TRUE,
                        elevation = 4,
                        width = 6,
                        height = 400,
                        status = "purple",
                        # Gráfico
                        #visNetworkOutput(outputId = "MençõesNetwork", width = "100%", height = "100%")
                        plotOutput(outputId = "MençõesNetwork", width = "100%", height = "100%")
                    )
                )
            ),
            # Página 2 - Aplicação
            bs4TabItem(
                tabName = "about",
                use_waiter(),
                bs4Jumbotron(
                    title = "NLP TWITTER BRASIL - GITHUB",
                    lead = "Desenvolvido para análise de textos do Twitter.",
                    status = "primary",
                    btnName = "GITHUB",
                    href = "https://github.com/mppallante/NLPTwitter-BR"
                )
            )
        )
    ),
    
    # Footer
    footer = dashboardFooter(
        fixed = TRUE,
        left = a(
            href = "https://mppallante.wixsite.com/mppallante",
            target = "_blank", "©MPPallante. Todos os direitos reservados."
        ),
        right = lubridate::year(Sys.time())
    )

)