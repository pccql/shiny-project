

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Estados', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('state', 'Estado', state_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre o estado", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Incêndios no estado", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('state_comp', 'Estado', state_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),   
                fluidRow(
                    box(title = "Incêndios no estados", width = 12, solidHeader = TRUE,
                        plotOutput('line_graph_comp')
                    )
                ),   
                fluidRow(
                    box(title = "Médias de incêndios nos estados", width = 5, solidHeader = TRUE,
                        plotOutput('bar_graph_comp')
                    )
                ),    
                fluidRow(
                    box(title = "Valor de correlação entre os casos de incêndios nos estados", width = 12, solidHeader = TRUE,
                        DTOutput('correlation')
                    )
                ), 
                fluidRow(
                    box(title = "Scatterplot", width = 12, solidHeader = TRUE,
                        plotOutput('scatterplot')
                    )
                ),  
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
