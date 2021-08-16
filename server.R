
# Define server logic required to draw a histogram
server <- function(input, output) {
    ################### INPUT ####################
    select_state <- eventReactive(input$go, {
        
        state_name <- input$state
        twin <- input$true_date

        df_state <- master_df %>% filter(state_name == state)

        df_state_date <- df_state %>% filter(Date >= twin[1] & Date <= twin[2])
        print(df_state_date)
        return(df_state_date)
    })
    
    output$timedate <- renderUI({
        
        state_name <- input$state
        
        df <- master_df %>% 
            filter(state == state_name)
        
        min_time <- min(df$Date)
        max_time <- max(df$Date)
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate_comp <- renderUI({
        
        state_name <- input$state
        
        df <- master_df %>% 
            filter(state %in% state_name)
        
        maxmin_time <- df %>% 
            group_by(state) %>% 
            summarise(MD = min(Date)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            group_by(state) %>% 
            summarise(MD = max(Date)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        df <- select_state()

        numbers <- df %>% select(number)

        mean <- numbers %>% colMeans()
        Média <- mean[[1]]

        median <- numbers
        Mediana <- median(median[[1]])

        moda<-function(x){which.max(tabulate(x))}
        Moda <- moda((numbers)[[1]])

        standDeviation <- numbers
        DesvioPadrão <- sd(standDeviation[[1]])

        ValorMáximo<- max(numbers[[1]])
        ValorMínimo<- min(numbers[[1]])
        

        Estado <- input$state
        
        df_tb <-  data.frame(Estado, Média, Mediana, Moda, DesvioPadrão, ValorMáximo, ValorMínimo)

        df_tb <- as.data.frame(t(df_tb))
        
        # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
        # tb <- tb %>% 
        #     rename('Informações' = nms,
        #            'Valores' = V2)
        # 
        return(df_tb)
    })
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
    
    output$sh <- renderPlot({
        # All the inputs
        df <- select_state()
        
        aux <- df$number %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        df$Date <- ymd(df$Date)
        a <- df %>% 
            ggplot(aes(Date, number, group=1)) +
            geom_path() +
            ylab('Número de Ocorrências no estado') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    })
}
