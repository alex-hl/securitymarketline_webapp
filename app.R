#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(tidyverse)
library(tidyquant)
library(plotly)
library(rmarkdown)
library(markdown)

rf_instruments <- list("4-Week Treasury Bill Market Rate" = "DTB4WK",
                       "3-Month Treasury Bill Market Rate" = "DTB3",
                       "6-Month Treasury Bill Market Rate" = "DTB6",
                       "1-Year Treasury Bill Market Rate" = "DTB1YR",
                       "2-Year Treasury Constant Maturity Rate" = "DGS2",
                       "5-YEAR Treasury Constant Maturity Rate" = "DGS5",
                       "10-YEAR Treasury Constant Maturity Rate" = "DGS10",
                       "30-YEAR Treasury Constant Maturity Rate" = "DGS30")



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Security Characteristic Line",

                tabPanel("Calculator",
                    titlePanel("Find which security generated positive alpha along with its beta coefficient (5Y Monthly Basis)"),
                    titlePanel(""),
                    sidebarPanel(textInput("ticker", 
                                           label = "Enter a Stock Ticker", 
                                           value = "TSLA"),
                                 selectInput("rf", 
                                             "Instrument to be used as the risk free rate", 
                                             choices = rf_instruments, 
                                             selected = rf_instruments[3]),
                                 actionButton("Compute", 
                                              "Compute", 
                                              class = "btn btn-primary")),
                    mainPanel(plotlyOutput('graph'),
                              h4("")),
                    sidebarPanel(h4('Results:'),
                                 uiOutput("alpha"),
                                 uiOutput("beta"),
                                 uiOutput("r2"))),
                tabPanel("About",
                         div(withMathJax(includeMarkdown("about.Rmd")),align="justify"))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    datasetInput <- reactive({
        
        if(!is.data.frame(tq_get(input$ticker))){NA} else {
        getSymbols(input$rf,src='FRED')
        riskfreerate <- as.data.frame(eval(parse(text = input$rf)))
        riskfreerate <- riskfreerate %>% 
            mutate(date = as.Date(rownames(riskfreerate)), rfmonthly = eval(parse(text = input$rf))/12) %>% 
            select(date, rfmonthly)
        row.names(riskfreerate) <- 1:nrow(riskfreerate)
        
        # Some days are missing, this loop fills these values in with the same rate as the previous day
        for(i in 1:length(riskfreerate$rfmonthly)){ 
            if(is.na(riskfreerate$rfmonthly[i])){
                riskfreerate$rfmonthly[i] <- riskfreerate$rfmonthly[i-1]
            }
        }
        
        stock <- tq_get(input$ticker) %>% select(date, close_stock = close)
        sp <- tq_get("^GSPC") %>% select(date, close_mkt = close)
        
        stock <-  stock %>% 
            filter(date <= (floor_date(today(), "month")-1) & date >= (floor_date(today(), "month")-1 - years(5)))
        temp_stock <- stock %>% 
            group_by(yearMon = as.yearmon(date)) %>%
            summarise(LastDay = last(date)) %>%
            rename(date = LastDay)
        stock <- stock %>% inner_join(temp_stock, by = "date")
        stock <- stock[order(nrow(stock):1),]
        
        df <- stock %>% 
            inner_join(riskfreerate, by = "date") %>% 
            inner_join(sp, by = "date")
        
        monthly_ret_stock <- c() 
        monthly_ret_mkt <- c()
        
        for(i in 1:length(df$close_stock)){
            monthly_ret_stock[i] <- (df$close_stock[i] - df$close_stock[i+1]) / df$close_stock[i+1]
            monthly_ret_mkt[i] <- (df$close_mkt[i] - df$close_mkt[i+1]) / df$close_mkt[i+1]
        }
        
        df <- df %>% mutate(monthly_ret_stock = monthly_ret_stock*100, monthly_ret_mkt = monthly_ret_mkt*100) %>% drop_na() %>% 
            mutate(excess_ret_stock =  monthly_ret_stock - rfmonthly,
                   excess_ret_mkt = monthly_ret_mkt - rfmonthly)
        
        df}
        })
    
    modelInput <- reactive({ 
        
        if(!is.data.frame(datasetInput())){x <- NA}
        else if (nrow(datasetInput()) >= 5){
            mod <- lm(data = datasetInput(), formula = excess_ret_stock~excess_ret_mkt)
            x <- mod
        } else {
            x <- NA
        
        }
        x
        })     
    
    output$alpha <- renderUI({
        if (input$Compute>0 ){isolate(
        if (is.list(modelInput())) {
            withMathJax(paste0("$$\\alpha =", round(modelInput()$coefficients[1], 3), "$$"))
        }
        else {
            print("You have entered an invalid ticker or the ticker of a company that has been IPO too recently (last 6 months).")
        }
        )}
        })
    
    output$beta <- renderUI({
        if (input$Compute>0 ){isolate(
        if (is.list(modelInput())){
            withMathJax(paste0("$$\\beta =", isolate(round(modelInput()$coefficients[2], 3)), "$$"))
        } else {print("")}
        )}
        })
    
    output$r2 <- renderUI({
        if (input$Compute>0 ){isolate(
        if (is.list(modelInput())){
            withMathJax(paste0("$$R^2 =", isolate(round(summary(modelInput())$r.squared, 3)), "$$"))
        } else(print(""))
        )}
        })
    
    plotInput <- reactive({
        p <- datasetInput() %>% 
            ggplot(aes(x = excess_ret_mkt, y = excess_ret_stock)) +
            geom_point(aes(text = paste0(paste(month(date, label = TRUE, abbr = FALSE), year(date)), "</br></br></br>", 
                                         "Market's Excess Returns: ", round(excess_ret_mkt), "%", 
                                         "</br>", input$ticker, "'s Excess Returns: ", round(excess_ret_stock), "%"))) +
            geom_hline(yintercept = 0) +
            geom_vline(xintercept = 0) + 
            geom_smooth(mapping = aes(x = excess_ret_mkt, y = excess_ret_stock), method = "lm", se = FALSE) +
            theme_bw() +
            labs(title = paste(input$ticker, "Characteristic Line (Interactive)"), 
                 y = paste(input$ticker, "Excess Rerturns (%)"), 
                 x = "Excess Rerturns SP&500 (%)") 
        
        ggplotly(p, tooltip = "text")
        
    })
    
    output$graph <- renderPlotly({
        if (input$Compute>0){isolate(
        if (is.list(modelInput())){
            isolate(plotInput())
        }
        )}
        })
    
}




# Run the application 
shinyApp(ui = ui, server = server)
