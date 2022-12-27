#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

theme_set(theme_light())

data <- read_csv("example/Organoid q=0.9_Distance_7_Neighbors.csv")

tidyInput <- . %>%
    select(-Dist0) %>% 
    mutate(id=row_number()) %>%
    pivot_longer(names_to="what", values_to="value", Fate1:Dist7) %>%
    mutate(neighbor_number=str_sub(what,5), what=str_sub(what,1,4)) %>%
    pivot_wider(values_from=value, names_from=what) %>%
    mutate(Fate=as_factor(Fate)) %>%
    rename(truth=`Ground Trouth`) %>%
    identity

tidyData <- tidyInput(data)

fate_color = c("#df5757","#6495ed")

plot_example <- function(data, example){
    data %>%
        filter(id==example) %>%
        ggplot(aes(Dist, neighbor_number, color=Fate)) +
        geom_point(size=10) +
        xlim(.5, 2.5) +
        scale_color_manual(values=c("0"=fate_color[1], "1"=fate_color[2])) +
        theme(legend.position="none") +
        xlab("distance") + ylab("neighbor")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("This is what you ask your network to do:"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Predict Fate"),
            actionButton("c1", "Fate 0", style=str_c("background-color: ",fate_color[1])),
            actionButton("c2", "Fate 1", style=str_c("background-color: ",fate_color[2])),
            hr(),
            actionButton("show_learning", "Show/Hide learning history"),
            hr(),
            fileInput("ownData", "Use own data (.csv)", accept=".csv"),
            actionButton("shuffle", "Shuffle"),
            hr(),
            downloadButton("downloadData", "Download Predictions")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("examplePlot"),
           htmlOutput("isCorrect"),
           tableOutput("percentage"),
           plotOutput("learningPlot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    v <- reactiveValues(
        current_example = 1,
        show_learning = FALSE,
        predictions = tibble(example=numeric(0),truth=numeric(0),prediction=numeric(0)),
        data = tidyData,
        example_order = seq(max(tidyData$id)),
        filename = "Organoid q=0.9_Distance_7_Neighbors.csv"
    )
    observeEvent(input$ownData, {
        file <- input$ownData
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        
        v$data <- tidyInput(read_csv(file$datapath))
        v$example_order <- seq(max(v$data$id))
        v$current_example <- 1
        v$predictions <- tibble(example=numeric(0),truth=numeric(0),prediction=numeric(0))
        v$filename <- file$name
    })
    observeEvent(input$show_learning, {v$show_learning=!v$show_learning})
    observeEvent(input$shuffle, {
        v$example_order<-sample(v$example_order)
        v$current_example<-1
    })
    observeEvent(input$c1, {
        current_id <- v$example_order[v$current_example]
        truth <- v$data %>%
            filter(id==current_id) %>%
            pull(truth) %>%
            unique()
        v$predictions <- add_row(v$predictions, tibble(example=current_id, truth=truth, prediction=0))
        v$current_example <- (v$current_example + 1) %% max(v$data$id)
    })
    observeEvent(input$c2, {
        current_id <- v$example_order[v$current_example]
        truth <- v$data %>%
            filter(id==current_id) %>%
            pull(truth) %>%
            unique()
        v$predictions <- add_row(v$predictions, tibble(example=current_id, truth=truth, prediction=1))
        v$current_example <- (v$current_example + 1) %% max(v$data$id)
    })
    output$isCorrect <- renderText({
        if(nrow(v$predictions)<1){
            return()
        }
        isCorrect <- v$predictions %>%
            tail(1) %>%
            mutate(isCorrect=if_else(truth==prediction,"correct","incorrect")) %>%
            pull(isCorrect)
        textCol <- "red"
        if(isCorrect=="correct"){
            textCol <- "green"
        }
        return(str_c("<h3><b style='color: ",textCol,"'>",isCorrect,"</b></h3>"))
    })
    output$examplePlot <- renderPlot({
        plot_example(v$data, v$example_order[v$current_example])
    })
    output$percentage <- renderTable({
        if(nrow(v$predictions)==0){
            return()
        }
        v$predictions %>% 
            summarize(guesses=n(),correct=sum(truth==prediction)) %>%
            mutate(incorrect=guesses-correct,accuracy=correct/guesses)
    })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("predictions-", format(Sys.time(), "%Y_%m_%d__%H_%M-"), v$filename, sep="") %>% str_replace("\\.csv$", ".tsv")
        },
        content = function(file) {
            write_tsv(v$predictions, file)
        }
    )
    output$learningPlot <- renderPlot({
        if(!v$show_learning || nrow(v$predictions)<1){
            return()
        }
        v$predictions %>%
            mutate(
                guess=row_number(),
                correct=truth==prediction,
                total = cumsum(correct),
                accuracy = total/guess
            ) %>%
            ggplot(aes(x=guess, y=accuracy)) +
                geom_line() +
                geom_point(aes(y=-.1, color=as_factor(prediction), size=5)) +
                geom_point(aes(y=-.15, color=as_factor(truth), size=5)) +
                theme(legend.position="none") +
                scale_color_manual(values=c("0"=fate_color[1], "1"=fate_color[2])) +
                scale_y_continuous(breaks = c(-.15,-.1,0,.25,.50,.75,1.00), labels = c("truth","pred","0%","25%","50%","75%","100%"))
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
