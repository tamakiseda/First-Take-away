#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(tidyverse)
library(ggplot2)


#get data set 
airdata<-arrange(subset(read.csv(file="https://raw.githubusercontent.com/tamakiseda/First-Take-away/master/air.csv",fileEncoding="UTF-8-BOM")))
#clean data
airdata<-na.omit(airdata)
airdata$Month<-month.abb[airdata$Month]

list_choices <-  unique(airdata$Month)[!is.na(unique(airdata$Month))]
names(list_choices) <- paste(unique(airdata$Month)[!is.na(unique(airdata$Month))],sep="")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 tabPanel("Introduction",
                          includeMarkdown("introduction.md")
                 ),
                 tabPanel("Data of air quality",
                          fluidPage( 
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("select", label = h3("Plot by month"), 
                                                  choices = character(0),
                                                  selected = 1)
                                  ), # sidebarPanel
                                  mainPanel(
                                      plotOutput(outputId = "plot", click = "plot_click"),
                                      tableOutput("info")
                                  ) # mainPanel
                              ) # sidebarLayout
                          ) # fluidPage
                 ), #  tabPanel
                 tabPanel("Random generator",
                          useShinyjs(),
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                            selectInput("dist", label = h3("Select the distribution"), 
                                                        choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                                                        selected = 1),
                                            sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                                                        max = 100, value = 50),
                                            actionButton("goButton", "Go!"),
                                            fluidRow(
                                                h3(style = "margin-left: 20px; margin-bottom: 0px;", "Number of bins"),
                                                column(2,
                                                       div(style = "margin-top: 37px", checkboxInput("auto_bins", label = "auto", value = TRUE))
                                                ),
                                                column(10,
                                                       sliderInput("n_bins", label="", min = 1, max = 50, value = 30)
                                                )
                                            ), # fluidRow
                                            downloadButton("report", "Generate report")
                                        ), # sidebarPanel
                                        mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Plot", plotOutput("histPlot")),
                                                        tabPanel("Summary", verbatimTextOutput("histSummary")),
                                                        tabPanel("Table", tableOutput("histTable"))
                                            )
                                        ) # mainPanel
                          ) # sidebarLayout
                 )
) # navbarPage

col_scale <- scale_colour_discrete(limits = unique(airdata$Month))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Can also set the label and select items
    updateSelectInput(session, "select",
                      choices = list_choices,
                      selected = tail(list_choices, 1)
    );
    attach(airdata)
    output$plot <- renderPlot({
        if(input$select != ""){
            # cat(file=stderr(), "input$select:", input$select == "", "\n")
            ggplot(airdata %>% filter(Month == input$select), aes(Ozone,Wind, colour = Month)) +
                scale_x_log10() +
                col_scale +
                geom_point()
        }
    });
    
    output$info <- renderTable({
        if(input$select != ""){
            nearPoints(airdata 
                       %>% filter(Month== input$select) 
                       %>% select( Ozone, Wind, Solar.R), 
                       input$plot_click, threshold =0, maxpoints = 1
            )
        }
    })
    
    samples <- reactive({
        input$goButton
        dist <- eval(parse(text=paste(input$dist)))
        dist(isolate(input$n_sample))
    })
    
    observe(if(input$auto_bins) disable("n_bins") else enable("n_bins"))
    
    output$histPlot <- renderPlot(
        hist(samples(), main="Random Generation", 
             breaks = if(!input$auto_bins) {input$n_bins} else {"Sturges"})
    )
    output$histSummary <- renderPrint(summary(samples()))
    output$histTable <- renderTable(samples())
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            params <- list(
                n_sample = isolate(input$n_sample), 
                dist = isolate(input$dist), 
                breaks = if(!isolate(input$auto_bins)) {isolate(input$n_bins)} else {"Sturges"}
            )
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
}

shinyApp(ui = ui, server = server)
