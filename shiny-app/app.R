library(shiny)
library(shinyjs)
library(purrr)
library(waiter)
library(glue)

source("transBinary2Dec.R")

ui <- fluidPage(
    includeCSS("www/styles.css"),
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    tags$br(),
    fluidRow(
        column(width = 2),
        column(
            width = 4, align = "center", offset = 2,
            div(
                id = "main",
                h3("Binary to Decimal converter"),
                textInput(
                    inputId = "binary_text",
                    label = "Binary",
                    placeholder = "Enter binary number here..."
                ),
                actionButton(inputId = "convert", label = "Convert"),
                br(), br(),
                textOutput(outputId = "decimal_text", container = shiny::span)
            )
        ),
        column(width = 2)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Limit #binary_text input to maximum 8 digits
    shinyjs::runjs("$('#binary_text').attr('maxlength', 8)")

    # Transforms binary to decimal digits
    decNumber <- eventReactive(input$convert, {
        req(input$binary_text)
        waiter <- waiter::Waiter$new(color = transparent(0.3))
        waiter$show()
        on.exit(waiter$hide())
        Sys.sleep(0.1)

        binary2dec(input$binary_text) |> as.character()
    })

    observeEvent(input$convert, {
        output$decimal_text <- renderText({
            req(input$binary_text)
            if (!is_binary(input$binary_text)) {
                validate("Input must be binary: Only 0 and 1 accepted")
            }

            glue::glue("Decimal number: {decNumber()}")
        })
    })

}

# Run the application
shinyApp(ui = ui, server = server)
