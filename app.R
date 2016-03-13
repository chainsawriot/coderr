## codebook <- list(list(var = "Q1", prompt = "Wie geht es Ihnen?", type = "text"),
##                  list(var = "Q2", prompt = "Wie ist diem name?", type = 'text'),
##                  list(var = "Q3", prompt = "2 + 2 = ?", type = 'numeric'),
##                  list(var = "Q4", prompt = "It is good right?", type = "multi", options = c("Yes", "No"))
##                  )

simple_codebook <- list(list(var = "Q1", prompt = "Wie geht es Ihnen?", type = 'text'),
                        list(var = "Q2", prompt = "Wie ist diem name?", type = 'text'),
                        list(var = "Q3", prompt = "2 + 2 = ?", type = 'numeric'),
                        list(var = "Q4", prompt = "It is good right?", type = "multi", options = c("Yes", "No")))

require(shiny)

handle_item <- function(item) {
    if (item['type'] == "text") {
        return(gen_textinput(item))
    } else if (item['type'] == 'numeric') {
        return(gen_numinput(item))
    } else if (item['type'] == 'multi') {
        return(gen_radio(item))
    }
}

gen_radio <- function(item) {
    radioButtons(inputId = item['var'], label = item['prompt'], choices = item['options'][[1]])
}

gen_textinput <- function(item) {
    textInput(inputId = item['var'], label = item['prompt'])
}


gen_numinput <- function(item) {
    numericInput(inputId = item['var'], label = item['prompt'], value = 0)
}



server <- function(input, output) {
    output$datab <- renderDataTable(data.frame(a = input['Q1'], input$Q2, input$Q3))
}

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(lapply(simple_codebook, handle_item), submitButton(text = "submit")),
        mainPanel(h1("Testing"), dataTableOutput("datab"))
))

shinyApp(ui = ui, server = server)
