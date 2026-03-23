ui <- fluidPage(
  theme = bslib::bs_theme(),
  sidebarLayout(
    sidebarPanel(
      numericInput("num", "Number one", value = 0, min = 0, max = 100),
      sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
      selectInput("state", "What's your favourite state?", state.name),
      
    ),
    mainPanel(
      h1(paste0("Theme: Custom")),
      h2("Header 2"),
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )
)

server <- function(input, output, session) {
  bslib::bs_themer()
  output$distPlot <- renderPlot({
    ggformula::gf_function(dnorm, xlim = c(-4,4), size = 2) + ggplot2::theme_minimal() + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  })
}

shinyApp(ui = ui, server = server)