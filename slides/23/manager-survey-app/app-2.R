# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(countrycode)

# Load data --------------------------------------------------------------------

manager_survey <- read_rds("manager-survey-processed.rds")

# Find all industries ----------------------------------------------------------

industry_choices <- manager_survey |>
  distinct(industry_other) |>
  arrange(industry_other) |>
  pull(industry_other)

# Randomly select 3 industries to start with -----------------------------------

selected_industry_choices <- sample(industry_choices, 3)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title = "Ask a Manager"),
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      checkboxGroupInput(
        inputId = "industry",
        label = "Select up to 8 industies:",
        choices = industry_choices,
        selected = selected_industry_choices
      )
    ),
    
    # Main panel
    mainPanel(
      hr(),
      "Showing only results for those with salaries in USD who have provided information on their industry and highest level of education completed.",
      br(), br(),
      textOutput(outputId = "selected_industries"),
      hr(),
      br(),
      tabsetPanel(
        type = "tabs",
        tabPanel("___", plotOutput(outputId = "___")),
        tabPanel("___", plotOutput(outputId = "___")),
        tabPanel("___", DT::dataTableOutput(outputId = "data"))
      )
    )
  )
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  
  # Print number of selected industries
  output$selected_industries <- reactive({
    str_c("You've selected", length(input$industry), "industries.",
          sep = " ")
  })
  
  # Filter data for selected industries
  manager_survey_filtered <- reactive({
    manager_survey |>
      filter(industry_other %in% input$industry)
  })
  
  # Make a table of filtered data
  output$data <- DT::renderDataTable({
    manager_survey_filtered() |>
      select(
        industry,
        job_title,
        annual_salary,
        other_monetary_comp,
        country,
        overall_years_of_professional_experience,
        years_of_experience_in_field,
        highest_level_of_education_completed,
        gender,
        race
      )
  })
  
  # Plot of jittered salaries from filtered data
  output$indiv_salary_plot <- renderPlot({
    
    ggplot(
      ___ ,
      aes(
        x = highest_level_of_education_completed,
        y = annual_salary,
        color = industry
      )
    ) +
      geom_jitter(size = 2, alpha = 0.6) +
      scale_x_discrete(labels = label_wrap_gen(10)) +
      scale_y_continuous(
        limits = input$ylim,
        labels = label_dollar()
      ) +
      theme(legend.position = "top")
  })
  
  # Plot of average salaries from filtered data
  output$avg_salary_plot <- renderPlot({
    
    
    ___ |>
      group_by(industry, highest_level_of_education_completed) |>
      summarise(
        mean_annual_salary = mean(annual_salary, na.rm = TRUE),
        .groups = "drop"
      ) |>
      ggplot(aes(
        x = highest_level_of_education_completed,
        y = mean_annual_salary,
        group = industry,
        color = industry
      )) +
      geom_line(linewidth = 1) +
      scale_x_discrete(labels = label_wrap_gen(10)) +
      scale_y_continuous(labels = label_dollar()) +
      theme(legend.position = "top")
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)