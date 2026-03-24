# Load packages ----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(countrycode)

# Load and prep data -----------------------------------------------------------

manager_survey <- read_csv(
  "https://stat220-s25.github.io/data/manager-survey.csv",
  na = c("", "NA"),
  show_col_types = FALSE
)

manager_survey <- manager_survey |>
  filter(
    !is.na(industry),
    !is.na(highest_level_of_education_completed),
    currency == "USD"
  ) |>
  mutate(
    industry_other = fct_lump_min(industry, min = 100),
    country = countrycode(country, origin = "country.name", destination = "cldr.name.en"),
    highest_level_of_education_completed = fct_relevel(
      highest_level_of_education_completed,
      "High School",
      "Some college",
      "College degree",
      "Master's degree",
      "Professional degree (MD, JD, etc.)",
      "PhD"
    ),
    highest_level_of_education_completed = fct_recode(
      highest_level_of_education_completed,
      "Professional degree" = "Professional degree (MD, JD, etc.)"
    )
  )

# Find all industries ----------------------------------------------------------

industry_choices <- manager_survey |>
  distinct(industry_other) |>
  arrange(industry_other) |>
  pull(industry_other)

# Randomly select 3 industries to start with -----------------------------------

selected_industry_choices <- sample(industry_choices, 3)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title = "Ask A Manager"),
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      # Checkbox for selecting industries
      checkboxGroupInput(
        inputId = "industry",
        label = "Select up to 8 industies:",
        choices = industry_choices,
        selected = selected_industry_choices
      )
    ),
    
    # Main panel
    mainPanel(
      # Informational text
      "Showing only results for those with salaries in USD who have provided information on their industry and highest level of education completed.",
      
      # Print number of selected industries
      textOutput(outputId = "selected_industries"),
      
      # Make a table of filtered data
      tabsetPanel(
        type = "tabs",
        tabPanel("Average Salaries", plotOutput(outputId = "avg_salary_plot")),
        tabPanel("Individual Salaries", plotOutput(outputId = "indiv_salary_plot")),
        tabPanel("Data", DT::dataTableOutput(outputId = "data"))
      )
    )
  )
)

# Define server function -------------------------------------------------------

server <- function(input, output, session) {
  
  # Filter data for selected industries
  manager_survey_filtered <- reactive({
    manager_survey %>%
      filter(industry %in% input$industry)
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
  
  # Make a plot of individual salaries
  output$indiv_salary_plot <- renderPlot({
    
    validate(
      need(length(input$industry) <= 8, "Please select a maxiumum of 8 industries.")
    )
    
    ggplot(
      manager_survey_filtered(),
      aes(
        x = highest_level_of_education_completed,
        y = annual_salary,
        color = industry
      )
    ) +
      geom_jitter(size = 2, alpha = 0.6) +
      scale_color_colorblind() +
      scale_x_discrete(labels = label_wrap_gen(10)) +
      scale_y_continuous(
        limits = input$ylim,
        labels = label_dollar()
      ) +
      labs(
        x = "Highest level of education completed",
        y = "Annual salary",
        color = "Industry",
        title = "Individual salaries"
      )
  })
  
  # Plot of average salaries from filtered data
  output$avg_salary_plot <- renderPlot({
    
    
    manager_survey_filtered() |>
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
      scale_color_colorblind() +
      scale_x_discrete(labels = label_wrap_gen(10)) +
      scale_y_continuous(labels = label_dollar()) +
      labs(
        x = "Highest level of education completed",
        y = "Mean annual salary",
        color = "Industry",
        title = "Average salaries"
      )
  })
  
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
