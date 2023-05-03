#Install necessary packages if you don't have them already
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("plotly")) install.packages("plotly")
if (!require("readxl")) install.packages("readxl")
if (!require("rjson")) install.packages("rjson")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

# Load necessary packages
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(readxl)
library(rjson)
library(dplyr)
library(tidyr)

source("alumni_cards.R")

# Add resource path for 'www' folder
addResourcePath("www", "www")

# Read the data from the Excel file
data <- read_excel("MAP_Outcomes.xlsx")

# Filter rows with outcome as 'graduate school'
grad_school_data <- data %>% filter(outcome == "graduate school")

# Create nodes and links for the Sankey diagram
majors <- unique(c(grad_school_data$major_1, grad_school_data$major_2))
grad_degree_fields <- unique(grad_school_data$grad_degree_field)
node_labels <- c(majors, grad_degree_fields)
node_color <- c(rep("blue", length(majors)), rep("green", length(grad_degree_fields)))

# Create a table of links between major and grad_degree_field
link_table <- grad_school_data %>%
  select(major_1, major_2, grad_degree_field) %>%
  gather(key = "major_key", value = "major", -grad_degree_field) %>%
  filter(!is.na(major), !is.na(grad_degree_field)) %>%  # Filter out rows with NA values
  group_by(major, grad_degree_field) %>%
  summarise(value = n()) %>%
  ungroup()

major_indices <- match(link_table$major, node_labels) - 1
grad_degree_field_indices <- match(link_table$grad_degree_field, node_labels) - 1

link_label <- paste(link_table$major, "->", link_table$grad_degree_field)

fig <- plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
  node = list(
    label = node_labels,
    color = node_color,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = major_indices,
    target = grad_degree_field_indices,
    value =  link_table$value,
    label = link_label
  )
) 
fig <- fig %>% layout(
  title = "Major to Graduate Degree Field Transitions for Students Pursuing Graduate School",
  font = list(
    size = 10
  ),
  xaxis = list(showgrid = F, zeroline = F),
  yaxis = list(showgrid = F, zeroline = F)
)

# Define UI for the application
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  tags$head(
    tags$script(src = "www/custom.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage("GrinnOvation",
             tabPanel("Home",
                      h1("Welcome to the Home Page"),
                      p("This is the home page."),
                      actionButton("btn", "Click me")
             ),
             tabPanel("About",
                      h1("About Us"),
                      p("This is the about page.")
             ),
             tabPanel("Design Process",
                      h1("Design Process"),
                      p("This is the design process page.")
             ),
             tabPanel("Alumni Profiles",
                      h1("Alumni Profiles"),
                      fluidRow(
                        generateAlumniCards()
                      ),
                      p("This is the alumni profiles page.")
             ),
             tabPanel("Student Opinions",
                      includeHTML("student_opinions.html")
             ),
             tabPanel("Mentored Advanced Projects (MAP)",
                      h1("Mentored Advanced Projects (MAP)"),
                      p("This is the mentored advanced projects (MAP) page."),
                      # Add Sankey diagram in the MAP tab
                      plotlyOutput("sankey_plot", height = "4800px", width = "100%")
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$sankey_plot <- renderPlotly({
    fig
  })
}

# Run the application
shinyApp(ui = ui, server = server)
