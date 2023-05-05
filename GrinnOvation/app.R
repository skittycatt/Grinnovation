# Install necessary packages if you don't have them already
if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("plotly")) install.packages("plotly")
if (!require("readxl")) install.packages("readxl")
if (!require("rjson")) install.packages("rjson")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("shinyglide")) install.packages("shinyglide")

# Load necessary packages
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(readxl)
library(rjson)
library(dplyr)
library(tidyr)
library(shinyglide)

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
  filter(!is.na(major), !is.na(grad_degree_field)) %>% # Filter out rows with NA values
  group_by(major, grad_degree_field) %>%
  summarise(value = n()) %>%
  ungroup()

# Vector of bib entries for packages
packs <- c("shiny", "shinyjs", "shinyWidgets", "plotly",
    "readxl", "rjson", "dplyr", "tidyr", "shinyglide")
cites <- c(
  citation("shiny"),
  citation("shinyjs"),
  citation("shinyWidgets"),
  citation("plotly"),
  citation("readxl"),
  citation("rjson"),
  citation("dplyr"),
  citation("tidyr"),
  citation("shinyglide")
)

# Properly formats citation of a package
# Input: Package name as string
# Output: Citation as string
formatCites <- function(cit) {
  # Find the index of the given package and pull its bib entry
  ci <- cites[which(packs == cit)]
  # Pull the first and last names of all the authors
  authFirst <- sapply(strsplit(paste(ci$author), "\\s+"), "[[", 1)
  authLast <- sapply(strsplit(paste(ci$author), "\\s+"), "[[", 2)
  auths <- c()
  x <- 1
  # Append all authors to auth vector in the format LastName FirstInitial.
  repeat{
    auths <- append(auths, paste(authLast[x], " ",
      substring(authFirst[x], 1, 1), ".",
      sep = ""
    ))
    x <- x + 1
    if (x > length(authFirst)) break
  }
  # Collapse the auth vector to a single string
  auths <- paste0(auths, collapse = ", ")
  # Paste together the bib chunks in the correct order
  paste(auths, " (", ci$year, "). ", em(paste(ci$title)), ". R package version ", packageVersion(cit), ", ", a(href = ci$url, ci$url, .noWS = "after"), ".",
    sep = ""
  )
}

major_indices <- match(link_table$major, node_labels) - 1
grad_degree_field_indices <- match(link_table$grad_degree_field, node_labels) - 1

link_label <- paste(link_table$major, "->", link_table$grad_degree_field)

fig <- plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0, 1),
    y =  c(0, 1)
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
    value = link_table$value,
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
                      #actionButton("btn", "Click me"),
                      glide(
                        height = "350px",
                        screen(
                          img(src = "cultural_evening_Int_stu_Affairs_website.jpg", height = "300px")
                        ),
                        screen(
                          img(src = "DAR Tie dye event.jpeg", height = "300px")
                        ),
                        screen(
                          img(src = "foodbazzar_ISA_website.jpeg", height = "400px", width = "100%")
                        ),
                        screen(
                          img(src = "gallery_offlags_ISA_website.jpeg", height = "300px", width = "100%")
                        ),
                        screen(
                          img(src = "LatinAmericanEnsembele_by_Ohana_Sarvotham_snb.jpeg", height = "500px", width = "100%")
                        ),
                        screen(
                          img(src = "paul_hansen_snb_pedal_grinnell.jpg", height = "500px", width = "100%")
                        ),
                        screen(
                          img(src = "Softball.EvanHein_snb.jpg", height = "500px", width = "100%")
                        ),
                        screen(
                          img(src = "WSOC_Champ_by_Tali_Berk_snb.jpeg", height = "500px", width = "100%")
                        ),
                        screen(
                          img(src = "Zimbabwean_Mbira_ensemble_by_Owen_barbato_snb.jpeg", height = "500px", width = "100%")
                        )
                      )
             ),
             tabPanel("About",
                      h1("About Us"),
                      p("The purpose of GrinnOvation is to show Innovation at Grinnell mainly directed towards our Alumni audience. 
                        Here we link innovation to the different activities Grinnell college students do whilst in college 
                        like(Maps, and Student Orgs) and after graduating. GrinnOvation puts an effort into displaying student 
                        opinion so that our users can get an idea of how it feels like to be a student here. As GrinnOvation our 
                        goal is not to persuade Alumni to donate rather we’re trying to show the Alumni, a problem or concern student
                        have and provide the Alumni with information on how they can get that donated.")
             ),
             tabPanel("Design Process",
                      h1("Design Process"),
                      p("This is the design process page.")
             ),
             tabPanel("Alumni Profiles",
                      h1("Alumni Profiles"),
                      div(class="grid-container", generateAlumniCards()),
                      br(),
                      p("Alumni profiles created and compiled by the Grinnell College Office of Admissions.")
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
