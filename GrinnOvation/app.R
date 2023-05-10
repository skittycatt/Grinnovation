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
employed_data <- data %>% filter(outcome == "employed")

# Create nodes and links for the Sankey diagram
majors <- sort(unique(c(employed_data$major_1, employed_data$major_2))) # Updated line
majors <- majors[!is.na(majors)]

graduate_fields <- unique(grad_school_data$grad_degree_field)
employment_fields <- unique(employed_data$employment_field)

node_labels_employed <- c(majors, employment_fields)
node_color_employed <- c(rep("blue", length(majors)), rep("green", length(employment_fields)))

node_labels_grad <- c(majors, graduate_fields)
node_color_grad <- c(rep("blue", length(majors)), rep("red", length(graduate_fields)))

link_table_employed <- employed_data %>%
  select(major_1, major_2, employment_field) %>%
  gather(key = "major_key", value = "major", -employment_field) %>%
  filter(!is.na(major), !is.na(employment_field)) %>% # Filter out rows with NA values
  group_by(major, employment_field) %>%
  summarise(value = n()) %>%
  ungroup()

link_table_grad <- grad_school_data %>%
  select(major_1, major_2, grad_degree_field) %>%
  gather(key = "major_key", value = "major", -grad_degree_field) %>%
  filter(!is.na(major), !is.na(grad_degree_field)) %>% # Filter out rows with NA values
  group_by(major, grad_degree_field) %>%
  summarise(value = n()) %>%
  ungroup()

# Employment
major_indices_employed <- match(link_table_employed$major, node_labels_employed) - 1
employment_field_indices <- match(link_table_employed$employment_field, node_labels_employed) - 1
link_label_employed <- paste(link_table_employed$major, "->", link_table_employed$employment_field)

# Graduate
major_indices_grad <- match(link_table_grad$major, node_labels_grad) - 1
graduate_field_indices <- match(link_table_grad$grad_degree_field, node_labels_grad) - 1
link_label_grad <- paste(link_table_grad$major, "->", link_table_grad$grad_degree_field)


# Add checkboxGroupInput for major selection
select_all_checkbox <- checkboxInput("select_all", "Select All", value = TRUE)
major_checkboxes <- checkboxGroupInput("major_checkboxes", "Select Majors:", choices = majors, selected = majors, inline = TRUE)
diagram_selector <- selectInput("diagram_selector", "Select Diagram:", choices = c("Employment Field", "Graduate School Field"), selected = "Employment Field")

# Vector of bib entries for packages
packs <- c(
  "shiny", "shinyjs", "shinyWidgets", "plotly",
  "readxl", "rjson", "dplyr", "tidyr", "shinyglide"
)
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

# Define UI for the application
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage(
    "GrinnOvation",
    tabPanel(
      "Home",
      h1("Welcome to the Home Page"),
      p("This is the home page."),
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
    tabPanel(
      "About",
      column(9,
        h1("About Us"),
        hr(),
        p("The purpose of GrinnOvation is to show Innovation at Grinnell mainly directed towards our Alumni audience.
                        Here we link innovation to the different activities Grinnell college students do whilst in college
                        like(Maps, and Student Orgs) and after graduating. GrinnOvation puts an effort into displaying student
                        opinion so that our users can get an idea of how it feels like to be a student here. As GrinnOvation our
                        goal is not to persuade Alumni to donate rather we're trying to show the Alumni, a problem or concern student
                        have and provide the Alumni with information on how they can get that donated."),
        offset = 1
      )
    ),
    tabPanel(
      "Design Process",
      column(12,
        includeHTML("design_process.html")
      )
    ),
    tabPanel(
      "Alumni Profiles",
      h1("Alumni Profiles"),
      hr(),
      div(class = "grid-container", generateAlumniCards()),
      br(),
      p("Alumni profiles created and compiled by the Grinnell College Office of Admissions.")
    ),
    tabPanel(
      "Student Opinions",
      column(9,
        includeHTML("student_opinions.html"),
        offset = 1
      )
    ),
    tabPanel("Mentored Advanced Projects (MAP)",
             h1("Mentored Advanced Projects (MAP)"),
             p("This is the mentored advanced projects (MAP) page."),
             select_all_checkbox,
             major_checkboxes,
             diagram_selector,
             plotlyOutput("sankey_plot", height = "auto", width = "100%")
    ),
    tabPanel(
      "Acknowledgements",
      tags$style( # Set lists to use bullets
        "ol { list-style:disc; }
              ul { list-style:disc; }"
      ),
      column(
        9,
        includeHTML("acknowledgements.html"),
        h2("Packages"),
        tags$ol(
          tags$li(HTML(formatCites("shinyjs"))),
          tags$li(HTML(formatCites("shiny"))),
          tags$li(HTML(formatCites("shinyWidgets"))),
          tags$li(HTML(formatCites("plotly"))),
          tags$li(HTML(formatCites("readxl"))),
          tags$li(HTML(formatCites("tidyr"))),
          tags$li(HTML(formatCites("shinyglide"))),
          tags$li(HTML(formatCites("dplyr")))
        ),
        offset = 1
      ),
      tags$script(
        HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top:12px;\"><a href=\"https://alumni.grinnell.edu/give\">Donate</a></div>')")
      )
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$select_all, {
    if (input$select_all) {
      updateCheckboxGroupInput(session, "major_checkboxes", selected = majors)
    } else {
      updateCheckboxGroupInput(session, "major_checkboxes", selected = character(0))
    }
    session$sendCustomMessage(type = "sankey_plot_update", message = list())
  })
  
  output$sankey_plot <- renderPlotly({
    if (input$diagram_selector == "Employment Field") {
      selected_majors <- input$major_checkboxes
      filtered_links <- link_table_employed %>%
        filter(major %in% selected_majors)
      
      if (nrow(filtered_links) > 0) {
        major_indices_employed <- match(filtered_links$major, node_labels_employed) - 1
        employment_field_indices <- match(filtered_links$employment_field, node_labels_employed) - 1
        
        link_label <- paste(filtered_links$major, "->", filtered_links$employment_field)
        
        fig_employment <- plot_ly(
          type = "sankey",
          domain = list(
            x = c(0, 1),
            y = c(0, 1)
          ),
          orientation = "h",
          valueformat = ".0f",
          valuesuffix = "TWh",
          
          node = list(
            label = node_labels_employed,
            color = node_color_employed,
            pad = 15,
            thickness = 15,
            line = list(
              color = "black",
              width = 0.5
            )
          ),
          
          link = list(
            source = major_indices_employed,
            target = employment_field_indices,
            value = filtered_links$value,
            label = link_label
          )
        ) %>%
          layout(
            title = "Major to Employment Field Transitions for Students Pursuing Employment",
            font = list(
              size = 10
            ),
            xaxis = list(showgrid = F, zeroline = F),
            yaxis = list(showgrid = F, zeroline = F)
          )
        
        # Update height based on the number of selected checkboxes
        sankey_height <- reactive({
          n_selected <- length(input$major_checkboxes)
          return(n_selected * 300)
        })
        fig_employment %>% layout(height = sankey_height())
      } else {
        plot_ly() %>% add_annotations(text = "No data to display", showarrow = FALSE, font = list(size = 24))
      }
    } 
    else 
    {
      selected_majors_grad <- input$major_checkboxes
      filtered_links_grad <- link_table_grad %>%
      filter(major %in% selected_majors_grad)
      
      if (nrow(filtered_links_grad) > 0) {
        major_indices_grad <- match(filtered_links_grad$major, node_labels_grad) - 1
        graduate_field_indices <- match(filtered_links_grad$grad_degree_field, node_labels_grad) - 1
        
        link_label_grad <- paste(filtered_links_grad$major, "->", filtered_links_grad$grad_degree_field)
        
        fig_graduate <- plot_ly(
          type = "sankey",
          domain = list(
            x = c(0, 1),
            y = c(0, 1)
          ),
          orientation = "h",
          valueformat = ".0f",
          valuesuffix = "TWh",
          
          node = list(
            label = node_labels_grad,
            color = node_color_grad,
            pad = 15,
            thickness = 15,
            line = list(
              color = "black",
              width = 0.5
            )
          ),
          
          link = list(
            source = major_indices_grad,
            target = graduate_field_indices,
            value = filtered_links_grad$value,
            label = link_label_grad
          )
        ) %>%
          layout(
            title = "Major to Graduate Field Transitions for Students Pursuing Graduate School",
            font = list(
              size = 10
            ),
            xaxis = list(showgrid = F, zeroline = F),
            yaxis = list(showgrid = F, zeroline = F)
          )
        
        # Update height based on the number of selected checkboxes
        sankey_height <- reactive({
          n_selected <- length(input$major_checkboxes)
          return(n_selected * 300)
        })
        fig_graduate %>% layout(height = sankey_height())
      } else {
        plot_ly() %>% add_annotations(text = "No data to display", showarrow = FALSE, font = list(size = 24))
      }
    }
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
