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
majors <- majors[!is.na(majors)]
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

major_indices <- match(link_table$major, node_labels) - 1
grad_degree_field_indices <- match(link_table$grad_degree_field, node_labels) - 1

link_label <- paste(link_table$major, "->", link_table$grad_degree_field)

fig_graduate <- plot_ly(
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
fig_graduate <- fig_graduate %>% layout(
  title = "Major to Graduate Degree Field Transitions for Students Pursuing Graduate School",
  font = list(
    size = 10
  ),
  xaxis = list(showgrid = F, zeroline = F),
  yaxis = list(showgrid = F, zeroline = F)
)

# Add checkboxGroupInput for major selection
select_all_checkbox <- checkboxInput("select_all", "Select All", value = TRUE)
major_checkboxes <- checkboxGroupInput("major_checkboxes", "Select Majors:", choices = majors, selected = majors, inline = TRUE)

sankey_height <- reactive({
  n_selected <- length(input$major_checkboxes)
  return(n_selected * 300)
})

# Define UI for the application
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  tags$head(
    tags$script(src = "www/custom.js"),
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
                        goal is not to persuade Alumni to donate rather we’re trying to show the Alumni, a problem or concern student
                        have and provide the Alumni with information on how they can get that donated."),
        offset = 1
      )
    ),
    tabPanel(
      "Design Process",
      column(9,
        h1("Design Process"),
        hr(),
        h3("Initial Prompt"),
        p("Our design brief was <quote the design brief>. Our initial interpretation of this prompt was to aim to 
           convince potential donor alumni to donate to student organizations. Our initial plan to fulfill this goal 
           was to collect information about the multitude of student organizations at Grinnell. We generated a variety 
           of ideas for data we could collect that would fit different forms of visualization. One of these types of 
           data would have been qualitative data about the activities of student organizations. Another type would 
           have been statistics on budget, attendance, cost, community engagement, etc. A major issue with relying on 
           collecting data from student organizations was the potentially low response rate, and the fact that even a 
           student in a leadership position might not have robust records of previous years of organization activity, 
           especially if those statistics weren’t collected in the first place"),
        p("After meeting with our alumni mentor, Conner Stanfield, we refined our definition of what we would 
           consider innovative at Grinnell. Conner identified four ways in which he believed Grinnell is unique: 
           diversity, professor and undergraduate student research opportunities, alumni profiles, and 
           student-centered programming. For each of these categories he suggested we display relevant data using 
           either a data visualization or text."),
        
        h3("Structuring The Website"),
        p("One of the challenges of designing a website for this data was making sure a user could navigate it 
           easily. Ideally, a user would be able to quickly identify the series of steps needed in order to find the 
           information they were interested in. In order to make the structure of the website predictable, we planned 
           to separate each topic into subcategories. Professor research could be divided by department, student 
           organization data could be divided by area of involvement, etc. These subcategories could be navigated by 
           a sidebar with anchor links. Since this is a very common way of organizing a website, we believed it would 
           be familiar enough that users could easily navigate the site, as well as providing enough of a guide to 
           other categories of data that a user could explore unfamiliar topics. We decided to have all of the 
           sections on one page so a user could use ctrl + F to search without our implementing a search function. 
           An image carousel on the home page would serve to direct users to various pages."),
        
        h3("Reconsidering Student Data"),
        p("Given the quantity of students at Grinnell, we decided that it would not be feasible to elicit similarly 
           structured data from all student organizations. Therefore, we narrowed our focus for student opinion to ask
           about each of the areas identified by Conner as unique strengths at Grinnell. In order to determine what 
           questions we would ask students, we spoke with Susan Sanning, Director of Social Innovation at Grinnell. 
           From the conversation with Susan, we decided to shift our project’s tone from encouraging donations to 
           accurately reflecting student opinion. We put out a survey which identified the four areas of interest to 
           our topic and asked how each area had affected the student, either positively or negatively."),
        p("As a result of the new format for our textual data, we adjusted the structure of the student opinion page 
           to now show the four questions as subsections, with anonymized individual student responses below each 
           question."),
        
        h3("Reconsidering Research Data"),
        p("In order to address the unique research environment at Grinnell, we collected data from the Registrar 
           about rates of student participation in Mentored Advanced Project (MAP)/Mentored Introductory Project (MIP).
           We also collected data about the post-graduation outcomes of MAP/MIP students in order to answer how MAP/MIP
           participation might impact different majors.Our goal was to depict the flow of MAP/MIP students into their 
           respective fields. As such, we decided to use a Sankey diagram to illustrate the pathways taken by students
           in different majors."),
        p("A major issue with our chosen visualization was that the quantity of majors resulted in an overwhelming, 
           confusing diagram. The solution the design team generated while discussing data visualization with Vivero 
           fellows solved this by allowing the user to select/filter by major"),
        
        h3("Implementation"),
        p("Our project morphed and took on many different design forms throughout the initial stages of our project. 
           Initially, when we were under the original assumption that the goal of our project was to convince alumni 
           donors to donate to Student Organizations at the college, we envisioned the app to contain multiple data 
           visualizations on various data from Student Organizations across the college, with these visualizations 
           appearing on a large “Student Organizations” tab that was navegable via an extensive sidebar. Other tabs 
           would have included a “Wilson Center” tab explaining the purpose of the Wilson Center at Grinnell along 
           with corresponding data visualizations, and tabs on a few of the major on-campus events that speak to the 
           innovation at Grinnell, a “Pioneer Weekend” tab and a “Hack GC” tab. Both of these tabs would be in a 
           similar format to the others in that it would explain the corresponding event/resource, provide helpful 
           links and visualizations that help to convey the extent of innovation that it promotes at Grinnell."),
        p("After identifying the issues with collecting data from Student Organizations and our conversation with 
           Conner Stanfield we redesigned the structure of our app to align with the overall shift in mission of our 
           app. Since our goal was no longer to convince alumni to donate to Student Orgs (for which we were 
           displaying next to no data), we adopted an informative app layout, in lieu of a persuasive one. The tabs 
           became “Diversity”, “Professor and Undergraduate Student Research Opportunities”, “Alumni Profiles”, and 
           “Student-Centered Programming” and within each tab we sought to provide simple text that was both 
           informative and which conveyed the truth about Grinnell, with no need for flashy effects and 
           visualizations. This manifested in a simple card layout for the “Alumni Profiles” tab which simply briefly 
           described some of Grinnell’s notable alumni. We kept the idea of an image carousel on the homepage which 
           served to direct users to different tabs within the app."),
        p("Further development saw the change of the “Student-Centered Programming” tab to “Student Opinions” in an 
           attempt to use the few honest responses that we obtained from our survey on Student Organizations at the 
           college, and the “Professor and Undergraduate Student Research Opportunities” was simplified to a 
           “Mentored Advanced Projects (MAPS)” page as this is the most Grinnell-specific aspect of research that both
           highlights innovation at Grinnell and innovation specific to Grinnell. The former tab became the responses 
           of the Student Organizations to our survey questions, arranged by question and joined by anchor links (so 
           the user could navigate to the question that most interests them at the top of the page to view the student
           organizations’ responses) and the latter tab became a large interactive visualization that allows the user
           to filter and view where students who participate in MAPs at Grinnell end up post-Grinnell. The aim of this
           latter visualization is both informative and for alumni donors to see the amazing potential and effect a 
           MAP has on a student’s career, and donate for more MAP funding accordingly."),
        p("Finally, the image carousel changed to simply provide visual diversity of the different ways that 
           innovation appears at Grinnell. We decided not to do links because we did not have sufficient time to 
           provide relevant information for each of the many ways in which innovation appears at Grinnell, but we 
           wanted to at least showcase, visually, the many outlets of innovation that Grinnell College offers."),
        
        h3("Performance Assessment"),
        p("In order to assess our implementation, the design team reviewed the developers’ code and used the Reactlog 
          tool to view the reactivity network over the course of a variety of test cases. (Reactlog animation rendered 
          below?)"),
        offset = 1
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
    tabPanel(
      "Mentored Advanced Projects (MAP)",
      hr(),
      p("This is the mentored advanced projects (MAP) page."),
      checkboxGroupInput("major_checkboxes", "Select Major(s):",
        choices = c("Select All", majors),
        selected = "Select All",
        inline = TRUE
      ),
      # Add Sankey diagram in the MAP tab with flexible height
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
    selected_majors <- input$major_checkboxes
    
    filtered_links <- link_table %>%
      filter(major %in% selected_majors)
    
    if (nrow(filtered_links) > 0) {
      major_indices <- match(filtered_links$major, node_labels) - 1
      grad_degree_field_indices <- match(filtered_links$grad_degree_field, node_labels) - 1
      
      link_label <- paste(filtered_links$major, "->", filtered_links$grad_degree_field)
      
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
          value = filtered_links$value,
          label = link_label
        )
      ) %>%
        layout(
          title = "Major to Graduate Degree Field Transitions for Students Pursuing Graduate School",
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
  })
  

}

# Run the application
shinyApp(ui = ui, server = server)
