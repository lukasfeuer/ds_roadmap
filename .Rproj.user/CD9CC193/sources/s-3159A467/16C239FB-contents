# =============================================================================:
# Project Roadmap -------------------------------------------------------------
# =============================================================================:

# TODO
#   * Text boxes have to save input
#   * Changes in time line items have to be saved
#   * Add reactive display of individual project model period
#   * check for missing time stamps / include the timing systematic dependent on
#     initial dates of presentation start value

library(shiny)
library(tidyverse)
library(timevis)

proj <- read_csv2("data/project_list.csv",
                  na = "NA",
                  locale = locale(date_format = c("%d.%m.%y")))

timevisData <- proj %>% select(-c(kommentar, model_type))

timevisDataGroups <- proj %>%
    select(-c(model_type, kommentar)) %>%
    filter(type == "range") %>%
    transmute(id = group,
              content = content) %>%
    data.frame()

# maybe redundant 
proj_groups <- proj %>%
    distinct(group) %>%
    drop_na() %>%
    .[[1]]


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
    ### CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    ### Animated Background ----
    tagList(tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/particles.js@2.0.0/particles.min.js")
    ),
    includeScript("www/app.js")),
    div(id = 'particles-js'),
    
    # Application title
    fluidRow(titlePanel(
        h1("Projects | Data Science"), windowTitle = "Projects"
    )),
    
    fluidRow(
        column(width = 4,
               h3("Projects"),
               hr(),
               wellPanel(class = "project_container", uiOutput('dynamic')), ),
        column(width = 8,
               h3("Time Line"),
               hr(),
               wellPanel(class = "timeline_container", 
                         timevisOutput("timeline")))
    )
    
)


# Server -----------------------------------------------------------------------

server <- function(input, output) {
    
    output$timeline <- renderTimevis({
        config <- list(
            editable = TRUE,
            align = "center",
            orientation = "top",
            snap = NULL,
            margin = list(item = 30, axis = 50)
        )
        timevis(
            data = timevisData,
            groups = timevisDataGroups,
            options = config,
            zoomFactor = 5
        )
    })
    
    # Generates code for individual info panel
    make_panel <- function(x, group, i) {
        x <- x %>%
            filter(group == UQ(group) &
                       !is.na(model_type)) %>%
            select(content, start, end, kommentar, model_type) %>%
            mutate(across(everything(), ~ as.character(.))) %>%
            pivot_longer(cols = everything(),
                         names_to = "var",
                         values_to = "value")
        
        wellPanel(
            class = "info_box",
            h4(x$value[x$var == "content"]),
            tags$b("Model Type:"),
            tags$i(x$value[x$var == "model_type"]), br(),
            tags$b("Model Period:"),
            tags$i("\nfrom", x$value[x$var == "start"],
                   "\nto", x$value[x$var == "start"]),
            br(),
            textAreaInput(
                inputId = "kommentar_input",
                label = "Comments:",
                value = x$value[x$var == "kommentar"]
            )
        )
    }
    
    output$dynamic <- renderUI({
        tags <- tagList()
        
        for (i in 1:length(proj_groups)) {
            tags[[i]] <- make_panel(proj, proj_groups[i], i)
        }
        
        tags
    })
}

shinyApp(ui = ui, server = server)
