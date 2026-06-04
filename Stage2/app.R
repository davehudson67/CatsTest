library(tidyverse)
library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)
library(readxl)
library(jose)
library(leaflet)
library(sf)
library(shinyBS)
library(jsonlite)
library(base64enc)
library(ShinyEditor)
library(rmarkdown)


# Function to load HTML content from file
loadHtmlFromFile <- function(filePath) {
  if (file.exists(filePath)) {
    paste(readLines(filePath, warn = FALSE), collapse = "\n")
  } else {
    "Error - No HTML file found."
  }
}

################################################################################

# Define UI for application
ui <- fluidPage(
  
  ## TinyMCE API key
  # use_editor("o0d8936xfj7k3qtrhu0hqwh7rh3zyh7ojb3n05in82v0jk4t"),
  use_editor("ipncvq0esvwgc4t8d3aru1frl1a1jz340g4y1kz7av0d69pr"),
  
  ## Will need to add domain here https://www.tiny.cloud/my-account/domains/ ##
  
  ## Admin toggle buttons
  hidden(actionButton("toggleEdit", "Admin user: Toggle Edit Options", style = "margin: 10px;", class = "neutDownload-button")),
  hidden(actionButton("viewGADashboard", "Admin user: View GA Dashboard", style = "margin: 10px;", class = "neutDownload-button",
                 onclick = "window.open('https://your-google-analytics-dashboard-link', '_blank')")),
  
  ## Custom CSS file
  tags$head(
    # Link to the CSS file for styling
    tags$link(rel = "stylesheet", type = "text/css", href = "styles_fluid3.css"),
    
    # Load Google Tag Manager (GTM)
    tags$script(HTML("
    (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
    new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
    j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
    'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
    })(window,document,'script','dataLayer','GTM-P8XHRQCD');
  ")),
    
    # JavaScript for tracking tab views and button clicks
    tags$script(HTML("
    let currentTab = null; // Track the current tab
    let tabStartTime = null; // Start time for the current tab

    // Function to log tab view event
    function logTabView(tabName, duration) {
      window.dataLayer.push({
        event: 'tab_view',
        tab_name: tabName, // Name of the tab
        time_spent: duration // Time spent on the tab in seconds
      });
    }

    // Listen for tab changes
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'navbar') { // Detect navbar input change
        let currentTime = new Date(); // Get current time

        if (currentTab && tabStartTime) {
          // Calculate time spent on the current tab
          let timeSpent = (currentTime - tabStartTime) / 1000; // Time in seconds
          logTabView(currentTab, timeSpent); // Log the current tab
        }

        // Update current tab and start time
        currentTab = event.value; // Set new tab name
        tabStartTime = currentTime; // Reset start time for new tab
      }
    });

    // Track all button clicks
    $(document).on('click', 'button', function(event) {
      let buttonId = $(this).attr('id'); // Get button ID
      if (buttonId) {
        window.dataLayer.push({
          event: 'button_click',
          button_id: buttonId // Log button ID
        });
      }
    });
  ")),
    # JavaScript for handling user department info sent from the server
    tags$script(HTML("
    Shiny.addCustomMessageHandler('userDepartment', function(message) {
      window.dataLayer = window.dataLayer || [];
      
      // Push user info to Google Tag Manager
      window.dataLayer.push({
        event: 'user_info',
        user_name: message.user_name,
        user_department: message.user_department
      });
    });
  "))
  ),
  
  ## CP logo at the top
  tags$div(
    style = "text-align: center; padding: 20px;",
    img(src = "CP_Logo.png", height = "100px")
  ),
  
  ##############################################################################
  
  ## Navbar structure
  navbarPage(
    title = NULL,
    selected = "Home",
    id = "navbar",
    
    ## Home Panel
    tabPanel(
      "Home", 
      div(class = "page-content-box", 
          
          # Welcome text UI
          uiOutput("welcomeText"),
          
          h2("Welcome to the Domestic Cat Population Model", class = "cats-h2"),
          
          # Editable text block
          div(class = "static-text-container", 
              uiOutput("home_text")),

          # Editable buttons for external links
          div(class = "button-container",
              uiOutput("home_buttons")),
      
      # Conditional editor for admin users
      conditionalPanel(
        condition = "output.showEditor == true",
        
        # Text editor using TinyMCE
        h3("Edit Home Page Content"),
        editor("editableHomeText"),
        actionButton("saveHomeText", "Save and update Home Page Content", 
                     style = "margin: 10px; padding: 10px;"),
        
        # Editable buttons section
        h3("Configure Home Buttons"),
        checkboxInput("showHomeButton1", "Show Button 1", value = TRUE),
        textInput("labelHomeButton1", "Label for Button 1", value = "Research Pages"),
        textInput("urlHomeButton1", "URL for Button 1", value = "https://www.cats.org.uk/"),
        
        checkboxInput("showHomeButton2", "Show Button 2", value = TRUE),
        textInput("labelHomeButton2", "Label for Button 2", value = "Read the Cats Report"),
        textInput("urlHomeButton2", "URL for Button 2", value = "https://www.cats.org.uk/about-cp/cats-report"),
        
        checkboxInput("showHomeButton3", "Show Button 3", value = TRUE),
        textInput("labelHomeButton3", "Label for Button 3", value = "User Interface Guide"),
        textInput("urlHomeButton3", "URL for Button 3", value = "https://www.cats.org.uk/"),
        
        checkboxInput("showHomeButton4", "Show Button 4", value = TRUE),
        textInput("labelHomeButton4", "Label for Button 4", value = "User Training Materials"),
        textInput("urlHomeButton4", "URL for Button 4", value = "https://www.cats.org.uk/")
      )
    ),
    
    # Exeter Uni developer info
    div(class = "page-content-box", 
        tags$img(src = "ExeLogo.png", height = "80px", width = "auto"),
        p("App created and developed by Dr D. Hudson, Centre for Ecology and Conservation, University of Exeter.", class = "cats-reg")
    )),
    
    ## Data Information Panel
    tabPanel(
      "Data Information", 
      div(class = "page-content-box", 
          h2("Data Information", class = "cats-h2"),
          
          fluidRow(
            column(8, # Left hand side
                   div(class = "static-text-container", 
                       # Top editable text block
                       uiOutput("text_top")
                   ),
                   
                   div(class = "padded-table", 
                       # Add CSS styles to make the table scrollable
                       div(style = "height: 400px; overflow-y: scroll; border: 1px solid #ddd; padding: 10px;",
                           tableOutput("dataInfoTable")
                       )
                   ),
                   
                   div(class = "static-text-container", 
                       # Bottom editable text block
                       uiOutput("text_bottom")
                   ),
                   
                   # Conditional text editor and file upload for admin users
                   conditionalPanel(
                     condition = "output.showEditor == true",
                     
                     # Editor for the top text block
                     h3("Update Top Text"),
                     editor("editableText_top"),
                     actionButton("saveText_top", "Save and update Top Text", 
                                  style = "margin: 10px; padding: 10px;"),
                     
                     # Editor for the bottom text block
                     h3("Update Bottom Text"),
                     editor("editableText_bottom"),
                     actionButton("saveText_bottom", "Save and update Bottom Text", 
                                  style = "margin: 10px; padding: 10px;"),
                     
                     # File upload input
                     h3("Update Table"),
                     fileInput("dataFile", "Upload new .xlsx data File", 
                               accept = c(".xlsx"),
                               buttonLabel = "Browse...", placeholder = "No file selected")
                   ),
                   
                   # New data badge
                   conditionalPanel(
                     condition = "output.showEditor == true",
                     checkboxInput("showBadge", "Show 'New Data Added' Badge", value = FALSE)
                   )
            ),
            
            column(4, # Right hand side
                   # Render the buttons dynamically
                   div(class = "button-column",
                       uiOutput("dynamicButtons")
                   ),
                   
                   # Add inputs for admin to configure buttons
                   conditionalPanel(
                     condition = "output.showEditor == true",
                     h3("Configure Buttons"),
                     checkboxInput("showButton1", "Show Button 1", value = TRUE),
                     textInput("labelButton1", "Label for Button 1", value = "Research Link 1"),
                     textInput("urlButton1", "URL for Button 1", value = "https://www.cats.org.uk/research"),
                     
                     checkboxInput("showButton2", "Show Button 2", value = TRUE),
                     textInput("labelButton2", "Label for Button 2", value = "Research Link 2"),
                     textInput("urlButton2", "URL for Button 2", value = "https://www.cats.org.uk/about"),
                     
                     checkboxInput("showButton3", "Show Button 3", value = TRUE),
                     textInput("labelButton3", "Label for Button 3", value = "Research Link 3"),
                     textInput("urlButton3", "URL for Button 3", value = "https://www.cats.org.uk/guides"),
                     
                     checkboxInput("showButton4", "Show Button 4", value = TRUE),
                     textInput("labelButton4", "Label for Button 4", value = "Research Link 4"),
                     textInput("urlButton4", "URL for Button 4", value = "https://www.cats.org.uk/training")
                   ),
                   
                   # Download references button
                   tags$a(href = "References.pdf", download = NA, class = "neutLink-button", "Download Reference Data"),
                   
                   # Add the New Data badge element (initially hidden)
                   conditionalPanel(
                     condition = "input.showBadge == true",
                     div(
                       class = "badge-container", 
                       style = "text-align: center; margin-top: 20px;",
                       tags$img(src = "NewDataLogo.png", alt = "New Data Added", height = 200, width = 200)
                     )
                   )
            )
          )
      )
    ,
    
    # Exeter Uni developer info
    div(class = "page-content-box", 
        tags$img(src = "ExeLogo.png", height = "80px", width = "auto"),
        p("App created and developed by Dr D. Hudson, Centre for Ecology and Conservation, University of Exeter.", class = "cats-reg")
    )),
    
    ## Neutering scenario tab
    tabPanel("Neutering Scenarios", 
             div(class = "page-content-box",
                 fluidRow(
                   useShinyjs(),
                   
                   # First box with column width 4
                   column(4,
                              h2("Neutering Scenarios", class = "cats-h2"),
                              div(class = "static-text-container", 
                                  uiOutput("text_top_neutering")
                                  ),
                          
                              # Input elements
                              sliderInput("timeframe_pred", "Timeframe of prediction parameters (yrs):", min = 1, max = 10, value = 10, step = 1, width = "100%", animate = TRUE),
                              
                              bsTooltip("timeframe_pred", "Adjust the number of years for prediction.", 
                                    placement = "top", trigger = "hover"),
                          
                              radioButtons("owned_neut_rate_A", "Owned Adult Neutering Prevalence:", choices = c("90%", "95%", "98%"), selected = "95%", inline = TRUE),
                              
                              bsTooltip("owned_neut_rate_A", "Adult owned cat neutering prevalence, which approximate low (83%), medium (88%) and high (92%) overall owned-cat neutering prevalence.", 
                                        placement = "top", trigger = "hover"),                              
                              
                              sliderInput("owned_neut_rate_K", "Owned Kitten Neutering Rate <6months (%):", min = 0, max = 50, value = 30, step = 5, width = "100%", animate = TRUE),
                            
                              bsTooltip("owned_neut_rate_K", "% of owned kittens neutered before 6 months ", 
                                    placement = "top", trigger = "hover"),
                              
                              actionButton("setUp", "Setup", class = "neut-button"),
                              hidden(actionButton("goButton", "Go", class = "neut-button", onclick = "gtag('event', 'Button Click', {'event_category': 'User Actions', 'event_label': 'Go Button'});")),
                              hidden(actionButton("resetButton", "Reset", class = "neut-button")),
                              hidden(actionButton("compareButton", "Compare with previous", class = "neut-button")),
                              hidden(actionButton("downloadButton", "Download", class = "neut-button")),
                              uiOutput("conditionalTextBox"),
                              hidden(actionButton("exportRuns", "Export Run(s) and Plot(s)", class = "neutDownload-button")),
                              downloadButton("downloadRuns", "Download Exported Runs", class = "neutDownload-button", style = "display: none;"),
                              
                              div(id = "tooltip_total",
                                class = "total-plot-box",
                                  h2("Total Population Projection", class = "cats-h2"),
                                  withSpinner(plotOutput("TotalPlot"))
                              ),
                          
                             div(
                                tags$a(
                                  href = "https://cat-kind.org.uk/",
                                  target = "_blank",
                                  class = "neutLink-button",
                                  "Visit the Cat-Kind website"
                                )
                              ),
                              # Conditional text editor
                              conditionalPanel(
                                condition = "output.showEditor == true",
                                h3("Edit Top Text"),
                                editor("editableText_neuteringTop"),
                                actionButton("saveText_neuteringTop", "Save and Update Text", 
                                             style = "margin: 10px; padding: 10px;")
                              )
                   ),
                   
                   # Second box with column width 8
                   column(8,
                          #div(class = "custom-box-content", # Box 2 with custom class for styling
                              h3(id = "populationCountsHeader", "Population Counts", class = "cats-h3"),
                          
                              bsTooltip(
                                id = "populationCountsHeader",  # Match the ID of the element
                                title = "The population count gives an estimate of the number of cats within each subpopulation at the end of simulation based on the intervention selected, and assumes starting populations of 92 222 owned cats, 232 shelter, 2340 stray and 5206 feral totalling 100000.", 
                                placement = "left", 
                                trigger = "hover"
                              ),
                          
                              fluidRow(
                                column(3, div(class = "custom-value-box-owned", 
                                              h4("Owned", class = "cats-h4"), 
                                              textOutput("BxOwned"))),
                                column(3, div(class = "custom-value-box-shelter", 
                                              h4("Shelter", class = "cats-h4"), 
                                              textOutput("BxShelter"))),
                                column(3, div(class = "custom-value-box-stray", 
                                              h4("Stray", class = "cats-h4"), 
                                              textOutput("BxStray"))),
                                column(3, div(class = "custom-value-box-feral", 
                                              h4("Feral", class = "cats-h4"), 
                                              textOutput("BxFeral")))
                              ),
                              
                              h3(id = "populationGrowthHeader", "Population Growth Rates", class = "cats-h3"),
                          
                              bsTooltip(
                                id = "populationGrowthHeader",  # Match the ID of the element
                                title = "The population growth rate indicates how the population has changed at the end of the simulation when compared to the starting population. If the growth rate is less than 1, the population has declined, if it is greater than 1, it has increased overall. Each subpopulation is likely to be impacted by neutering interventions differently, and thus will have its own growth rate.", 
                                placement = "left", 
                                trigger = "hover"
                              ),
                          
                              fluidRow(
                                column(3, div(class = "custom-value-box-owned", 
                                              h4("Owned", class = "cats-h4"), 
                                              textOutput("BxOwnedG"))),
                                column(3, div(class = "custom-value-box-shelter", 
                                              h4("Shelter", class = "cats-h4"), 
                                              textOutput("BxShelterG"))),
                                column(3, div(class = "custom-value-box-stray", 
                                              h4("Stray", class = "cats-h4"), 
                                              textOutput("BxStrayG"))),
                                column(3, div(class = "custom-value-box-feral", 
                                              h4("Feral", class = "cats-h4"), 
                                              textOutput("BxFeralG")))
                              ),
                              
                              h3(id = "populationplots", "Population Plots", class = "cats-h3"),
                          
                          bsTooltip(
                            id = "populationplots",  
                            title = "These graphs represent the changing population numbers throughout the duration of the simulation, at monthly time intervals. Each subpopulation will experience different population dynamics as a result of the modelled intervention, and therefore has its own graph.", 
                            placement = "left", 
                            trigger = "hover"
                          ),
                          
                          fluidRow(
                            column(
                              width = 6,
                              div(id = "tooltip_owned", 
                                  withSpinner(plotOutput("OwnedPlot"))
                              )
                            ),
                            column(
                              width = 6,
                              div(id = "tooltip_shelter", 
                                  withSpinner(plotOutput("ShelterPlot"))
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 6,
                              div(id = "tooltip_stray", 
                                  withSpinner(plotOutput("StrayPlot"))
                              )
                            ),
                            column(
                              width = 6,
                              div(id = "tooltip_feral", 
                                  withSpinner(plotOutput("FeralPlot"))
                              )
                            )
                          ),
                          #)
                   )
                 )
             ),
             
             # Exeter Uni developer info
             div(class = "page-content-box", 
                 tags$img(src = "ExeLogo.png", height = "80px", width = "auto"),
                 p("App created and developed by Dr D. Hudson, Centre for Ecology and Conservation, University of Exeter.", class = "cats-reg")
             )
    ),
    
    
    tabPanel(
      "UK Cat Distribution",
      div(
        class = "page-content-box",
        
        fluidRow(
          useShinyjs(),
          
          # Left column for map information and file upload
          column(4,
                 h2("UK Cat Population", class = "cats-h2"),
                 
                 # Editable text at the top
                 div(class = "static-text-container", 
                     uiOutput("text_top_map")
                 ),
                 
                 # Display clicked area info
                 div(class = "custom-box-content",
                     h4("Selected County Details"),
                     verbatimTextOutput("clickedAreaInfo")
                 ),
                 
                 # File upload for shapefile (zip format)
                 conditionalPanel(
                   condition = "output.showEditor == true",
                   h3("Upload New Shapefile"),
                   fileInput("shapefileUpload", "Upload Shapefile (ZIP):", 
                             accept = c(".zip"),
                             buttonLabel = "Browse...", 
                             placeholder = "No file selected"),
                   actionButton("loadShapefile", "Update Shapefile", class = "btn-primary", style = "margin-top: 10px;")
                 )
          ),
          
          # Right column for the map
          column(8,
                 div(
                   class = "map-container",
                   withSpinner(leafletOutput("map", height = "600px"), type = 6)
                 )
          )
        ),
        
        # Conditional text editor
        conditionalPanel(
          condition = "output.showEditor == true",
          h3("Edit Top Text"),
          editor("editableText_mapTop"),
          actionButton("saveText_mapTop", "Save and Update Text", 
                       style = "margin: 10px; padding: 10px;")
        )
      ),
      
      # Exeter Uni developer info
      div(class = "page-content-box", 
          tags$img(src = "ExeLogo.png", height = "80px", width = "auto"),
          p("App created and developed by Dr D. Hudson, Centre for Ecology and Conservation, University of Exeter.", class = "cats-reg")
      ))
    ),
  
  # Feedback button
  tags$div(class = "feedback-button",
           tags$a(href = "https://forms.office.com/Pages/ResponsePage.aspx?id=cKIDhhadt0KoTrkbbW6b3nK8EJbAta1Am6RbuoxN64VUNjY3SEZSQVZZWDlaM1FYOEwwVzVSNjYwTC4u",
                  target = "_blank",  # Open in a new tab
                  "Take 2 minutes to tell us about your experience today")
  ),
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize shinyjs
  useShinyjs()

  # Reactive value to store edit permission (initially FALSE)
  EditPermission <- reactiveVal(FALSE)

  # Toggle EditPermission when button is clicked
  observeEvent(input$toggleEdit, {
    EditPermission(!EditPermission())  # Toggle between TRUE and FALSE
  })

  ## JWT Authentication

  # Extract and decode SSO token
{  ## JWT Authentication
  
  # Extract and decode SSO token
  sso_principal <- session$request$X_MS_CLIENT_PRINCIPLE
  
  # Default values
  user_name <- "Guest"
  user_department <- "Unknown"
  
  # For testing, set a mock SSO token if not running in production
  if (is.null(sso_principal)) {
    # Use the provided Base64-encoded token for testing
    sso_principal <- "eyJhdXRoX3R5cCI6ImFhZCIsImNsYWltcyI6W3sidHlwIjoiYXVkIiwidmFsIjoiN2EyNTY3ZDktY2FjMS00YTcxLWE2ZTQtYzY3ZjUwNDVhNDJiIn0seyJ0eXAiOiJpc3MiLCJ2YWwiOiJodHRwczpcL1wvbG9naW4ubWljcm9zb2Z0b25saW5lLmNvbVwvODYwM2EyNzAtOWQxNi00MmI3LWE4NGUtYjkxYjZkNmU5YmRlXC92Mi4wIn0seyJ0eXAiOiJpYXQiLCJ2YWwiOiIxNzMzMTMwNDY3In0seyJ0eXAiOiJuYmYiLCJ2YWwiOiIxNzMzMTMwNDY3In0seyJ0eXAiOiJleHAiLCJ2YWwiOiIxNzMzMTM0MzY3In0seyJ0eXAiOiJhaW8iLCJ2YWwiOiJBV1FBbVwvOFlBQUFBUEdOMVdlWGhpOUtVWUdYY2Z3bndNekZqdEQ5WE1aN3lOUldjcUIyS09PRzNGUmJOb2dBM3hhQkRTMDdReGpTYWNOR1pCeHpXeG9FZ3pNTzFhU2gwaU5EaU1GRWtnV1grMlc0dnZTRmhYUGFXdUlZWE53RWpCYzJUOEp4WXlXNWwifSx7InR5cCI6ImNfaGFzaCIsInZhbCI6IkI1X2lEc0ViZ1ZCVkdGcmhhdGdQWVEifSx7InR5cCI6Imh0dHA6XC9cL3NjaGVtYXMueG1sc29hcC5vcmdcL3dzXC8yMDA1XC8wNVwvaWRlbnRpdHlcL2NsYWltc1wvZW1haWxhZGRyZXNzIiwidmFsIjoiUGF1bC5SaWxleUBjYXRzLm9yZy51ayJ9LHsidHlwIjoibmFtZSIsInZhbCI6IlBhdWwgUmlsZXkifSx7InR5cCI6Im5vbmNlIiwidmFsIjoiNDAyZjc0ZDIzMzdkNDU3Njk1YmJkYWVjNTcxZTRiZjVfMjAyNDEyMDIwOTE3MzkifSx7InR5cCI6Imh0dHA6XC9cL3NjaGVtYXMubWljcm9zb2Z0LmNvbVwvaWRlbnRpdHlcL2NsYWltc1wvb2JqZWN0aWRlbnRpZmllciIsInZhbCI6ImRmZmVkMmUzLWYzZDctNGQ1Yy1hNDU2LTc3NzNhODUyNDRjNiJ9LHsidHlwIjoicHJlZmVycmVkX3VzZXJuYW1lIiwidmFsIjoiUGF1bC5SaWxleUBjYXRzLm9yZy51ayJ9LHsidHlwIjoicmgiLCJ2YWwiOiIxLkFRc0FjS0lEaGhhZHQwS29UcmtiYlc2YjN0bG5KWHJCeW5GS3B1VEdmMUJGcEN1RUFOVUxBQS4ifSx7InR5cCI6InJvbGVzIiwidmFsIjoiU2l0ZTpBZG1pbiJ9LHsidHlwIjoicm9sZXMiLCJ2YWwiOiJTaXRlOlVzZXIifSx7InR5cCI6Imh0dHA6XC9cL3NjaGVtYXMueG1sc29hcC5vcmdcL3dzXC8yMDA1XC8wNVwvaWRlbnRpdHlcL2NsYWltc1wvbmFtZWlkZW50aWZpZXIiLCJ2YWwiOiJUYlVUN3ZzVlBpaThwTlc0ZjdzZFhMeUsxU1B2bUhTdW54Q3pCUlBHNjdvIn0seyJ0eXAiOiJodHRwOlwvXC9zY2hlbWFzLm1pY3Jvc29mdC5jb21cL2lkZW50aXR5XC9jbGFpbXNcL3RlbmFudGlkIiwidmFsIjoiODYwM2EyNzAtOWQxNi00MmI3LWE4NGUtYjkxYjZkNmU5YmRlIn0seyJ0eXAiOiJ1dGkiLCJ2YWwiOiJDc25jMGVKaW5FV1hkQzNqaEZYMUFBIn0seyJ0eXAiOiJ2ZXIiLCJ2YWwiOiIyLjAifV0sIm5hbWVfdHlwIjoiaHR0cDpcL1wvc2NoZW1hcy54bWxzb2FwLm9yZ1wvd3NcLzIwMDVcLzA1XC9pZGVudGl0eVwvY2xhaW1zXC9lbWFpbGFkZHJlc3MiLCJyb2xlX3R5cCI6Imh0dHA6XC9cL3NjaGVtYXMubWljcm9zb2Z0LmNvbVwvd3NcLzIwMDhcLzA2XC9pZGVudGl0eVwvY2xhaW1zXC9yb2xlIn0="
  }
  
  if (!is.null(sso_principal)) {
    decoded_token <- rawToChar(base64decode(sso_principal))
    token_data <- fromJSON(decoded_token)
    
    # Extract user name
    name_row <- token_data$claims[token_data$claims$typ == "name", ]
    if (nrow(name_row) > 0) {
      user_name <- name_row$val
    } else {
      user_name <- "Guest"
    }
    
    # Extract user roles (department)
    roles_rows <- token_data$claims[token_data$claims$typ == "roles", ]
    if (nrow(roles_rows) > 0) {
      user_department <- paste(roles_rows$val, collapse = ", ") # Combine roles if multiple exist
    } else {
      user_department <- "Unknown"
    }
  }
  
  # Control visibility of "toggleEdit" button based on department
  observe({
    if (grepl("Admin", user_department)) {
      shinyjs::show("toggleEdit")  # Show the button if user_department contains "Admin"
      shinyjs::show("viewGADashboard")
    } else {
      shinyjs::hide("toggleEdit")  # Hide the button otherwise
      shinyjs::hide("viewGADashboard")
    }
  })
  
  # UI feedback: Welcome text
  output$welcomeText <- renderUI({
    tagList(
      h3(paste("Hi,", user_name), class = "cats-h3"),
      p(paste("Your roles:", user_department), class = "cats_reg")
    )
  })
  
  # Conditionally show the editor UI based on EditPermission
  output$showEditor <- reactive({
    EditPermission()
  })
  outputOptions(output, "showEditor", suspendWhenHidden = FALSE)
  
  # Send the user's department to the client-side for GTM
  session$sendCustomMessage("userDepartment", list(
    user_name = user_name,
    user_department = user_department
  ))

  } ## JWT Authentication
  
{ # Track tab views
    observeEvent(input$navbar, {
      session$sendCustomMessage(
        "trackEvent", 
        list(
          category = "Tab",
          action = "View",
          label = input$navbar
        )
      )
    })
  } ## GA tracking
  
{
  # File path to save the large editable text
  homeHtmlPath <- "data/HomeText.html"
  
  # Reactive value for the large text
  homeSavedHtml <- reactiveVal(loadHtmlFromFile(homeHtmlPath))
  
  # Render the large text block in the UI
  output$home_text <- renderUI({
    HTML(homeSavedHtml())
  })
  
  # Render dynamic buttons
  output$home_buttons <- renderUI({
    buttons <- tagList()
    if (input$showHomeButton1) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlHomeButton1, target = "_blank", class = "custom-button", input$labelHomeButton1))
    }
    if (input$showHomeButton2) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlHomeButton2, target = "_blank", class = "custom-button", input$labelHomeButton2))
    }
    if (input$showHomeButton3) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlHomeButton3, target = "_blank", class = "custom-button", input$labelHomeButton3))
    }
    if (input$showHomeButton4) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlHomeButton4, target = "_blank", class = "custom-button", input$labelHomeButton4))
    }
    div(class = "button-container", buttons)
  })
  
  # Save the large text content
  observeEvent(input$saveHomeText, {
    if (EditPermission()) {
      editorText(session, editorid = "editableHomeText", outputid = "savedHomeText")
      invalidateLater(200, session)
      observe({
        req(input$savedHomeText)
        homeSavedHtml(input$savedHomeText)
        writeLines(input$savedHomeText, homeHtmlPath)
      })
    }
  })
  
  } ## Home tab  
  
{
  
  # Display or Hide Editor Based on Permission
  output$showEditor <- reactive({
    EditPermission()
  })
  
  outputOptions(output, "showEditor", suspendWhenHidden = FALSE)
  
  # Paths to save HTML content
  htmlFilePath_top <- "data/DataInformation_text_top.html"
  htmlFilePath_bottom <- "data/DataInformation_text_bottom.html"
  
  # Reactive values to hold the saved HTML content for both text blocks
  savedHtml <- reactiveValues(
    text_top = loadHtmlFromFile(htmlFilePath_top),
    text_bottom = loadHtmlFromFile(htmlFilePath_bottom)
  )
  
  # Render HTML content for display
  output$text_top <- renderUI({
    HTML(savedHtml$text_top)  # Render top HTML content
  })
  
  output$text_bottom <- renderUI({
    HTML(savedHtml$text_bottom)  # Render bottom HTML content
  })
  
  # Save TinyMCE HTML Content for top text
  observeEvent(input$saveText_top, {
    if (EditPermission()) {
      # Capture HTML from TinyMCE editor for top text
      editorText(session, editorid = "editableText_top", outputid = "savedText_top")
      
      # Delay to ensure `input$savedText_top` is updated
      invalidateLater(200, session)  # Wait 200 milliseconds
      
      # Save content if updated
      observe({
        req(input$savedText_top)  # Ensure it's not null
        saved_text_top <- as.character(input$savedText_top)
        
        if (!is.null(saved_text_top) && saved_text_top != "") {
          savedHtml$text_top <- saved_text_top  # Update reactive value for display
          writeLines(saved_text_top, htmlFilePath_top)  # Save to file
        }
      })
    }
  })
  
  # Save TinyMCE HTML Content for bottom text
  observeEvent(input$saveText_bottom, {
    if (EditPermission()) {
      # Capture HTML from TinyMCE editor for bottom text
      editorText(session, editorid = "editableText_bottom", outputid = "savedText_bottom")
      
      # Delay to ensure `input$savedText_bottom` is updated
      invalidateLater(200, session)  # Wait 200 milliseconds
      
      # Save content if updated
      observe({
        req(input$savedText_bottom)  # Ensure it's not null
        saved_text_bottom <- as.character(input$savedText_bottom)
        
        if (!is.null(saved_text_bottom) && saved_text_bottom != "") {
          savedHtml$text_bottom <- saved_text_bottom  # Update reactive value for display
          writeLines(saved_text_bottom, htmlFilePath_bottom)  # Save to file
        }
      })
    }
  })
  
  
  ## Table content #############################################################
  
  library(readxl)
  
  # Helper function to sanitize data by removing non-ASCII characters
  sanitize_data <- function(data) {
    data[] <- lapply(data, function(x) {
      if (is.character(x)) {
        iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")  # Remove problematic characters
      } else {
        x
      }
    })
    return(data)
  }
  
  # Reactive function to load and sanitize the data information from the Excel file
  dataInfo <- reactive({
    tryCatch({
      # Attempt to read and sanitize the Excel file
      temp_data <- read_excel("data/DataInfoTable.xlsx")
      temp_data <- as.data.frame(temp_data)  # Convert to a data frame
      sanitize_data(temp_data)
    }, error = function(e) {
      # Log the error and return a placeholder dataframe
      message("Error reading DataInfoTable.xlsx: ", e$message)
      showNotification("Error: Could not read data file. Please check the Excel format.", type = "error")
      data.frame(Error = "Invalid Excel format")  # Placeholder for display
    })
  })
  
  # Render the table in the UI
  output$dataInfoTable <- renderTable({
    tryCatch({
      dataInfo()  # Use the sanitized reactive data
    }, error = function(e) {
      showNotification("Error: Unable to render the table.", type = "error")
      data.frame(Error = "Error rendering table")  # Placeholder for UI
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Observe file upload to replace the data file
  observeEvent(input$dataFile, {
    req(EditPermission())  # Ensure the user has permission to edit
    req(input$dataFile)    # Ensure a file is uploaded
    
    tryCatch({
      # Attempt to read the uploaded Excel file and validate its format
      temp_data <- read_excel(input$dataFile$datapath)
      temp_data <- as.data.frame(temp_data)  # Convert to a data frame
      temp_data <- sanitize_data(temp_data)  # Sanitize the uploaded data
      
      # Save the file and replace the existing Excel file
      file.copy(input$dataFile$datapath, "data/DataInfoTable.xlsx", overwrite = TRUE)
      showNotification("File uploaded successfully!", type = "message")
      
      # Refresh the table with the new data
      output$dataInfoTable <- renderTable({
        temp_data  # Use sanitized data for display
      }, striped = TRUE, hover = TRUE, bordered = TRUE)
    }, error = function(e) {
      showNotification("Error: Uploaded file has invalid characters or format issues.", type = "error")
    })
  })
  
  
  ## Links editing #############################################################
  
  # Render the buttons dynamically based on admin input
  output$dynamicButtons <- renderUI({
    # Create a list of buttons based on admin inputs
    buttons <- tagList()  # Use `tagList` directly
    
    if (input$showButton1) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlButton1, target = "_blank", class = "stacked-button", input$labelButton1))
    }
    if (input$showButton2) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlButton2, target = "_blank", class = "stacked-button", input$labelButton2))
    }
    if (input$showButton3) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlButton3, target = "_blank", class = "stacked-button", input$labelButton3))
    }
    if (input$showButton4) {
      buttons <- tagAppendChild(buttons, tags$a(href = input$urlButton4, target = "_blank", class = "stacked-button", input$labelButton4))
    }
    
    # Wrap all buttons in a single div with the desired class
    div(class = "button-column", buttons)
  })
} ## Data Information Tab
  
{ 
  ##############################################
  ##############################################################################
  
  ##  Neutering Scenarios Tab
  
  ##############################################################################
  ##############################################################################
  
  # File path for neutering scenarios top text
  neuteringTextPath <- "data/NeuteringText.html"
  
  observe({
    if (input$owned_neut_rate_K < 5) {
      updateSliderInput(session, "owned_neut_rate_K", value = 5)
    }
  })
  
  observe({
    if (input$timeframe_pred < 2) {
      updateSliderInput(session, "timeframe_pred", value = 2)
    }
  })
  
  observe({
    req(rv$current_run)  # Ensure rv$current_run exists
    
    # Extract number of years and format to 2 decimal places
    sim_years <- sprintf("%.2f", rv$lengthoftime / 12)  
    
    # Create tooltips with dynamic data, rounding to 2 decimal places
    addTooltip(session, "tooltip_owned",
               title = paste("Owned Cats Population. Simulation ran for", sim_years, 
                             "years. Median Population:", sprintf("%.2f", median(rv$current_run$Owned))),
               placement = "top", trigger = "hover")
    
    addTooltip(session, "tooltip_shelter",
               title = paste("Shelter Cats Population. Simulation ran for", sim_years, 
                             "years. Median Population:", sprintf("%.2f", median(rv$current_run$Shelter))),
               placement = "top", trigger = "hover")
    
    addTooltip(session, "tooltip_stray",
               title = paste("Stray Cats Population. Simulation ran for", sim_years, 
                             "years. Median Population:", sprintf("%.2f", median(rv$current_run$Stray))),
               placement = "top", trigger = "hover")
    
    addTooltip(session, "tooltip_feral",
               title = paste("Feral Cats Population. Simulation ran for", sim_years, 
                             "years. Median Population:", sprintf("%.2f", median(rv$current_run$Feral))),
               placement = "top", trigger = "hover")
    
    addTooltip(session, "tooltip_total",
               title = paste("Total Cats Population. Simulation ran for", sim_years, 
                             "years. Median Population:", sprintf("%.2f", median(rv$current_run$Total))),
               placement = "top", trigger = "hover")
  })
  
  
  
  # Load the initial HTML content for the top text
  loadHtmlFromFile <- function(filePath) {
    if (file.exists(filePath)) {
      paste(readLines(filePath, warn = FALSE), collapse = "\n")
    } else {
      "Default text for neutering scenarios."
    }
  }
  
  # Reactive value for editable top text
  neuteringSavedHtml <- reactiveVal(loadHtmlFromFile(neuteringTextPath))
  
  # Render top text in the UI
  output$text_top_neutering <- renderUI({
    HTML(neuteringSavedHtml())
  })
  
  # Save the edited top text
  observeEvent(input$saveText_neuteringTop, {
    editorText(session, editorid = "editableText_neuteringTop", outputid = "savedText_neuteringTop")
    observe({
      req(input$savedText_neuteringTop)
      neuteringSavedHtml(input$savedText_neuteringTop)
      writeLines(input$savedText_neuteringTop, neuteringTextPath)
      showNotification("Text updated successfully!", type = "message")
    })
  })
  
  observe({
    runjs("
    $('#timeframe_pred').on('input change', function() {
      let value = $(this).val();
      if (value < 2) {
        $(this).val(2);
      }
    });
  ")
  })
  
  
  ## initiate reactive values
  rv <- reactiveValues(Catvec2 = c(2.60E+03, 8.76E+02, 8.88E+00, 1.60E+03, 1.65E+01, 1.04E+02, 1.06E+00,
                                   1.10E+02, 3.06E+02, 1.25E+01, 7.22E+02, 1.09E+03, 2.98E+01, 6.96E+01,
                                   7.76E+01, 0.00E+00, 7.85E+00, 0.00E+00, 1.30E+02, 0.00E+00, 1.68E+01,
                                   1.94E+03, 9.96E+02, 1.93E+03, 4.76E+03, 6.32E+04, 3.96E+02, 1.90E+04),
                       bFA = 0, bFJ = 0, bOA = 0, bOJ = 0, bStA = 0, bStJ = 0, counter = 0, 
                       MatA = 0, MatF = NULL, MatU = NULL, Feral = 0, Owned = 0, Shelter = 0, Stray = 0, 
                       overallbFA = 0, overallbFJ = 0, overallbOA = 0, overallbOJ = 0, overallbStA = 0, overallbStJ = 0, 
                       SeasonalReproduction = 0, TFAtoE = 0, TFJtoA = 0, TFKtoJ = 0, TFKUtoFKN = 0, TFtoO = 0, TFtoSh = 0, 
                       TFUtoFN = 0, TOAtoE = 0, TOAUtoOAN = 0, TOJtoA = 0, TOJUtoOJN = 0.13, TOKtoJ = 0, TOKUtoOKN = 0, 
                       Total = 0, TOtoShA = 0, TOtoShE = 0, TOtoShJ = 0, TOtoShK = 0, TOtoStA = 0, TOtoStE = 0, TOtoStJ = 0, 
                       TOtoStK = 0, TShAtoE = 0, TShJtoA = 0, TShKtoJ = 0, TShtoO = 0, TStAtoE = 0, TStJtoA = 0, TStKtoJ = 0, 
                       TSttoF = 0, TSttoO = 0, TSttoSh = 0, TStUtoOAN = 0, TStUtoOJN = 0, TStUtoOKN = 0, σFA = 0, σFE = 0, 
                       σFJ = 0, σFK = 0, σOA = 0, σOE = 0, σOJ = 0, σOK = 0, σShA = 0, σShE = 0, σShJ = 0, σShK = 0, 
                       σStA = 0, σStE = 0, σStJ = 0, σStK = 0, OwnedHist = 0, ShelterHist = 0, StrayHist = 0, FeralHist = 0, TotalHist = 0,
                       OwnedP = 0, ShelterP = 0, StrayP = 0, FeralP = 0, iterationCounter = 0, PG_Owned = 0, PG_Feral = 0, Total = 0,
                       PG_Stray = 0, PG_Shelter = 0, go = 0, runs = list(), current_run = NULL, compare = FALSE, lengthoftime = 0)
  
  ## Reset RV values function
  resetReactiveValues <- function() {
    
    rv$go = 0
    rv$Catvec2 = c(2.60E+03, 8.76E+02, 8.88E+00, 1.60E+03, 1.65E+01, 1.04E+02, 1.06E+00,
                   1.10E+02, 3.06E+02, 1.25E+01, 7.22E+02, 1.09E+03, 2.98E+01, 6.96E+01,
                   7.76E+01, 0.00E+00, 7.85E+00, 0.00E+00, 1.30E+02, 0.00E+00, 1.68E+01,
                   1.94E+03, 9.96E+02, 1.93E+03, 4.76E+03, 6.32E+04, 3.96E+02, 1.90E+04)
    rv$bFA = 0
    rv$bFJ = 0
    rv$bOA = 0
    rv$bOJ = 0
    rv$bStA = 0
    rv$bStJ = 0
    rv$counter = 0
    rv$MatA = 0
    rv$MatF = NULL
    rv$MatU = NULL
    rv$Feral = 0
    rv$Owned = 0
    rv$Shelter = 0
    rv$Stray = 0
    rv$Total = 0
    rv$overallbFA = 0
    rv$overallbFJ = 0
    rv$overallbOA = 0
    rv$overallbOJ = 0
    rv$overallbStA = 0
    rv$overallbStJ = 0
    rv$SeasonalReproduction = 0
    rv$TFAtoE = 0
    rv$TFJtoA = 0
    rv$TFKtoJ = 0
    rv$TFKUtoFKN = 0
    rv$TFtoO = 0
    rv$TFtoSh = 0
    rv$TFUtoFN = 0
    rv$TOAtoE = 0
    rv$TOAUtoOAN = 0
    rv$TOJtoA = 0
    rv$TOJUtoOJN = 0.13
    rv$TOKtoJ = 0
    rv$TOKUtoOKN = 0
    rv$Total = 0
    rv$TOtoShA = 0
    rv$TOtoShE = 0
    rv$TOtoShJ = 0
    rv$TOtoShK = 0
    rv$TOtoStA = 0
    rv$TOtoStE = 0
    rv$TOtoStJ = 0
    rv$TOtoStK = 0
    rv$TShAtoE = 0
    rv$TShJtoA = 0
    rv$TShKtoJ = 0
    rv$TShtoO = 0
    rv$TStAtoE = 0
    rv$TStJtoA = 0
    rv$TStKtoJ = 0
    rv$TSttoF = 0
    rv$TSttoO = 0
    rv$TSttoSh = 0
    rv$TStUtoOAN = 0
    rv$TStUtoOJN = 0
    rv$TStUtoOKN = 0
    rv$σFA = 0
    rv$σFE = 0
    rv$σFJ = 0
    rv$σFK = 0
    rv$σOA = 0
    rv$σOE = 0
    rv$σOJ = 0
    rv$σOK = 0
    rv$σShA = 0
    rv$σShE = 0
    rv$σShJ = 0
    rv$σShK = 0
    rv$σStA = 0
    rv$σStE = 0
    rv$σStJ = 0
    rv$σStK = 0
    rv$OwnedHist = 0
    rv$ShelterHist = 0
    rv$StrayHist = 0
    rv$FeralHist = 0
    OwnedP = 0
    rv$ShelterP = 0
    rv$StrayP = 0
    rv$FeralP = 0
    rv$iterationCounter = 0
    rv$PG_Owned = 0
    rv$PG_Stray = 0
    rv$PG_Shelter = 0
    rv$PG_Feral = 0
    rv$compare = FALSE
    updateSliderInput(session, "timeframe_pred", value = 10)
    updateRadioButtons(session, "owned_neut_rate_A", selected = "95%")
    updateSliderInput(session, "owned_neut_rate_K", value = 30)
    enable("timeframe_pred")
    enable("owned_neut_rate_A")
    enable("owned_neut_rate_K")
  }
  
  ## Run model code functions:
  observeEvent(input$setUp, {
    #browser()
    rv$show_params <- TRUE
    hide("setUp")
    show("resetButton")
    show("goButton")
    
    disable("timeframe_pred")
    disable("owned_neut_rate_A")
    disable("owned_neut_rate_K")
    
    ## Time ###############
    nyears <- input$timeframe_pred
    rv$lengthoftime <- 12 * nyears
    rv$counter <- 1
    rv$show_params <- TRUE
    
    # user input neuter rates
    owned_neut_rate_K <- input$owned_neut_rate_K / 100
    selected_rate_K_index <- (owned_neut_rate_K - 0.05) / 0.05 + 1
    rv$TOKUtoOKN <- owned_neut_rate_K
    rv$StUtoOKN <- owned_neut_rate_K
    
    # set TOAUtoOAN and TStutoOAN
    if (input$owned_neut_rate_A == "90%") {
      rv$TOAUtoOAN <- c(0.757249083,0.743762921,0.728690152,0.711733286,0.692515505,0.670552327,
                        0.645210198,0.615644382,0.580702962,0.538773258)
      rv$TStUtoOAN <- 0.9
      
    } else if (input$owned_neut_rate_A == "95%") {
      rv$TOAUtoOAN <- c(0.878624542,0.871881461,0.864345076,0.855866643,0.846257753,0.835276164,
                        0.822605099,0.807822191,0.790351481,0.769386629)
      rv$TStUtoOAN <- 0.95
    } else {
      rv$TOAUtoOAN <- c(0.951449817, 0.948752584, 0.94573803,  0.942346657, 0.938503101, 0.934110465, 
                        0.92904204,0.923128876,0.916140592, 0.907754652)
      rv$TStUtoOAN <- 0.98
    }
    
    #define TStutoOJN
    rv$TStUtoOJN <- c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,
                      0.6048982,0.63782335,0.6707485)
    
    # Select the corresponding TOAUtoOAN and TStutoOJN value based on the selected_rate_K_index
    rv$TOAUtoOAN <- rv$TOAUtoOAN[selected_rate_K_index]
    rv$TStUtoOJN <- rv$TStUtoOJN[selected_rate_K_index]
    
    ## Derived or set parameters #################
    #account for seasonality in breeding - vector to be multiplied with whatever annual reproductive rate is specified as in model code
    rv$SeasonalReproduction = c(rep(c(rep(0.0366667, 3), rep(0.13, 6), rep(0.0366667, 3)), nyears), 0.0366667)
    
    #FERAL PARAMETERS
    # Neuter rate ferals  
    rv$TFKUtoFKN <- 0
    rv$TFUtoFN <- 0.01
    #Survival ferals
    rv$σFK <- 0.81
    rv$σFJ <- 0.92
    rv$σFA <- 0.96
    rv$σFE <- 0.9
    #Transition to other subpop ferals
    rv$TFtoSh <- 0.003
    rv$TFtoO <- 0.02
    #birth rate ferals
    rv$overallbFJ <- 1.5
    rv$overallbFA <- 2.5
    rv$bFJ <- rv$SeasonalReproduction[1] * rv$overallbFJ
    rv$bFA <- rv$SeasonalReproduction[1] * rv$overallbFA
    
    #SHELTER PARAMETERS
    #survival rates
    rv$σShK <- 0.974
    rv$σShJ <- 0.993
    rv$σShA <- 0.985
    rv$σShE <- 0.9
    #transition to owned shelters
    rv$TShtoO <- 0.63
    
    #OWNED PARAMETERS
    #survival rates owned cats
    rv$σOK <- 0.97
    rv$σOJ <- 0.995
    rv$σOA <- 0.995
    rv$σOE <- 0.98
    #transition out of owned cats
    rv$TOtoStK <- 0.0009
    rv$TOtoStJ <- 0.0009
    rv$TOtoStA <- 0.0009
    rv$TOtoStE <- 0.0009
    rv$TOtoShK <- 0.002
    rv$TOtoShJ <- 0.002
    rv$TOtoShA <- 0.002
    rv$TOtoShE <- 0.002
    #birth rates owned
    rv$overallbOJ <- 1.4
    rv$overallbOA <- 2.1
    rv$bOJ <- rv$SeasonalReproduction[1] * rv$overallbOJ
    rv$bOA <- rv$SeasonalReproduction[1] * rv$overallbOA
    
    #STRAY PARAMETERS
    #survival strays
    rv$σStK <- 0.918
    rv$σStJ <- 0.97
    rv$σStA <- 0.97
    rv$σStE <- 0.9
    #neuter rates stray
    #rv$TStUtoOKN <- 0.41
    rv$TStUtoOJN <- 0.61
    #rv$TStUtoOAN <- 0.95 - Now conditional
    
    #transition out of stray subpop
    rv$TSttoSh <- 0.03
    rv$TSttoO <- 0.04
    rv$TSttoF <- 0.14186
    #birth rates stray
    rv$overallbStJ <- 1.5
    rv$overallbStA <- 2.5
    rv$bStJ <- rv$SeasonalReproduction[1] * rv$overallbStJ
    rv$bStA <- rv$SeasonalReproduction[1] * rv$overallbStA
    
    #Aging parameters - SAS 
    p <- 0:6; rv$TFKtoJ <- rv$σFK^5 / sum(rv$σFK^p)
    p <- 0:6; rv$TFJtoA <- rv$σFJ^5 / sum(rv$σFJ^p)
    p <- 0:120; rv$TFAtoE <- rv$σFA^119 / sum(rv$σFA^p)
    p <- 0:6; rv$TStKtoJ <- rv$σStK^5 / sum(rv$σStK^p)
    p <- 0:6; rv$TStJtoA <- rv$σStJ^5 / sum(rv$σStJ^p)
    p <- 0:120; rv$TStAtoE <- rv$σStA^119 / sum(rv$σStA^p)
    p <- 0:6; rv$TShKtoJ <- rv$σShK^5 / sum(rv$σShK^p)
    p <- 0:6; rv$TShJtoA <- rv$σShJ^5 / sum(rv$σShJ^p)
    p <- 0:120; rv$TShAtoE <- rv$σShA^119 / sum(rv$σShA^p)
    p <- 0:6; rv$TOKtoJ <- rv$σOK^5 / sum(rv$σOK^p)
    p <- 0:6; rv$TOJtoA <- rv$σOJ^5 / sum(rv$σOJ^p)
    p <- 0:120; rv$TOAtoE <- rv$σOA^119 / sum(rv$σOA^p)
    
    ##Megamodel build- transition matrix
    rv$MatU <- matrix(c(rv$σFK * (1-rv$TFKtoJ)*(1- rv$TFtoSh-rv$TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        rv$σFK * (rv$TFKtoJ)*(1-rv$TFKUtoFKN)	,	rv$σFJ * (1-rv$TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        rv$σFK * (rv$TFKtoJ)*(rv$TFKUtoFKN)	,	0	,	rv$σFJ * (1-rv$TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	rv$σFJ * rv$TFJtoA*(1-rv$TFUtoFN)	,	0	,	rv$σFA*(1- rv$TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	rv$σFJ * rv$TFJtoA*rv$TFUtoFN	,	rv$σFJ * rv$TFJtoA	,	0	,	rv$σFA*(1- rv$TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	rv$σFA* rv$TFAtoE	,	0	,	rv$σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	rv$σFA* rv$TFAtoE	,	0	,	rv$σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*(1- rv$TSttoSh-rv$TSttoO-rv$TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*(1- rv$TSttoSh-rv$TSttoO)	,	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoStJ*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA*(1-rv$TOAUtoOAN[1])	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA*rv$TOAUtoOAN[1]	,	rv$σOJ * rv$TOJtoA*rv$TOtoStA	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoStE	,	0	,	rv$σOE * rv$TOtoStE	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoStE	,	0	,	rv$σOE * rv$TOtoStE	,
                        rv$σFK * (1-rv$TFKtoJ)*rv$TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σShK * (1-rv$TShKtoJ)*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoSh	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoShJ*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	rv$σShK * rv$TShKtoJ*(1- rv$TShtoO)	,	rv$σShJ* (1-rv$TShJtoA)*(1- rv$TShtoO)	,	rv$σShJ * (1-rv$TShJtoA)*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoSh	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA*(1-rv$TOAUtoOAN[1])	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoSh	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	,	0	,	0	,	0	,	rv$σShJ* rv$TShJtoA*(1- rv$TShtoO)	,	rv$σShJ * rv$TShJtoA*(1- rv$TShtoO)	,	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	,	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA*rv$TOAUtoOAN[1]	,	rv$σOJ * rv$TOJtoA*rv$TOtoShA	,	0	,	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoSh	,	0	,	rv$σStE *rv$TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoShE	,	0	,	rv$σOE * rv$TOtoShE	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoSh	,	0	,	rv$σStE *rv$TSttoSh	,	0	,	0	,	0	,	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	,	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	,	rv$σShE*(1- rv$TShtoO)	,	rv$σShE*(1- rv$TShtoO)	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*rv$TOtoShE	,	0	,	rv$σOE * rv$TOtoShE	,
                        rv$σFK * (1-rv$TFKtoJ)*rv$TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σShK * (1-rv$TShKtoJ)*rv$TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * (1-rv$TOKtoJ)*(1- rv$TOtoShK-rv$TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoO *(1-rv$TStUtoOKN)	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*(1-rv$TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOKUtoOKN)	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK * rv$TStKtoJ*rv$TSttoO *rv$TStUtoOKN	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*rv$TStUtoOJN	,	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO	,	0	,	0	,	0	,	0	,	rv$σShK * rv$TShKtoJ*rv$TShtoO	,	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	,	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	,	0	,	0	,	0	,	0	,	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOKUtoOKN	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOJUtoOJN	,	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)	,	0	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*(1-rv$TOAUtoOAN[1])	,	0	,	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStJ * rv$TStJtoA*rv$TSttoO*rv$TStUtoOAN	,	rv$σStJ * rv$TStJtoA*rv$TSttoO	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*rv$TStUtoOAN	,	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO	,	0	,	0	,	0	,	rv$σShJ * rv$TShJtoA*rv$TShtoO	,	rv$σShJ * rv$TShJtoA*rv$TShtoO	,	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	,	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	,	0	,	0	,	0	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*rv$TOAUtoOAN[1]	,	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoO*(1-rv$TStUtoOAN)	,	0	,	rv$σStE *rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	,	0	,	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	,	0	,
                        0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStA * rv$TStAtoE*rv$TSttoO*rv$TStUtoOAN	,	rv$σStA * rv$TStAtoE*rv$TSttoO	,	0	,	rv$σStE *rv$TSttoO	,	0	,	0	,	0	,	rv$σShA *rv$TShAtoE*rv$TShtoO	,	rv$σShA *rv$TShAtoE*rv$TShtoO	,	rv$σShE*rv$TShtoO	,	rv$σShE*rv$TShtoO	,	0	,	0	,	0	,	0	,	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	,	0	,	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	
    ) ,nrow=28, byrow=T)
    
    #Build fecundity matrix
    rv$MatF <- matrix(c(    rv$σFK *rv$TFKtoJ*rv$bFJ *(1- rv$TFtoSh-rv$TFtoO)*(1-rv$TFKUtoFKN)	,	rv$σFJ * rv$bFJ	,	0	,	rv$σFA * rv$bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK *rv$TStKtoJ*rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	,	rv$σStJ *rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	rv$σStA *rv$bStA *(1- rv$TSttoSh-rv$TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoStK*(1-rv$TOKUtoOKN)	,	rv$σOJ *rv$bOJ*rv$TOtoStJ*(1-rv$TOJUtoOJN)	,	0	,	rv$σOA *rv$bOA* rv$TOtoStA	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	(rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoSh)/2	,	(rv$σStJ *rv$bStJ *rv$TSttoSh)/2	,	0	,	(rv$σStA *rv$bStA *rv$TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoShK*(1-rv$TOKUtoOKN))/2	,	(rv$σOJ *rv$bOJ*rv$TOtoShJ*(1-rv$TOJUtoOJN))/2	,	0	,	(rv$σOA *rv$bOA* rv$TOtoShA)/2	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoO	,	rv$σStJ *rv$bStJ *rv$TSttoO	,	0	,	rv$σStA *rv$bStA *rv$TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	rv$σOK *rv$TOKtoJ*rv$bOJ *(1- rv$TOtoShK-rv$TOtoStK)*(1-rv$TOKUtoOKN)	,	rv$σOJ *rv$bOJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	,	0	,	rv$σOA *rv$bOA*(1- rv$TOtoShA-rv$TOtoStA)	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                            0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
    ), nrow=28, byrow=T)
    
    
    rv$MatA <- rv$MatU + rv$MatF
    
    rv$VectorMatrix = matrix(ncol = 28,  nrow = rv$lengthoftime)
    rv$VectorMatrix[1, ] <- rv$Catvec2
    
    rv$MATS[[1]] <- rv$MatA
    rv$Total <- sum(rv$Catvec2)
    rv$Owned <- sum(rv$Catvec2[22:28])
    rv$Feral <- sum(rv$Catvec2[1:7])
    rv$Stray <- sum(rv$Catvec2[8:14])
    rv$Shelter <- sum(rv$Catvec2[15:21])
    
    rvTotalHist <- rv$Total
    rv$OwnedHist <- rv$Owned
    rv$FeralHist <- rv$Feral
    rv$StrayHist <- rv$Stray
    rv$ShelterHist <- rv$Shelter
  }) # Setup Button
  
  observeEvent(input$goButton, {
    
    hide("goButton")
 
    rv$go <- 1
    
    for (i in 1:(rv$lengthoftime - 1)){
      print(paste("Iteration:", i))
      
      #browser()
      rv$counter < - rv$counter + i
      rv$CatsProject <- rv$MATS[[i]] %*% rv$VectorMatrix[i, ]
      
      rv$VectorMatrix[(i+1), ] <- rv$CatsProject
      rv$NOwned <- sum(rv$VectorMatrix[(i+1), 22:28])
      rv$NShelter <- sum(rv$VectorMatrix[(i+1), 15:21])
      rv$NStray <- sum(rv$VectorMatrix[(i+1), 8:14])
      rv$NFeral <- sum(rv$VectorMatrix[(i+1), 1:7])
      
      #carrying capacity functions for homes and shelters- parameters change if move 10% above initial stage strucutre,  although slight changes at 1%
      if(rv$NOwned>sum(rv$Catvec2[22:28])*1.1){
        #can't increase shelter as that is often at capacity already
        rv$TOtoStK <- 0.0009
        rv$TOtoStJ <- 0.0009
        rv$TOtoStA <- 0.0009
        rv$TOtoStE <- 0.0009
        rv$TFtoO <- 0.02*0.01
        rv$TSttoO <- 0.04*0.01
      }else if(rv$NOwned>(sum(rv$Catvec2[22:28])*1.01) & rv$NOwned<(sum(rv$Catvec2[22:28])*1.1)){
        rv$TOtoStK <- 0.0009*1.25
        rv$TOtoStJ <- 0.0009*1.25
        rv$TOtoStA <- 0.0009*1.25
        rv$TOtoStE <- 0.0009*1.25
        rv$TFtoO <- 0.02*0.25#0.0075
        rv$TSttoO <- 0.04*0.45
      }else {
        rv$TOtoStK <- 0.0009
        rv$TOtoStJ <- 0.0009
        rv$TOtoStA <- 0.0009
        rv$TOtoStE <- 0.0009
        rv$TFtoO <- 0.02#0.01
        rv$TSttoO <- 0.04
      }
      
      
      if(rv$NShelter>(sum(rv$Catvec2[15:21])*1.1)){
        rv$TFtoSh <- 0.003*0.25
        rv$TSttoSh <- 0.03*0.25
        rv$TOtoShK <- 0.002*0.5#0.0015#1798
        rv$TOtoShJ <- 0.002*0.5#0.0015#1798
        rv$TOtoShA <- 0.002*0.5#0.0015#1798
        rv$TOtoShE <- 0.002*0.5#0.0015#1798
        
      }
      else if(rv$NShelter>(sum(rv$Catvec2[15:21])*1.01) & rv$NShelter<(sum(rv$Catvec2[15:21])*1.1)){
        rv$TFtoSh <- 0.003*0.75#0.0015
        rv$TSttoSh <- 0.03*0.75#0.015
        rv$TOtoShK <- 0.002*0.75#0.0015#1798
        rv$TOtoShJ <- 0.002*0.75#0.0015#1798
        rv$TOtoShA <- 0.002*0.75#0.0015#1798
        rv$TOtoShE <- 0.002*0.75#0.0015#1798
      }
      else {
        rv$TFtoSh <- 0.003
        rv$TSttoSh <- 0.03
        rv$TOtoShK <- 0.002#1798
        rv$TOtoShJ <- 0.002#1798
        rv$TOtoShA <- 0.002#1798
        rv$TOtoShE <- 0.002#1798
      }
      #birth rates change monthly to account for seasonality
      rv$bFJ <- rv$SeasonalReproduction[(i+1)]*rv$overallbFJ
      rv$bFA <- rv$SeasonalReproduction[(i+1)]*rv$overallbFA
      
      rv$bOJ <- rv$SeasonalReproduction[(i+1)]*rv$overallbOJ
      rv$bOA <- rv$SeasonalReproduction[(i+1)]*rv$overallbOA
      
      rv$bStJ <- rv$SeasonalReproduction[(i+1)]*rv$overallbStJ
      rv$bStA <- rv$SeasonalReproduction[(i+1)]*rv$overallbStA
      
      #matrices are rebuilt
      
      rv$MatU <- matrix(c(rv$σFK * (1-rv$TFKtoJ)*(1- rv$TFtoSh-rv$TFtoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoF	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          rv$σFK * (rv$TFKtoJ)*(1-rv$TFKUtoFKN)	, 	rv$σFJ * (1-rv$TFJtoA)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          rv$σFK * (rv$TFKtoJ)*(rv$TFKUtoFKN)	, 	0	, 	rv$σFJ * (1-rv$TFJtoA)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	rv$σFJ * rv$TFJtoA*(1-rv$TFUtoFN)	, 	0	, 	rv$σFA*(1- rv$TFAtoE)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	rv$σFJ * rv$TFJtoA*rv$TFUtoFN	, 	rv$σFJ * rv$TFJtoA	, 	0	, 	rv$σFA*(1- rv$TFAtoE)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	rv$σFA* rv$TFAtoE	, 	0	, 	rv$σFE	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	rv$σFA* rv$TFAtoE	, 	0	, 	rv$σFE	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * (1-rv$TStKtoJ)*(1- rv$TSttoSh-rv$TSttoO-rv$TSttoF)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoStK	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * rv$TStKtoJ*(1- rv$TSttoSh-rv$TSttoO)	, 	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*(1-rv$TOKUtoOKN)	, 	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoStJ*(1-rv$TOJUtoOJN)	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * (1-rv$TStJtoA)*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*rv$TOtoStJ*rv$TOKUtoOKN	, 	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ*rv$TOJUtoOJN	, 	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoStJ	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*rv$TOtoStA*(1-rv$TOAUtoOAN)	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	rv$σStA * (1-rv$TStAtoE)*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*rv$TOtoStA*rv$TOAUtoOAN	, 	rv$σOJ * rv$TOJtoA*rv$TOtoStA	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*rv$TOtoStA	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*rv$TOtoStE	, 	0	, 	rv$σOE * rv$TOtoStE	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	rv$σStE *(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*rv$TOtoStE	, 	0	, 	rv$σOE * rv$TOtoStE	, 
                          rv$σFK * (1-rv$TFKtoJ)*rv$TFtoSh	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoSh	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σShK * (1-rv$TShKtoJ)*(1- rv$TShtoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * (1-rv$TOKtoJ)*rv$TOtoShK	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * rv$TStKtoJ*rv$TSttoSh	, 	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*(1-rv$TOKUtoOKN)	, 	rv$σOJ * (1-rv$TOJtoA)* rv$TOtoShJ*(1-rv$TOJUtoOJN)	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoSh	, 	0	, 	0	, 	0	, 	0	, 	rv$σShK * rv$TShKtoJ*(1- rv$TShtoO)	, 	rv$σShJ* (1-rv$TShJtoA)*(1- rv$TShtoO)	, 	rv$σShJ * (1-rv$TShJtoA)*(1- rv$TShtoO)	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*rv$TOtoShJ*rv$TOKUtoOKN	, 	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ*rv$TOJUtoOJN	, 	rv$σOJ * (1-rv$TOJtoA)*rv$TOtoShJ	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*rv$TSttoSh	, 	0	, 	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*rv$TOtoShA*(1-rv$TOAUtoOAN)	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*rv$TSttoSh	, 	0	, 	rv$σStA * (1-rv$TStAtoE)*rv$TSttoSh	, 	0	, 	0	, 	0	, 	rv$σShJ* rv$TShJtoA*(1- rv$TShtoO)	, 	rv$σShJ * rv$TShJtoA*(1- rv$TShtoO)	, 	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	, 	rv$σShA* (1-rv$TShAtoE)*(1- rv$TShtoO)	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*rv$TOtoShA*rv$TOAUtoOAN	, 	rv$σOJ * rv$TOJtoA*rv$TOtoShA	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*rv$TOtoShA	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*rv$TSttoSh	, 	0	, 	rv$σStE *rv$TSttoSh	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*rv$TOtoShE	, 	0	, 	rv$σOE * rv$TOtoShE	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*rv$TSttoSh	, 	0	, 	rv$σStE *rv$TSttoSh	, 	0	, 	0	, 	0	, 	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	, 	rv$σShA *rv$TShAtoE*(1- rv$TShtoO)	, 	rv$σShE*(1- rv$TShtoO)	, 	rv$σShE*(1- rv$TShtoO)	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*rv$TOtoShE	, 	0	, 	rv$σOE * rv$TOtoShE	, 
                          rv$σFK * (1-rv$TFKtoJ)*rv$TFtoO	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * (1-rv$TStKtoJ)*rv$TSttoO	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σShK * (1-rv$TShKtoJ)*rv$TShtoO	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * (1-rv$TOKtoJ)*(1- rv$TOtoShK-rv$TOtoStK)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * rv$TStKtoJ*rv$TSttoO *(1-rv$TStUtoOKN)	, 	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*(1-rv$TStUtoOJN)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOKUtoOKN)	, 	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK * rv$TStKtoJ*rv$TSttoO *rv$TStUtoOKN	, 	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO*rv$TStUtoOJN	, 	rv$σStJ * (1-rv$TStJtoA)*rv$TSttoO	, 	0	, 	0	, 	0	, 	0	, 	rv$σShK * rv$TShKtoJ*rv$TShtoO	, 	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	, 	rv$σShJ * (1-rv$TShJtoA)*rv$TShtoO	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK * rv$TOKtoJ*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOKUtoOKN	, 	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)*rv$TOJUtoOJN	, 	rv$σOJ * (1-rv$TOJtoA)*(1- rv$TOtoShJ-rv$TOtoStJ)	, 	0	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*rv$TSttoO*(1-rv$TStUtoOAN)	, 	0	, 	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*(1-rv$TStUtoOAN)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*(1-rv$TOAUtoOAN)	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	, 	0	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStJ * rv$TStJtoA*rv$TSttoO*rv$TStUtoOAN	, 	rv$σStJ * rv$TStJtoA*rv$TSttoO	, 	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO*rv$TStUtoOAN	, 	rv$σStA * (1-rv$TStAtoE)*rv$TSttoO	, 	0	, 	0	, 	0	, 	rv$σShJ * rv$TShJtoA*rv$TShtoO	, 	rv$σShJ * rv$TShJtoA*rv$TShtoO	, 	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	, 	rv$σShA* (1-rv$TShAtoE)*rv$TShtoO	, 	0	, 	0	, 	0	, 	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)*rv$TOAUtoOAN	, 	rv$σOJ * rv$TOJtoA*(1- rv$TOtoShA-rv$TOtoStA)	, 	0	, 	rv$σOA * (1-rv$TOAtoE)*(1- rv$TOtoShA-rv$TOtoStA)	, 	0	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*rv$TSttoO*(1-rv$TStUtoOAN)	, 	0	, 	rv$σStE *rv$TSttoO	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	, 	0	, 	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	, 	0	, 
                          0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStA * rv$TStAtoE*rv$TSttoO*rv$TStUtoOAN	, 	rv$σStA * rv$TStAtoE*rv$TSttoO	, 	0	, 	rv$σStE *rv$TSttoO	, 	0	, 	0	, 	0	, 	rv$σShA *rv$TShAtoE*rv$TShtoO	, 	rv$σShA *rv$TShAtoE*rv$TShtoO	, 	rv$σShE*rv$TShtoO	, 	rv$σShE*rv$TShtoO	, 	0	, 	0	, 	0	, 	0	, 	rv$σOA * rv$TOAtoE*(1- rv$TOtoShE-rv$TOtoStE)	, 	0	, 	rv$σOE * (1- rv$TOtoShE-rv$TOtoStE)	
      ) , nrow=28, byrow=T)
      
      
      rv$MatF <- matrix(c(    rv$σFK *rv$TFKtoJ*rv$bFJ *(1- rv$TFtoSh-rv$TFtoO)*(1-rv$TFKUtoFKN)	, 	rv$σFJ * rv$bFJ	, 	0	, 	rv$σFA * rv$bFA	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK *rv$TStKtoJ*rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	, 	rv$σStJ *rv$bStJ *(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	rv$σStA *rv$bStA *(1- rv$TSttoSh-rv$TSttoO)	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoStK*(1-rv$TOKUtoOKN)	, 	rv$σOJ *rv$bOJ*rv$TOtoStJ*(1-rv$TOJUtoOJN)	, 	0	, 	rv$σOA *rv$bOA* rv$TOtoStA	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	(rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoSh)/2	, 	(rv$σStJ *rv$bStJ *rv$TSttoSh)/2	, 	0	, 	(rv$σStA *rv$bStA *rv$TSttoSh)/2	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	(rv$σOK *rv$TOKtoJ*rv$bOJ *rv$TOtoShK*(1-rv$TOKUtoOKN))/2	, 	(rv$σOJ *rv$bOJ*rv$TOtoShJ*(1-rv$TOJUtoOJN))/2	, 	0	, 	(rv$σOA *rv$bOA* rv$TOtoShA)/2	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σStK *rv$TStKtoJ*rv$bStJ *rv$TSttoO	, 	rv$σStJ *rv$bStJ *rv$TSttoO	, 	0	, 	rv$σStA *rv$bStA *rv$TSttoO	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	rv$σOK *rv$TOKtoJ*rv$bOJ *(1- rv$TOtoShK-rv$TOtoStK)*(1-rv$TOKUtoOKN)	, 	rv$σOJ *rv$bOJ*(1- rv$TOtoShJ-rv$TOtoStJ)*(1-rv$TOJUtoOJN)	, 	0	, 	rv$σOA *rv$bOA*(1- rv$TOtoShA-rv$TOtoStA)	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 
                              0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	, 	0	
      ) , nrow=28, byrow=T)
      
      rv$MatA <- rv$MatU+rv$MatF
      rv$MATS[[i+1]] <- rv$MatA
      
      
    }
    #browser()
    rv$Total <- rowSums(rv$VectorMatrix)
    rv$Owned <- rowSums(rv$VectorMatrix[, 22:28])
    rv$Feral <- rowSums(rv$VectorMatrix[, 1:7])
    rv$Stray <- rowSums(rv$VectorMatrix[, 8:14])
    rv$Shelter <- rowSums(rv$VectorMatrix[, 15:21])
    
    start_window <- max(1, rv$lengthoftime - 12)
    rv$PG_Total <- sum(rv$Total[start_window:rv$lengthoftime]) / sum(rv$Total[1:12])
    rv$PG_Owned <- sum(rv$Owned[start_window:rv$lengthoftime]) / sum(rv$Owned[1:12])
    rv$PG_Feral <- sum(rv$Feral[start_window:rv$lengthoftime]) / sum(rv$Feral[1:12])
    rv$PG_Stray <- sum(rv$Stray[start_window:rv$lengthoftime]) / sum(rv$Stray[1:12])
    rv$PG_Shelter <- sum(rv$Shelter[start_window:rv$lengthoftime]) / sum(rv$Shelter[1:12])
    
    # Save the current run
    current_run <- list(
      Total = rv$Total,      # Total populations
      Owned = rv$Owned,      # Owned populations
      Feral = rv$Feral,      # Feral populations
      Stray = rv$Stray,      # Stray populations
      Shelter = rv$Shelter,  # Shelter populations
      PG_Total = rv$PG_Total,
      PG_Owned = rv$PG_Owned,
      PG_Feral = rv$PG_Feral,
      PG_Stray = rv$PG_Stray,
      PG_Shelter = rv$PG_Shelter,
      parameters = list(
        owned_neut_rate_K = rv$TOKUtoOKN,
        owned_neut_rate_A = rv$TStUtoOAN
      )
    )
    
    rv$runs[[length(rv$runs) + 1]] <- current_run  # Append to saved runs
    rv$current_run <- current_run                  # Store as the current run
    
    # Show notification for successful save
    showNotification("Model run saved successfully!", type = "message")
    
    show("resetButton")
    show("exportRuns")
    
    if (length(rv$runs) > 1) {
      shinyjs::show("compareButton")
    }
    
  }) # Go Button
  
  observeEvent(input$resetButton, {
    #browser()
    print(rv$StUtoOKN)
    rv$go <- rv$go + 1
    # Hide buttons for the current run
    hide("resetButton")
    hide("compareButton")
    hide("goButton")
    hide("downloadRuns")
    show("setUp")
    
    # Reset reactive values to initial state
    resetReactiveValues()
    
  }) # Save/reset Button
  
  # Track the compareButton click and display previous run parameters
  observeEvent(input$compareButton, {
    if (length(rv$runs) > 1) {
      rv$compare <- !rv$compare
      rv$previousParams <- rv$runs[[length(rv$runs) - 1]]$parameters  # Select the second-to-last run
    } else {
      showNotification("No previous run to compare.", type = "warning")
    }
  })
  
  output$conditionalTextBox <- renderUI({
    div(
      class = "custom-box-content",
      verbatimTextOutput("runParams", placeholder = TRUE)
    )
  })
  
 output$runParams <- renderText({
  if (!isTRUE(rv$show_params)) {
    return("")  # If Setup hasn't been pressed, display nothing
  }
  
  paramNames <- list(
    owned_neut_rate_A = "Owned Adult Neutering Rate",
    owned_neut_rate_K = "Owned Kitten Neutering Rate"
  )
  
  # Helper function to generate parameter text
  generateParamText <- function(params, label) {

    if (!is.null(params)) {
      
      ordered_params <- c("owned_neut_rate_A", "owned_neut_rate_K")
      
      paramText <- c()
      for (param in ordered_params) {
        if (param %in% names(params)) {
          value <- params[[param]]
          
          # Multiply by 100 and format as percentage
          if (is.numeric(value)) {
            value <- sprintf("%.0f%%", value * 100)
          }
          
          paramText <- c(paramText, paste0(paramNames[[param]], ": ", value))
        }
      }
      
      paste0(label, ":\n", paste(paramText, collapse = "\n"))
    } else {
      paste0(label, ": No parameters to display.")
    }
  }
  
  # Generate text for both current and previous parameters
  currentRunText <- generateParamText(rv$current_run$parameters, "Current Run Parameters")
  previousRunText <- if (length(rv$runs) > 1) {
    generateParamText(rv$runs[[length(rv$runs) - 1]]$parameters, "Previous Run Parameters")
  } else {
    "Previous Run Parameters: No previous run to display."
  }
  
  # Combine and return the text with explicit headings
  paste0(currentRunText, "\n\n", previousRunText)
 })
  
  # Population Counts
  output$BxOwned <- renderText({
    if (rv$go == 0) {
      round(rv$OwnedHist)
    } else {
      round(rv$NOwned)
    }
  })
  
  output$BxShelter <- renderText({
    if (rv$go == 0) {
      round(rv$ShelterHist)
    } else {
      round(rv$NShelter)
    }
  })
  
  output$BxStray <- renderText({
    if (rv$go == 0) {
      round(rv$StrayHist)
    } else {
      round(rv$NStray)
    }
  })
  
  output$BxFeral <- renderText({
    if (rv$go == 0) {
      round(rv$FeralHist)
    } else {
      round(rv$NFeral)
    }
  })
  
  # Population Growth Rates
  output$BxOwnedG <- renderText({
    round(median(rv$PG_Owned), digits = 3)
  })
  output$BxShelterG <- renderText({
    round(median(rv$PG_Shelter), digits = 3)
  })
  output$BxStrayG <- renderText({
    round(median(rv$PG_Stray), digits = 3)
  })
  output$BxFeralG <- renderText({
    round(median(rv$PG_Feral), digits = 3)
  })
  
  
  output$OwnedPlot <- renderPlot({
    if (rv$go == 0) {
      plot_data <- data.frame(
        Time = 0,
        Owned = max(rv$Owned),
        Run = "Current"
      )
    } else {
      if (rv$compare && length(rv$runs) > 1) {
        # Get lengths of the current and previous runs
        previous_length <- length(rv$runs[[length(rv$runs) - 1]]$Owned)
        current_length <- length(rv$Owned)
        
        # Create plot data for each run
        previous_plot_data <- data.frame(
          Time = 1:previous_length,
          Owned = rv$runs[[length(rv$runs) - 1]]$Owned,
          Run = "Previous"
        )
        current_plot_data <- data.frame(
          Time = 1:current_length,
          Owned = rv$Owned,
          Run = "Current"
        )
        
        # Combine the two datasets
        plot_data <- rbind(previous_plot_data, current_plot_data)
      } else {
        current_length <- length(rv$Owned)
        plot_data <- data.frame(
          Time = 1:current_length,
          Owned = rv$Owned,
          Run = "Current"
        )
      }
    }
    
    ggplot(plot_data, aes(x = Time, y = Owned)) +
      geom_line(data = subset(plot_data, Run == "Previous"), 
                size = 2, colour = "#7A1b72", alpha = 0.3) +
      geom_line(data = subset(plot_data, Run == "Current"), 
                size = 2, colour = "#7A1b72") +
      annotate("point", x = 1, y = max(plot_data$Owned[plot_data$Run == "Current"][1]), size = 4, color = "#7A1b72") +
      labs(x = "Time (months)", y = "Owned Population") +
      theme(
        axis.text = element_text(size = 12),
        text = element_text(size = 15),
        panel.background = element_rect(fill = "#f5effb"),
        plot.background = element_rect(fill = "#f9f9f9")
      ) +
      scale_x_continuous(limits = c(0, max(plot_data$Time))) +
      scale_y_continuous(
        limits = c(max(0, min(plot_data$Owned) - 500), max(plot_data$Owned) + 500)
      ) +
      theme(legend.position = "none")
  })
  
  output$ShelterPlot <- renderPlot({
    if (rv$go == 0) {
      plot_data <- data.frame(
        Time = 0,
        Shelter = max(rv$Shelter),
        Run = "Current"
      )
    } else {
      if (rv$compare && length(rv$runs) > 1) {
        previous_length <- length(rv$runs[[length(rv$runs) - 1]]$Shelter)
        current_length <- length(rv$Shelter)
        
        previous_plot_data <- data.frame(
          Time = 1:previous_length,
          Shelter = rv$runs[[length(rv$runs) - 1]]$Shelter,
          Run = "Previous"
        )
        current_plot_data <- data.frame(
          Time = 1:current_length,
          Shelter = rv$Shelter,
          Run = "Current"
        )
        
        plot_data <- rbind(previous_plot_data, current_plot_data)
      } else {
        current_length <- length(rv$Shelter)
        plot_data <- data.frame(
          Time = 1:current_length,
          Shelter = rv$Shelter,
          Run = "Current"
        )
      }
    }
    
    ggplot(plot_data, aes(x = Time, y = Shelter)) +
      geom_line(data = subset(plot_data, Run == "Previous"),
                size = 2, colour = "#C928B2", alpha = 0.3) +
      geom_line(data = subset(plot_data, Run == "Current"),
                size = 2, colour = "#C928B2") +
      annotate("point", x = 1, y = max(plot_data$Shelter[plot_data$Run == "Current"][1]), size = 4, color = "#C928B2") +
      labs(x = "Time (months)", y = "Shelter Population") +
      theme(
        axis.text = element_text(size = 12),
        text = element_text(size = 15),
        panel.background = element_rect(fill = "#f5effb"),
        plot.background = element_rect(fill = "#f9f9f9")
      ) +
      scale_x_continuous(limits = c(0, max(plot_data$Time))) +
      scale_y_continuous(
        limits = c(max(0, min(plot_data$Shelter) - 50), max(plot_data$Shelter) + 50)
      ) +
      theme(legend.position = "none")
  })
  
  output$StrayPlot <- renderPlot({
    if (rv$go == 0) {
      plot_data <- data.frame(
        Time = 0,
        Stray = max(rv$Stray),
        Run = "Current"
      )
    } else {
      if (rv$compare && length(rv$runs) > 1) {
        previous_length <- length(rv$runs[[length(rv$runs) - 1]]$Stray)
        current_length <- length(rv$Stray)
        
        previous_plot_data <- data.frame(
          Time = 1:previous_length,
          Stray = rv$runs[[length(rv$runs) - 1]]$Stray,
          Run = "Previous"
        )
        current_plot_data <- data.frame(
          Time = 1:current_length,
          Stray = rv$Stray,
          Run = "Current"
        )
        
        plot_data <- rbind(previous_plot_data, current_plot_data)
      } else {
        current_length <- length(rv$Stray)
        plot_data <- data.frame(
          Time = 1:current_length,
          Stray = rv$Stray,
          Run = "Current"
        )
      }
    }
    
    ggplot(plot_data, aes(x = Time, y = Stray)) +
      geom_line(data = subset(plot_data, Run == "Previous"),
                size = 2, colour = "#7E3BE7", alpha = 0.3) +
      geom_line(data = subset(plot_data, Run == "Current"),
                size = 2, colour = "#7E3BE7") +
      annotate("point", x = 1, y = max(plot_data$Stray[plot_data$Run == "Current"][1]), size = 4, color = "#7E3BE7") +
      labs(x = "Time (months)", y = "Stray Population") +
      theme(
        axis.text = element_text(size = 12),
        text = element_text(size = 15),
        panel.background = element_rect(fill = "#f5effb"),
        plot.background = element_rect(fill = "#f9f9f9")
      ) +
      scale_x_continuous(limits = c(0, max(plot_data$Time))) +
      scale_y_continuous(
        limits = c(max(0, min(plot_data$Stray) - 200), max(plot_data$Stray) + 200)
      ) +
      theme(legend.position = "none")
  })
  
  output$FeralPlot <- renderPlot({
    if (rv$go == 0) {
      plot_data <- data.frame(
        Time = 0,
        Feral = max(rv$Feral),
        Run = "Current"
      )
    } else {
      if (rv$compare && length(rv$runs) > 1) {
        # Get lengths of the current and previous runs
        previous_length <- length(rv$runs[[length(rv$runs) - 1]]$Feral)
        current_length <- length(rv$Feral)
        
        # Create plot data for each run
        previous_plot_data <- data.frame(
          Time = 1:previous_length,
          Feral = rv$runs[[length(rv$runs) - 1]]$Feral,
          Run = "Previous"
        )
        current_plot_data <- data.frame(
          Time = 1:current_length,
          Feral = rv$Feral,
          Run = "Current"
        )
        
        # Combine the two datasets
        plot_data <- rbind(previous_plot_data, current_plot_data)
      } else {
        # Only the current run exists
        current_length <- length(rv$Feral)
        plot_data <- data.frame(
          Time = 1:current_length,
          Feral = rv$Feral,
          Run = "Current"
        )
      }
    }
    
    ggplot(plot_data, aes(x = Time, y = Feral)) +
      geom_line(data = subset(plot_data, Run == "Previous"),
                size = 2, colour = "#1A006B", alpha = 0.3) +
      geom_line(data = subset(plot_data, Run == "Current"),
                size = 2, colour = "#1A006B") +
      annotate("point", x = 1, y = max(plot_data$Feral[plot_data$Run == "Current"][1]), size = 4, color = "#1A006B") +
      labs(x = "Time (months)", y = "Feral Population") +
      theme(
        axis.text = element_text(size = 12),
        text = element_text(size = 15),
        panel.background = element_rect(fill = "#f5effb"),
        plot.background = element_rect(fill = "#f9f9f9")
      ) +
      scale_x_continuous(limits = c(0, max(plot_data$Time))) +
      scale_y_continuous(
        limits = c(max(0, min(plot_data$Feral) - 200), max(plot_data$Feral) + 200)
      ) +
      theme(legend.position = "none")
  })
  
  output$TotalPlot <- renderPlot({
    if (rv$go == 0) {
      plot_data <- data.frame(
        Time = 0,
        Total = max(rv$Total),
        Run = "Current"
      )
    } else {
      if (rv$compare && length(rv$runs) > 1) {
        # Get lengths of the current and previous runs
        previous_length <- length(rv$runs[[length(rv$runs) - 1]]$Total)
        current_length <- length(rv$Total)
        
        # Create plot data for each run
        previous_plot_data <- data.frame(
          Time = 1:previous_length,
          Total = rv$runs[[length(rv$runs) - 1]]$Total,
          Run = "Previous"
        )
        current_plot_data <- data.frame(
          Time = 1:current_length,
          Total = rv$Total,
          Run = "Current"
        )
        
        # Combine the two datasets
        plot_data <- rbind(previous_plot_data, current_plot_data)
      } else {
        # Only the current run exists
        current_length <- length(rv$Total)
        plot_data <- data.frame(
          Time = 1:current_length,
          Total = rv$Total,
          Run = "Current"
        )
      }
    }
    
    ggplot(plot_data, aes(x = Time, y = Total)) +
      geom_line(data = subset(plot_data, Run == "Previous"),
                size = 2, colour = "black", alpha = 0.3) +
      geom_line(data = subset(plot_data, Run == "Current"),
                size = 2, colour = "black") +
      annotate("point", x = 1, y = max(plot_data$Total[plot_data$Run == "Current"][1]), size = 4, color = "black") +
      labs(x = "Time (months)", y = "Total Population") +
      theme(
        axis.text = element_text(size = 12),
        text = element_text(size = 15),
        panel.background = element_rect(fill = "#f5eff5"),
        plot.background = element_rect(fill = "#f9f9f9")
      ) +
      scale_x_continuous(limits = c(0, max(plot_data$Time))) +
      scale_y_continuous(
        limits = c(max(0, min(plot_data$Total) - 500), max(plot_data$Total) + 500)
      ) +
      theme(legend.position = "none")
  })
  
  
  ## Export runs and plots ##
  observeEvent(input$exportRuns, {
    
    hide("exportRuns")
    show("downloadRuns")
    
    # Temporary directory to store files
    temp_dir <- tempdir()
    export_path <- file.path(temp_dir, "exported_runs")
    
    # Ensure the export directory is clean before saving new files
    if (dir.exists(export_path)) {
      unlink(export_path, recursive = TRUE)
    }
    dir.create(export_path, showWarnings = FALSE)
    
    # Define correct facet order
    population_order <- c("Total", "Owned", "Shelter", "Stray", "Feral")
    
    # Combine all runs into a single data frame for CSV, preserving correct time lengths
    combined_data <- do.call(rbind, lapply(seq_along(rv$runs), function(i) {
      run <- rv$runs[[i]]
      
      # Skip if the run is empty
      if (is.null(run) || is.null(run$Owned) || length(run$Owned) == 0) {
        return(NULL)
      }
      
      # Extract parameters
      params <- run$parameters
      owned_neut_rate_A <- ifelse(!is.null(params$owned_neut_rate_A), params$owned_neut_rate_A, NA)
      owned_neut_rate_K <- ifelse(!is.null(params$owned_neut_rate_K), params$owned_neut_rate_K, NA)
      
      # Use the actual length of the run (avoid repeating)
      actual_length <- length(run$Owned)
      
      # Create data frame for the run
      data.frame(
        Run = paste0("Run_", i),
        Time = 1:actual_length,  # Ensure correct time values
        Owned = run$Owned[1:actual_length],
        Shelter = run$Shelter[1:actual_length],
        Stray = run$Stray[1:actual_length],
        Feral = run$Feral[1:actual_length],
        Total = run$Total[1:actual_length],
        owned_neut_rate_A = owned_neut_rate_A,
        owned_neut_rate_K = owned_neut_rate_K
      )
    }))
    
    # Save combined data to a single CSV file
    write.csv(combined_data, file.path(export_path, "Combined_Runs.csv"), row.names = FALSE)
    
    # Generate individual plots for each run
    lapply(seq_len(length(rv$runs)), function(i) {  
      run <- rv$runs[[i]]
      
      # Skip empty or invalid runs
      if (is.null(run) || is.null(run$Owned) || length(run$Owned) == 0) {
        message(paste("Skipping Run", i, "- No data available."))
        return(NULL)
      }
      
      # Extract user-selected parameters
      params <- run$parameters
      owned_neut_rate_A <- ifelse(!is.null(params$owned_neut_rate_A), sprintf("%.0f%%", params$owned_neut_rate_A * 100), "N/A")
      owned_neut_rate_K <- ifelse(!is.null(params$owned_neut_rate_K), sprintf("%.0f%%", params$owned_neut_rate_K * 100), "N/A")
      
      # Format plot title
      plot_title <- paste0("Run ", i, " | Owned Adult Neutering: ", owned_neut_rate_A, 
                           " | Kitten Neutering: ", owned_neut_rate_K)
      
      # Convert to long format for ggplot
      actual_length <- length(run$Owned)
      plot_data <- data.frame(
        Time = 1:actual_length,  
        Population = rep(c("Total", "Owned", "Shelter", "Stray", "Feral"), each = actual_length), 
        Count = c(run$Total[1:actual_length], run$Owned[1:actual_length], run$Shelter[1:actual_length], 
                  run$Stray[1:actual_length], run$Feral[1:actual_length])
      ) %>%
        mutate(Population = factor(Population, levels = population_order))
      
      # Convert to long format and order factors
      plot_data <- plot_data %>%
        mutate(Population = factor(Population, levels = c("Total", "Owned", "Shelter", "Stray", "Feral")))
      
      # Generate the faceted plot
      faceted_plot <- ggplot(plot_data, aes(x = Time, y = Count, color = Population)) +
        geom_line(size = 1) +
        scale_color_manual(
          values = c("Total" = "black", "Owned" = "#7A1b72", "Shelter" = "#C928B2", 
                     "Stray" = "#7E3BE7", "Feral" = "#1A006B")
        ) +
        labs(
          title = plot_title,
          x = "Time (months)", 
          y = "Population Count"
        ) +
        theme_bw() +
        scale_x_continuous(limits = c(0, actual_length)) +
        facet_wrap(~ Population, scales = "free_y", ncol = 2)
      
      # Save the plot
      filename <- file.path(export_path, paste0("Run_", i, ".png"))
      ggsave(filename = filename, plot = faceted_plot, width = 12, height = 8)
    })
    
    # Generate the comparison plot, ensuring correct run length
    comparison_data <- combined_data %>%
      pivot_longer(cols = c(Total, Owned, Shelter, Stray, Feral), 
                   names_to = "Population", values_to = "Count") %>%
      mutate(
        Population = factor(Population, levels = population_order),  # Ensure correct order
        Run = paste0(Run, 
                     " (Adult Neutering: ", sprintf("%.0f%%", owned_neut_rate_A * 100), 
                     ", Kitten Neutering: ", sprintf("%.0f%%", owned_neut_rate_K * 100), ")")
      )
    
    # Generate the comparison plot
    comparison_plot <- ggplot(comparison_data, aes(x = Time, y = Count, color = Run, group = interaction(Run, Population))) +
      geom_line(size = 1) +
      facet_wrap(~ Population, scales = "free_y", ncol = 2) +  
      labs(
        title = "Comparison of All Runs",
        x = "Time (months)",
        y = "Population Count",
        color = "Run Details"  # Legend title
      ) +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +  
      theme(
        strip.text = element_text(size = 12, face = "bold"),  
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "right"
      )
    
    # Save the comparison plot
    comparison_filename <- file.path(export_path, "Comparison_All_Runs.png")
    ggsave(filename = comparison_filename, plot = comparison_plot, width = 12, height = 8)
    
    
    # ✅ Generate PDF Report
    pdf_report_path <- file.path(export_path, "Population_Report.pdf")
    rmd_content <- '
---
title: "Population Simulation Report"
author: "Cats Protection - catpopulationmodel@cats.org.uk"
date: "`r Sys.Date()`"
output: pdf_document
---

# Welcome to the Domestic Cat Population Model

The UK Cat Population model is a flexible tool for modelling, projecting and analysing cat populations over time. The model combines demographic information on unowned, stray, shelter and owned cats, including the interactions and transitions between these groups, across a range of ages and neuter statuses, into a unified framework enabling us to better understand population size, structure and changes through time.
The work that underpins this model was published in 2023 and highlighted that connectivity between cat subpopulations can have a big influence on population dynamics. The model enables us to consider all cats in our work, by recognising the interconnected nature of all cat types and highlighting the indirect impact interventions may have on all cats.
---

## **Key Caveats and Assumptions**
- This model aims to improve our understanding of domestic cat population dynamics at a population level, based on information from the individual level (birth, death, aging, relinquishment, straying, rehoming, neutering). It showcases a matrix population model that accounts for various factors such as a cats age, whether its living in a home, shelter, or freely, and its reproductive status, resulting in a model that describes 28 different states a cat can be in. The model also includes factors such as how cat populations change with the number of cats (density-dependence), seasonal variations in reproduction, and uncertainties in predictions due to data gaps. However, this model does not account for wider factors affecting the cat population, such as changes to the number of households with cats, importation or deportation of cats, indoor-lifestyle.
- Given the nature of the model, in addition to knowledge gaps in the data currently available to input into the model, there are limitations and caveats around the output that the model can produce. These are detailed below and will be available for reference on any report downloaded from this site.
- Like most matrix models, the model is female-specific therefore our neutering scenarios refer to neutering females only and we infer total population size assuming a 50:50 ratio. This is a justified approach when assessing neutering given a single unneutered male can fertilize multiple unneutered females. However, a female-only model prevents assessment of other factors, for example the impact of sex-specific demographic rates, such as males likely having lower survival rates or the wider implications of neutering male cats, which may influence survival rates, such as through reductions in conflict, competition, and infectious disease. These may be considered in future model development, given adequate data provision.
- The model cannot retrospectively analyse the cat population due to uncertainty around many parameters, particularly for the stray, feral and shelter cat populations of which we have no adequate current or historic data. Instead, the model was developed to replicate average conditions and in its current form is calibrated to provide a relatively stable environment to enable intervention testing. Therefore, we can explore potential trends but not absolute numbers. We hope to fill these data gaps going forward.
- For simplicity, we do not include uncertainty in these model scenarios and instead report single values. Therefore, it should be noted that the potential outcomes will be more variable than if we considered uncertainty in the underpinning parameters. Given the lack of data on unowned and shelter cats, which are reliant on our best approximations, we cannot currently model the impact of our services that target these specific subpopulations, i.e. TNR and rehoming. With time we hope to fill this data gap. Where data was not available on cats in a UK context, for example, when considering unowned-cat parameters, data collected in other international contexts was utilised. As UK demographics may differ to those taken from an international context, more robust estimators of demographics across different UK environments are needed.

---

## **Cat Categories**
Different terms are used for domestic cats, often with ambiguity in their definitions. Below are the categories used in this model:

- **Owned Cats**  
  - also known as pet or companion cats. Owned cats are socialized to humans and are closely associated with a household. Owned cats can come from the stray, feral (as kittens) or shelter subpopulations. They can also feed into the shelter population through relinquishment and can enter the stray population through abandonment or becoming lost.

- **Stray Cats**  
  - are generally considered to be lost, or previously owned, cats. They are typically socialised to humans and suitable for homing as an owned cat. They may be taken into a shelter for homing.

- **Feral Cats**  
  - are unsocialised to humans and are much less likely to enter shelter or owned cat population, compared to the stray subpopulation. The exception are feral kittens, who may be socialized and therefore potentially considered suitable to become owned cats.  

- **Shelter Cats**  
  - reside in animal shelters, also known as adoption centres or rescue centres. They can originate from feral (as kittens), stray and relinquished owned cat subpopulations. These cats then are homed to become an owned cat.

---

For further information, contact the **Cat Population Research Team** at:  
**catpopulationmodel@cats.org.uk**

# Comparison Plot
![](Comparison_All_Runs.png)

'
    
    rmd_file <- file.path(export_path, "report.Rmd")
    writeLines(rmd_content, con = rmd_file)
    
    render(rmd_file, output_format = "pdf_document", output_file = file.path(export_path, "Population_Report.pdf"))
    render(rmd_file, output_format = "word_document", output_file = file.path(export_path, "Population_Report.docx"))
    
    unlink(rmd_file)
    
    # Zip files
    oldwd <- getwd()
    setwd(export_path)
    zipfile <- file.path(temp_dir, "exported_runs.zip")
    zip(zipfile, files = list.files(".", full.names = FALSE))
    setwd(oldwd)
    
    # Make the zip file available for download
    output$downloadRuns <- downloadHandler(
      filename = function() {
        paste0("exported_runs_", Sys.Date(), ".zip")
      },
      content = function(file) {
        file.copy(zipfile, file)
      }
    )
    
    # Notify user
    showNotification("Export complete! Click download to retrieve the file.", type = "message")
  })
  
  
  # Hide the download button after it's clicked
  observeEvent(input$downloadRuns, {
    shinyjs::hide("downloadRuns")  # Hide button once download starts
  })
  
  
} ## Neutering Scenario Tab
  
{# File path for top text
  
  # Path to the editable text file
  mapTextPath <- "data/MapText.html"
  
  # Reactive value to store the uploaded shapefile data
  uploadedShapefile <- reactiveVal(NULL)
  
  # Load the initial HTML content for the top text
  loadHtmlFromFile <- function(filePath) {
    if (file.exists(filePath)) {
      paste(readLines(filePath, warn = FALSE), collapse = "\n")
    } else {
      "Default text for the mapping tool."
    }
  }
  
  # Reactive value for editable top text
  mapSavedHtml <- reactiveVal(loadHtmlFromFile(mapTextPath))
  
  # Render top text in the UI
  output$text_top_map <- renderUI({
    HTML(mapSavedHtml())
  })
  
  # Save the edited top text
  observeEvent(input$saveText_mapTop, {
    editorText(session, editorid = "editableText_mapTop", outputid = "savedText_mapTop")
    observe({
      req(input$savedText_mapTop)
      mapSavedHtml(input$savedText_mapTop)
      writeLines(input$savedText_mapTop, mapTextPath)
      showNotification("Text updated successfully!", type = "message")
    })
  })
  
  # Load the shapefile from a zip file
  observeEvent(input$loadShapefile, {
    req(input$shapefileUpload)
    
    zipPath <- input$shapefileUpload$datapath
    tempDir <- tempfile()
    dir.create(tempDir)
    
    # Unzip the uploaded file
    unzip(zipPath, exdir = tempDir)
    
    tryCatch({
      # Read the shapefile
      newShapefile <- st_read(tempDir, quiet = TRUE)
      uploadedShapefile(newShapefile)  # Store in reactive value
      
      file.copy(
        from = list.files(tempDir, full.names = TRUE),
        to = "data",
        overwrite = TRUE
      )
      
      showNotification("Shapefile loaded and updated successfully!", type = "message")
    }, error = function(e) {
      showNotification("Failed to load shapefile. Ensure it contains .shp, .shx, .dbf files.", type = "error")
    })
  })
  
  # Load the default shapefile (new one: UKCatDensitiesCounty.shp)
  shapefile <- st_read("data/UKCatDensitiesCounty.shp")
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    shapefileData <- uploadedShapefile() %||% shapefile  # Use uploaded shapefile if available
    
    # Define color palette for Densities_
    density_pal <- colorNumeric("YlOrRd", domain = shapefileData$Densities_, na.color = "gray")
    
    leaflet(data = shapefileData) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId = ~County,
        fillColor = ~density_pal(Densities_),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~County,  # Display only the County name
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "15px"
        )
      ) %>%
      addLegend(
        pal = density_pal,
        values = shapefileData$Densities_,
        title = "Density",
        opacity = 0.7,
        na.label = "Unknown"
      )
  })
  
  
  # Display clicked area information
  observeEvent(input$map_shape_click, {
    clicked <- input$map_shape_click
    if (!is.null(clicked)) {
      clicked_row <- shapefile[shapefile$County == clicked$id, ]
      output$clickedAreaInfo <- renderText({
        paste(
          "Selected County: ", clicked_row$County, "\n",
          "Density: ", ifelse(is.na(clicked_row$Densities_), "Unknown", clicked_row$Densities_)
        )
      })
    } else {
      output$clickedAreaInfo <- renderText("Click on a county to view details.")
    }
  })
  
  # Debugging outputs (optional)
  print(st_crs(shapefile))  # Ensure CRS is EPSG:4326 or similar
  print(colnames(shapefile))  # Ensure columns like County and Densities_ exist
  print(summary(shapefile$Densities_))  # Check Densities_ data validity
  
}
  
}

# Run the application 
shinyApp(ui = ui, server = server)
