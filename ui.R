library(shiny)
library(flexdashboard)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("superhero"),
    headerPanel("Breast Cancer Risk Assessment"),
    fluidRow(
    sidebarPanel(
        fluidRow(column(10, h2('Questionaire')),
                 column(2, tags$br(), 
                        imageOutput("image", height = 25, width = 25,
                                    click = clickOpts(id = "image_click")),
                        tags$br(), tags$br())),
        
        numericInput('age',"Current Age?", 35, min=35, max=85, step = 1),
        
        selectInput('menstruation',"Age of first menstruation?",
                    choices = list("Less than 12 years old" = 11,
                                   "12 through 13 years old" = 12,
                                   "Greater than 13 years old" = 14,
                                   "Unknown" = 99), selected = 99),
        
        selectInput('first_birth',"Age at first birth?",
                    choices = list("No births" = 98,
                                   "Less than 20 years old"= 19,
                                   "20 through 24 years old" = 20,
                                   "25 through 29 years old" = 25,
                                   "30 years old and greater"= 30,
                                   "Unknown" = 99), selected = 99),
        
        selectInput('relatives',"Number of 1st degree relatives that have had breast cancer?",
                    choices = list("Unknown" = 99,
                                    "0 relatives" = 0,
                                   "1 relatives" = 1,
                                   "2 or more relatives" = 2), selected = 99),
        
        selectInput('biopsies', "Number of breast biopsies?",
                    choices = list("Unknown" = 99,
                                   "no biopsies" = 0,
                                   "1 biopsies" = 1,
                                   "2 or more biopsies" = 2), selected = 99),
        
        conditionalPanel(
            condition = "input.biopsies == 1 || input.biopsies == 2",
            selectInput("hyperplasia", "Did the biopsy display hyperplasia?",
                        choices = list("No" = 0,
                                       "Yes" = 1,
                                       "Unknown" = 99), selected = 99)),
        
        selectInput('race',"Race/Ethnicity?",
                    choices = list("White" = 1,
                                   "African-American" = 2,
                                   "Hispanic-American" = 3,
                                   "Other (Native American and unknown race" = 4,
                                   "Chinese-American" = 6,
                                   "Japanese-American" = 7,
                                   "Filipino-American" = 8,
                                   "Hawaiian-Amerian" = 9,
                                   "Other Pacific Islander" = 10,
                                   "Other Asian" = 11), selected = 1),
        
        actionButton("do","Calculate Risk", class = "btn-primary")
        
    ),
    
    mainPanel(
        h3('Your Results'),
        tags$hr(),
        span(textOutput("five_yr_title"), style="font-size: 20px"),
        textOutput("five_yr_text"),
        tags$br(),
        span(textOutput("lifetime_title"), style="font-size: 20px"),
        textOutput("lifetime_text"),
        tags$hr(),
        fluidRow(
            column(7
                   , fluidRow(
                       column(12, span(textOutput("advice_title"), style="font-size: 20px"),
                              tags$hr(width = "50%", align = "left"))
                   )
                   , fluidRow(
                       column(12, textOutput("advice_text1"), tags$br(),
                              textOutput("advice_text2"))
                   )
            )
            , column(5, tags$br(), htmlOutput("short_five"), htmlOutput("short_life"), tags$br(), gaugeOutput("plt1"))
        )
    )),
    
    fluidRow(
        tags$hr(),
        tags$div(class = "footer",("© 2018 - "), style = "bottom:0",
                 tags$a(
                     href="https://www.linkedin.com/in/trent-parkinson-920836120/",
                     target="_blank",
                     "Trent Parkinson"),
                 align = "center")
        
    )
    
)

)