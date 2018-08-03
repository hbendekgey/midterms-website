library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)

note <- "Note: both models above require information that isn't known until early September. These models might change over the next month."
compare_desc <- "Some forecasters, called structuralists, use historic voting trends based on the economy and national polls (such as presidential approval and generic ballot polling). This camp gives Democrats barely better than a coin flip's chance to retake the House. But special election results, district-by-district polls and expert race-by-race ratings predict a landslide. The difference is big. In 2006 and 2010, experts anticipated the wave elections (in favor of Democrats and Republicans, respectively) much better than structuralists. But in 2016, experts forecasted a wave for Democrats that never materialized. Who do you trust? \n To play around with more complicated models, use the menu bar on the left."
desc_text <- "The Structure-X model predicts the house elections using presidential approval ratings, real disposible income growth, and who holds the presidency. If the economy is doing well, or if the President is popular, the model expects the incumbent party to do better. These predictions are combined with expert opinion seat ratings (aka which seats are leaning or safe in each direction). What if you don't trust the experts? What if you think presidential disapproval is a better predictor than presidential approval? How would the election look different if Trump was more popular? Play around!"
npdi_desc_text <- "The National Poll District Info model predicts the national house popular vote based on generic ballot polls and who currently controls the presidency. From these structural conditions the model expects Democrats to win 53.2% of the votes for representatives across the country, an environment very friendly to Democrats. The model then predicts how each seat will behave based on how they behaved in 2016, shifted to reflect the change in national environtment. Finally, it runs thousands of simulations: first picking a value for the national vote, then picking an outcome for each district based on that. Play around with some of the parameters and see how it affects the model!"
gb_desc <- "This is a structural model which makes use of national conditions, or 'fundamentals.' This model makes its prediction based on Generic ballot polls, who controls the presidency, and how many seats each party currently has. Generic ballot polls are national polls that ask respondents if they want the Democrat or Republican to win in their district."
sit_desc <- "This is an expert model which makes use of seat ratings by Cook Political. Individual seats are rated as Solid, Likely, or Lean for either party, or as Tossups. This model makes its prediction based on how many seats for each party are 'in trouble,' or rated by experts as being a likely flip."

header <- dashboardHeader(
  title = "2018 Midterms",
  tags$li(class = "dropdown", circleButton("info", icon("info"), size="sm", status="info"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Seats-In-Trouble and Generic Ballot", tabName = "compare"),
    menuItem("National Polls and District Info", tabName = "bafumi"),
    menuItem("Structure-X", tabName = "sx")
  )
)

body <- dashboardBody(useShinyalert(), fluidRow(
  tabItems(
    tabItem(tabName="compare",
            box(width=12, align="left", status="primary", collapsible=TRUE, 
                tags$h2("Building a forecast: who do you trust?"),
                compare_desc),
            column(6, align="left",
                   box(width=NULL, status="warning", 
                       tags$h4("Generic Ballot Model"),
                       gb_desc),
                   box(width=NULL,
                       tabsetPanel(
                         tabPanel("House", 
                                  plotOutput("gbhouse"),
                                  textOutput("gb_win_pct"),
                                  textOutput("gb_est")),
                         tabPanel("Senate", 
                                  plotOutput("gbsenate"), 
                                  textOutput("gb_sen_win_pct"),
                                  textOutput("gb_sen_est"))
                       ))),
            column(6, align="left",
                   box(width=NULL, status="danger", 
                       tags$h4("Seats-In-Trouble Model"),
                       sit_desc),
                   box(width=NULL,
                       tabsetPanel(
                         tabPanel("House", 
                                  plotOutput("sithouse"),
                                  textOutput("sit_win_pct"),
                                  textOutput("sit_est")),
                         tabPanel("Senate", 
                                  plotOutput("sitsenate"),
                                  textOutput("sit_sen_win_pct"),
                                  textOutput("sit_sen_est"))
                       ))),
            column(width=8, offset=2, align="center", box(width=NULL, status="primary", note))
    ),
    # Bafumi, Erikson Wlezien ----
    tabItem(tabName="bafumi",
            column(8, align="center",
                   box(width = NULL, align="left", status="primary", collapsible=TRUE, npdi_desc_text),
                   box(width = NULL, 
                       tabsetPanel(
                         tabPanel("Histogram of House Seats", plotOutput("npdi_plot")),
                         tabPanel("District-Level Data Table", dataTableOutput("district_info")),
                         tabPanel("Tossup Districts", dataTableOutput("tossups"))
                       )
                   ),
                   box(width = NULL, 
                       h4(textOutput("npdi_interval")),
                       h4(textOutput("npdi_est")),
                       h4(textOutput("npdi_dem_win_pct"))
                   )
            ),
            column(4,
                   box(width = NULL, status = "warning",
                       tags$h4(tags$b("Fix up the model your way")),
                       sliderInput(inputId = "trump",
                                   label = "How much is the GOP the Party of Trump?",
                                   value = 0.42, min = 0, max = 1, step=0.01),
                       actionLink("trump_info", "What does this mean?"),
                       sliderInput(inputId = "open_adv",
                                   label = "Expected Democratic overperformance in open seats",
                                   value = 0, min = -1, max = 3, step=0.1),
                       actionLink("open_adv_info", "Why would I want this?"),
                       sliderInput(inputId = "open_stdev",
                                   label = "Open Seat Uncertainty",
                                   value = 6.1, min = 2, max = 10, step=0.1),
                       actionLink("open_stdev_info", "Tell me more"),
                       sliderInput(inputId = "inc_stdev",
                                   label = "Incumbent Seat Uncertainty",
                                   value = 4.5, min = 2, max = 10, step=0.1),
                       actionLink("inc_stdev_info", "Tell me more"),
                       sliderInput(inputId = "nat_stdev",
                                   label = "National Vote Uncertainty",
                                   value = 1.8, min = 1, max = 3, step=0.1),
                       actionLink("nat_stdev_info", "Please, tell me more"),
                       tags$br(),
                       actionButton("npdi_reset", "Reset")
                   )
            )
    ),
    # Structure-X ----
    tabItem(tabName="sx",
            column(8, align="center",
                   box(width = NULL, align="left", status="primary", collapsible=TRUE, desc_text),
                   box(width = NULL, plotOutput("plot")),
                   box(width = NULL, 
                       h4(textOutput("interval")),
                       h4(textOutput("est")),
                       h4(textOutput("dem_win_pct"))
                   )
            ),
            column(4,
                   box(width = NULL, status = "warning",
                       tags$h4(tags$b("1. Fix up the model your way")),
                       selectInput(inputId = "pres",
                                   label = "Predict results based on presidential...",
                                   choices = c("Approval", "Disapproval", "Net Approval", "Approval Share")),
                       checkboxInput(inputId = "structure_only", label="Use only Structural Model"),
                       actionLink("s_only_info", "What is this?")
                   ),
                   box(width = NULL, status = "warning",
                       tags$h4(tags$b("2. What if conditions were different?")),
                       
                       sliderInput(inputId = "rdig",
                                   label = "December '17-June '18 Real Disposible Income Growth",
                                   value = 1.52, min = -1, max = 5, step=0.01),
                       sliderInput(inputId = "pres_app",
                                   label = "Trump June Approval:",
                                   value = 42, min = 30, max = 70),
                       sliderInput(inputId = "pres_dis",
                                   label = "Trump June Disapproval",
                                   value = 52, min = 30, max = 70),
                       actionButton("reset", "Reset")
                   )
            )
    ))
))

dashboardPage(header, sidebar, body)
