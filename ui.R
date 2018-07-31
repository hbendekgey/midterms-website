library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)

note <- "Note: both models above require information that isn't known until early September. These models might change over the next month."
compare_desc <- "Some forecasters, called structuralists, use historic voting trends based on the economy and national polls (such as presidential approval and generic ballot polling). This camp gives Democrats barely better than a coin flip's chance to retake the House. But special election results, district-by-district polls and expert race-by-race ratings predict a landslide. The difference is big. In 2006 and 2010, experts anticipated the waves much better than structuralists. But in 2014 and 2016, experts were much further off. Who do you trust? \n To play around with more complicated models, use the menu bar on the left."
s_only_text <- "Half of the prediction comes from Rothenberg's rating of races as e.g. Safe-D or Tossup or Lean-R. These have historically been very accurate, but did not do well in 2016. If you do not trust pundits, check this box."
desc_text <- "The Structure-X model predicts the house elections using Presidential approval ratings, real disposible income growth, and who holds the Presidency. If the economy is doing well, or if the President is popular, we expect the incumbent party to do better. These predictions are combined with expert opinion seat ratings (aka which seats are leaning or safe in each direction). What if you don't trust the experts? What if you think Presidential disapproval is a better predictor than Presidential approval? How would the election look different if Trump was more popular? Play around!"
shift_pred_text <- "For the past 6 elections (as long as Rothenberg has been rating races) this model has predicted outcomes more favorable to the Presidential party every election, on average by about 6 seats. With so few data points we don't have statistical significance, (p=0.06) and it's dangerous to overfit the data, so you should only check this if you think there's a systematic reason this model might overestimate incumbent performance."
info_text <- "For more information on these models, check out <a href=\"https://github.com/hbendekgey/house_forecast_2018\">my Github</a>"
npdi_desc_text <- "The National Poll District Info model predicts the national house popular vote based on generic ballot polls and who currently controls the presidency. From these structural conditions we expect Democrats to win 53.2% of the votes for representatives across the country, an environment very friendly to Democrats. The model then predicts how each seat will behave based on how they behaved in 2016, shifted to reflect the change in national environtment. Finally, it runs thousands of simulations: first picking a value for the national vote, then picking an outcome for each district based on that. Play around with some of the parameters and see how it affects the model!"
trump_info_text <- "To make predictions for seats where incumbents are runnning, we take a combination of its house vote and presidential vote in 2016. This slider represents the percentage of the prediction that comes from the presidential margin. Predicting the 2010 elections from 2008 suggest the value should be 42%, while predicting the 2014 elections from 2012 suggest it should be 12%. If you expect Republicans to do best in districts Trump did well in, increase this value. If you expect Republicans to do best in districts where incumbents did well in 2016, decrease this."
open_adv_info_dec <- "This model assumes seats with incumbents running for relection and those without will swing (on average) by the same amount from 2016 to 2018. However, the large number of Republicans retiring (over double the number of Democrats) indicate that compared to their national margin, Democrats might overperform in open seats compared to how they performed in 2016 (and therefore underperform in incumbent seats). A simple calculation using incumbency advantage implies this advantage should be 1 percentage point (making the corresponding disadvantage 0.2 pct, because there are more incumbent seats than open ones). Play around and see how it changes predictions."
open_stdev_desc <- "We know that our predictions aren't perfect, but we want to know by how much. Based on past elections, we expect the real results in seats without incumbents to be centered around our prediction with a standard deviation of 6.1. However, we have very few data points, so if you very much trust (or don't trust) this model, change this value."
inc_stdev_desc <- "We know that our predictions aren't perfect, but we want to know by how much. Based on past elections, we expect the real results in seats with incumbents running to be centered around our prediction with a standard deviation of 4.5. However, we have very few data points, so if you very much trust (or don't trust) this model, change this value."
nat_stdev_desc <- "We know that our predictions aren't perfect, but we want to know by how much. Based on past elections, we expect the true percentage of the national house votes that goes to the Democrats to be centered around our prediction with a standard deviation of 1.8. This relies on regression from elections since 1950, so if you believe this election to be atypical and hard to predict from precedent, increase this value. If you think this election will follow historic trends, decrease it."
gb_desc <- "This is a structural model which makes use of national conditions, or 'fundamentals.' This model makes its prediction based on Generic ballot polls, who controls the Presidency, and how many seats each party currently has. Generic ballot polls are national polls that ask respondents if they want the Democrat or Republican to win in their district."
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
                                   label = "Predict results based on Presidential...",
                                   choices = c("Approval", "Disapproval", "Net Approval", "Approval Share")),
                       checkboxInput(inputId = "structure_only", label="Use only Structural Model"),
                       actionLink("s_only_info", "What is this?"),
                       conditionalPanel(
                         condition = "!input.structure_only",
                         checkboxInput(inputId = "shift_pred", label="Shift prediction towards out-party"),
                         actionLink("shift_pred_info", "Why do this?")
                       )
                   ),
                   box(width = NULL, status = "warning",
                       tags$h4(tags$b("2. What if conditions were different?")),
                       
                       sliderInput(inputId = "rdig",
                                   label = "December '17-June '18 Real Disposible Income Growth",
                                   value = 1.47, min = -1, max = 5, step=0.01),
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
