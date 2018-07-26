library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(dplyr)
library(readr)
library(ggplot2)

# add other models
# allow for analysis of past years

s_only_text <- "Half of the prediction comes from Rothenberg's rating of races as e.g. Safe-D or Tossup or Lean-R. These have historically been very accurate, but did not do well in 2016. If you do not trust pundits, check this box."
desc_text <- "The Structure-X model predicts the house elections using Presidential approval ratings, real disposible income growth, and who holds the Presidency. If the economy is doing well, or if the President is popular, we expect the incumbent party to do better. These predictions are combined with expert opinion seat ratings (aka which seats are leaning or safe in each direction). What if you don't trust the experts? What if you think Presidential disapproval is a better predictor than Presidential approval? How would the election look different if Trump was more popular? Play around!"
shift_pred_text <- "For the past 6 elections (as long as Rothenberg has been rating races) this model has predicted outcomes more favorable to the Presidential party every election, on average by about 6 seats. With so few data points we don't have statistical significance, (p=0.06) and it's dangerous to overfit the data, so you should only check this if you think there's a systematic reason this model might overestimate incumbent performance."
info_text <- "For more information on these models, check out https://github.com/hbendekgey/house_forecast_2018"

header <- dashboardHeader(
  title = "2018 House",
  tags$li(class = "dropdown", circleButton("info", icon("info"), size="sm", status="info"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Structure-X", tabName = "sx"),
    menuItem("National Polls and District Info", tabName = "bafumi")
  )
)

body <- dashboardBody(useShinyalert(), fluidRow(
  tabItems(
    # Structure-X
    tabItem(tabName="sx",
    column(8, align="center",
      box(width = NULL, align="left", status="primary", desc_text),
      box(width = NULL, plotOutput("plot")),
      box(width = NULL, 
          h4(textOutput("interval")),
          h4(textOutput("est")),
          h4(textOutput("dem_win_pct"))
      )
    ),
    column(4,
      box(width = NULL, status = "warning",
        tags$h4(tags$b("Fix up the model your way")),
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
  ),
  # Bafumi, Erikson Wlezien
  tabItem(tabName="bafumi", fluidPage()))
))

ui <- dashboardPage(header, sidebar, body)


structure <- read_csv("~/house_forecast_2018/data/structurex.csv") %>%
  mutate(pres_net = pres_app - pres_dis, pres_share = pres_app/(pres_app + pres_dis) * 100) %>%
  filter(year >= 1950)
structurex <- filter(structure, year >= 2006)

server <- function(input, output, session) {
  observeEvent(input$info, {
    shinyalert("2018 House Forecasting Models", 
               info_text, type = "info")
  })
  ###Structure-X
  
  # sx info buttons and link
  observeEvent(input$s_only_info, {
    shinyalert("Use Only the Structural Model", 
               s_only_text, type = "info")
  })
  observeEvent(input$shift_pred_info, {
    shinyalert("Shift Prediction Towards Out-Party", 
               shift_pred_text, type = "info")
  })
  
  # reset sliders and make sure they don't add to > 100
  observeEvent(input$reset, {
    updateSliderInput(session, "rdig", value=1.47)
    updateSliderInput(session, "pres_app", value=42)
    updateSliderInput(session, "pres_dis", value=52)
  })
  observeEvent(input$pres_app, {
    if (input$pres_app + input$pres_dis > 100) {
      updateSliderInput(session, "pres_dis", value=100-input$pres_app)
    }
  })
  observeEvent(input$pres_dis, {
    if (input$pres_app + input$pres_dis > 100) {
      updateSliderInput(session, "pres_app", value=100-input$pres_dis)
    }
  })
  
  # sx regression values
  sxvals <- reactiveValues()
  observe({
    sxvals$net <- input$pres_app - input$pres_dis
    sxvals$share <- round(input$pres_app/ (input$pres_dis + input$pres_app) * 100)
    sxvals$fit <- switch(input$pres,
                       "Approval" = lm(chiseats ~ midterm + rdi_growth + pres_app, data=structure),
                       "Disapproval" = lm(chiseats ~ midterm + rdi_growth + pres_dis, data=structure),
                       "Net Approval" = lm(chiseats ~ midterm + rdi_growth + pres_net, data=structure),
                       "Approval Share" = lm(chiseats ~ midterm + rdi_growth + pres_share, data=structure))
    params <- data.frame(rdi_growth=input$rdig, pres_dis = input$pres_dis, 
                         midterm = 1, pres_app=input$pres_app, 
                         pres_net=sxvals$net, pres_share = sxvals$share)
    interval <- predict.lm(sxvals$fit, params, interval = "prediction", se.fit = TRUE, level=0.9)
    if(input$structure_only) {
      sxvals$mean <- interval$fit[1]
      sxvals$lower <- ceiling(interval$fit[2])
      sxvals$upper <- floor(interval$fit[3])
      sxvals$df <- interval$df
      t.cutoff <- qt(0.95, df=sxvals$df)
      sxvals$se <- (interval$fit[1] - interval$fit[2])/t.cutoff
    } else {
      roth <- c(-31,-16,-64,17,2,46)
      fitted <- sxvals$fit$fitted.values[29:34]
      pred <- (fitted + roth)/2
      actual <- structurex$chiseats
      sxpred <- data.frame(pred, actual)
      if (input$shift_pred) {
        comb_fit <- lm(actual ~ offset(1 *pred), data=sxpred) %>% summary()
        sxvals$mean <- (interval$fit[1] - 59)/2 + comb_fit$coefficients[1,1]
      } else {
        comb_fit <- lm(actual ~ 0 + offset(1 *pred), data=sxpred) %>% summary()
        sxvals$mean <- (interval$fit[1] - 59)/2
      }
      sxvals$se <- comb_fit$sigma
      sxvals$df <- comb_fit$df[2]
      t.cutoff <- qt(0.95, df=sxvals$df)
      sxvals$lower <- ceiling(sxvals$mean - t.cutoff * sxvals$se)
      sxvals$upper <- floor(sxvals$mean + t.cutoff * sxvals$se)
    }
  })
  
  # sx output
  output$pres <- renderText({
    switch(input$pres,
           "Approval" = paste("Trump Approval: ", input$pres_app),
           "Disapproval" = paste("Trump Disapproval: ", input$pres_dis),
           "Net Approval" = paste("Trump Net Approval: ", sxvals$net),
           "Approval Share" = paste("Trump Approval Share: ", sxvals$share))
  })
  output$interval <- renderText({
    paste("90% confidence interval for number of Democratic seats:", 
          194 - sxvals$upper, " to ", 194 - sxvals$lower)
  })
  output$est <- renderText({
    paste("Estimated number of Democratic seats:", 
          194 - round(sxvals$mean))
  })
  output$dem_win_pct <- renderText({
    paste("Probability of Democrats taking house:", 
          round(pt((-23.5-sxvals$mean)/sxvals$se,df=sxvals$df) * 100),
          "%")
  })
  output$plot <- renderPlot({
    lower.bound <- 150
    upper.bound <- 285
    dist_df <- data.frame(dseats=c(lower.bound:upper.bound)) %>%
      mutate(prob = 10000 * (pt((194-dseats+0.5-sxvals$mean)/sxvals$se,df=sxvals$df) - 
                    pt((194-dseats-0.5-sxvals$mean)/sxvals$se,df=sxvals$df)))
    forecast_df <- data.frame(dseats=rep(dist_df$dseats, dist_df$prob))
    ggplot(aes(x=dseats, fill=(dseats > 217)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in house") + 
      ylab("Probability") + scale_x_continuous(limits=c(150,285)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
}

shinyApp(ui = ui, server = server)
