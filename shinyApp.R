library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(dplyr)
library(readr)
library(ggplot2)

# figure out how to use updateSilderInput in order to max Presidential opinion at 100

header <- dashboardHeader(
  title = "Structure-X Forecasting Model",
  tags$li(class = "dropdown", circleButton("info", icon("info"), size="sm", status="info"))
)

sidebar <- dashboardSidebar(disable=TRUE)

body <- dashboardBody(useShinyalert(), fluidRow(
  column(8, align="center",
    box(width = NULL, plotOutput("plot")),
    box(width = NULL, 
        h3(textOutput("interval")),
        h3(textOutput("dem_win_pct"))
    )
  ),
  column(4,
    box(width = NULL, status = "warning",
      selectInput(inputId = "pres",
                  label = "Predict results based on Presidential...",
                  choices = c("Approval", "Disapproval", "Net Approval", "Approval Share")),
      sliderInput(inputId = "rdig",
                  label = "December 2017-June 2018 Real Disposible Income Growth",
                  value = 1.47, min = -1, max = 5, step=0.01),
      sliderInput(inputId = "pres_app",
                  label = "Trump June Approval:",
                  value = 42, min = 40, max = 60),
      sliderInput(inputId = "pres_dis",
                  label = "Trump June Disapproval",
                  value = 52, min = 40, max = 60)
    ),
    box(width= NULL, status = "warning", tags$b(textOutput("pres"))),
    box(width = NULL, status = "warning",
      checkboxInput(inputId = "structure_only", label="Use only Structural Model"),
      actionLink("s_only_info", "What is this?"),
      conditionalPanel(
        condition = "input.structure_only",
        checkboxInput(inputId = "shift_pred", label="Shift prediction towards out-party"),
        actionLink("shift_pred_info", "Why do this?")
      )
    )
  )
))

ui <- dashboardPage(header, sidebar, body)


structure <- read_csv("~/house_forecast_2018/data/structurex.csv") %>%
  mutate(pres_net = pres_app - pres_dis, pres_share = pres_app/(pres_app + pres_dis) * 100) %>%
  filter(year >= 1950)
structurex <- filter(structure, year >= 2006)

s_only_text <- "The Structure-X model is formed by combining structural factors and expert opinions, namely the opinions of Rothenberg Political Report. If you do not trust pundits, check this box"

server <- function(input, output) {
  observeEvent(input$s_only_info, {
    shinyalert("Use Only the Structural Model", 
               s_only_text, type = "info")
  })
  
  observeEvent(input$shift_pred_info, {
    shinyalert("Oops!", "Something went wrong.", type = "info")
  })
  
  vals <- reactiveValues()
  observe({
    vals$net <- input$pres_app - input$pres_dis
    vals$share <- round(input$pres_app/ (input$pres_dis + input$pres_app) * 100)
    vals$fit <- switch(input$pres,
                       "Approval" = lm(chiseats ~ midterm + rdi_growth + pres_app, data=structure),
                       "Disapproval" = lm(chiseats ~ midterm + rdi_growth + pres_dis, data=structure),
                       "Net Approval" = lm(chiseats ~ midterm + rdi_growth + pres_net, data=structure),
                       "Approval Share" = lm(chiseats ~ midterm + rdi_growth + pres_share, data=structure))
    params <- data.frame(rdi_growth=input$rdig, pres_dis = input$pres_dis, 
                         midterm = 1, pres_app=input$pres_app, 
                         pres_net=vals$net, pres_share = vals$share)
    interval <- predict.lm(vals$fit, params, interval = "prediction", se.fit = TRUE, level=0.9)
    if(input$structure_only) {
      vals$mean <- interval$fit[1]
      vals$lower <- ceiling(interval$fit[2])
      vals$upper <- floor(interval$fit[3])
      vals$df <- interval$df
      t.cutoff <- qt(0.95, df=vals$df)
      vals$se <- (interval$fit[1] - interval$fit[2])/t.cutoff
    } else {
      roth <- c(-31,-16,-64,17,2,46)
      fitted <- vals$fit$fitted.values[29:34]
      pred <- (fitted + roth)/2
      actual <- structurex$chiseats
      sxpred <- data.frame(pred, actual)
      if (input$shift_pred) {
        comb_fit <- lm(actual ~ offset(1 *pred), data=sxpred) %>% summary()
        vals$mean <- (interval$fit[1] - 59)/2 + comb_fit$coefficients[1,1]
      } else {
        comb_fit <- lm(actual ~ 0 + offset(1 *pred), data=sxpred) %>% summary()
        vals$mean <- (interval$fit[1] - 59)/2
      }
      vals$se <- comb_fit$sigma
      vals$df <- comb_fit$df[2]
      t.cutoff <- qt(0.95, df=vals$df)
      vals$lower <- ceiling(vals$mean - t.cutoff * vals$se)
      vals$upper <- floor(vals$mean + t.cutoff * vals$se)
    }
  })
  
  output$pres <- renderText({
    switch(input$pres,
           "Approval" = paste("Trump Approval: ", input$pres_app),
           "Disapproval" = paste("Trump Disapproval: ", input$pres_dis),
           "Net Approval" = paste("Trump Net Approval: ", vals$net),
           "Approval Share" = paste("Trump Approval Share: ", vals$share))
  })
  
  output$interval <- renderText({
    paste("90% confidence interval for change in Republican seats:", 
          vals$lower, " to ", vals$upper)
  })
  
  output$dem_win_pct <- renderText({
    paste("Probability of Democrats taking house:", 
          round(pt((-24-vals$mean)/vals$se,df=vals$df) * 100),
          "%")
  })
  
  output$plot <- renderPlot({
    set.seed(474747)
    chrseats <- rt(10000, vals$df) * vals$se + vals$mean
    dseats <- round(194 - chrseats)
    forecast_df <- data.frame(dseats)
    ggplot(aes(x=dseats, fill=(dseats > 217)), data=forecast_df) + 
      geom_histogram(binwidth=1) + xlab("Democratic seats in house") + 
      ylab("Simulations") + scale_x_continuous(limits=c(150,285)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
  
}

shinyApp(ui = ui, server = server)
