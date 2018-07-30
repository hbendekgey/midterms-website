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
info_text <- "For more information on these models, check out <a href=\"https://github.com/hbendekgey/house_forecast_2018\">my Github</a>"
npdi_desc_text <- "The National Poll District Info model predicts the national house popular vote based on generic ballot polls and who currently controls the presidency. From these structural conditions we expect Democrats to win 53.2% of the votes for representatives across the country, an environment very friendly to Democrats. The model then predicts how each seat will behave based on how they behaved in 2016, shifted to reflect the change in national environtment. Finally, it runs thousands of simulations: first picking a value for the national vote, then picking an outcome for each district based on that. Play around with some of the parameters and see how it affects the model!"

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
    # Structure-X ----
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
  ),
  # Bafumi, Erikson Wlezien ----
  tabItem(tabName="bafumi",
          column(8, align="center",
                 box(width = NULL, align="left", status="primary", npdi_desc_text),
                 box(width = NULL, plotOutput("npdi_plot")),
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
                 actionLink("open-adv-info", "Why would I want this?"),
                 sliderInput(inputId = "open_stdev",
                             label = "Open Seat Uncertainty",
                             value = 6.1, min = 2, max = 10, step=0.1),
                 actionLink("open-stdev-info", "Tell me more"),
                 sliderInput(inputId = "inc_stdev",
                             label = "Incumbent Seat Uncertainty",
                             value = 4.5, min = 2, max = 10, step=0.1),
                 actionLink("inc-stdev-info", "Tell me more"),
                 sliderInput(inputId = "nat_stdev",
                             label = "National Vote Uncertainty",
                             value = 1.8, min = 1, max = 3, step=0.1),
                 actionLink("nat-stdev-info", "Please, tell me more"),
                 tags$br(),
                 actionButton("npdi_reset", "Reset")
             )
          )
        ))
))

ui <- dashboardPage(header, sidebar, body)

# data ----
structure <- read_csv("~/house_forecast_2018/data/structurex.csv") %>%
  mutate(pres_net = pres_app - pres_dis, pres_share = pres_app/(pres_app + pres_dis) * 100) %>%
  filter(year >= 1950)
structurex <- filter(structure, year >= 2006)

seatchange <- read_csv("~/house_forecast_2018/data/seatchange.csv") %>%
  filter(midterm != 0)

genpolls <- read_csv("~/house_forecast_2018/data/GenericPolls.csv") 

model <- genpolls %>%
  filter(mtil >= 121, mtil <= 180) %>%
  filter(!is.na(dem), !is.na(rep)) %>%
  mutate(dem_share_poll = dempct - 50, is_rv = ifelse(is.na(type),TRUE, type == "RV" | type=="A")) %>%
  group_by(year) %>%
  summarise(genpoll = mean(dem_share_poll),pct_rv=mean(is_rv)) %>%
  merge(seatchange, by="year") %>%
  mutate(vote = 100 * nat_vote_dem/(nat_vote_dem + nat_vote_rep) - 50) %>%
  mutate(president_party = -1 * midterm, adj_genpoll = genpoll - 1.277 * pct_rv) %>%
  select(year, genpoll, vote, president_party, pct_rv, adj_genpoll)

cd2018data <- read_csv("~/house_forecast_2018/data/cd2018data.csv")
Dconcede <- cd2018data %>% filter(concede == 1) %>% nrow() # 41 races handed to Democrats
Rconcede <- cd2018data %>% filter(concede == -1) %>% nrow() # 27 races handed to Republicans
open18 <- cd2018data %>%
  filter(concede == 0, incumbent18 == 0 | grepl("PA",district))
inc18 <- cd2018data %>%
  filter(concede == 0, incumbent18 != 0 & !grepl("PA",district)) %>%
  mutate(frosh = incumbent18 * (incumbent16 != incumbent18))

# server ----
server <- function(input, output, session) {
  observeEvent(input$info, {
    shinyalert("2018 House Forecasting Models", 
               info_text, html = TRUE, type = "info")
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
      ylab("Probability") + scale_x_continuous(limits=c(lower.bound,upper.bound)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
  
  # NPDI 
  params18 <- data.frame(adj_genpoll=3.177, president_party=-1, genpoll=genpoll2018+1.277)
  dem_share16 <- 49.441-50
  
  observeEvent(input$npdi_reset, {
    updateSliderInput(session, "trump", value=0.42)
    updateSliderInput(session, "open_adv", value=0)
    updateSliderInput(session, "open_stdev", value=6.1)
    updateSliderInput(session, "inc_stdev", value=4.5)
    updateSliderInput(session, "nat_stdev", value=1.8)
  })
  
  sim <- reactive({
    set.seed(4747)
    fit <-  lm(vote ~ 0 + adj_genpoll + president_party, data=model)
    interval <- predict.lm(fit, params18, se.fit = TRUE, interval="prediction")
    t.cuttoff <- qt(0.975, df=interval$df)
    sdswing <- (interval$fit[1]-interval$fit[2])/t.cuttoff
    expswing <- interval$fit[1] - dem_share16
    openint <- mean(open18$dem16share) - mean(open18$pres16share)
    incint <- mean(inc18$dem16share) - mean(0.58 * inc18$dem16share + 0.42 * inc18$pres16share + 2.4 * inc18$frosh) 
    open18 <- open18 %>% 
      mutate(exp18 = openint + pres16share) 
    inc18 <- inc18 %>%
      mutate(exp18 = incint + 0.58 * dem16share + 0.42 * pres16share + 2.03 * frosh)
    niter <- 10000
    open <- matrix(NA,nrow=niter,ncol=nrow(open18))
    inc <- matrix(NA,nrow=niter,ncol=nrow(inc18))
    set.seed(4747)
    natswing <- rt(niter, df=interval$df) * sdswing + expswing
    for (i in 1:niter) {
      open[i,] <- rnorm(nrow(open18), mean = open18$exp18, sd=6.1) + natswing[i]
      inc[i,] <- rnorm(nrow(inc18), mean = inc18$exp18, sd=4.0) + natswing[i]
    }
    cbind(open, inc)
  })
  dseats <- reactive({
    rowSums(sim() > 0) + Dconcede
  })
  
  output$npdi_plot <- renderPlot({
    forecast_df <- data.frame(dseats=dseats())
    ggplot(aes(x=dseats, fill=(dseats > 217)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in house") + 
      ylab("Probability") + scale_x_continuous(limits=c(150,285)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
  output$npdi_interval <- renderText({
    bounds <- quantile(dseats(), c(0.05,0.95))
    paste("90% confidence interval for number of Democratic seats:", 
          bounds[1], " to ", bounds[2])
  })
  output$npdi_dem_win_pct <- renderText({
    paste("Probability of Democrats taking house:", 
          round(mean(dseats() > 217) * 100),
          "%")
  })
  output$npdi_est <- renderText({
    paste("Estimated number of Democratic seats:", 
          median(dseats()))
  })
  
}

shinyApp(ui = ui, server = server)
