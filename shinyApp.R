library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(tidyverse)

# allow for analysis of past years

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

ui <- dashboardPage(header, sidebar, body)

# data ----
structure <- read_csv("~/house_forecast_2018/data/structurex.csv") %>%
  mutate(pres_net = pres_app - pres_dis, pres_share = pres_app/(pres_app + pres_dis) * 100) %>%
  filter(year >= 1950)
structurex <- filter(structure, year >= 2006)

seatchange <- read_csv("~/house_forecast_2018/data/seatchange.csv") %>%
  filter(midterm != 0)
model <- read_csv("~/house_forecast_2018/data/GenericPolls.csv")  %>%
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
Dconcede <- nrow(filter(cd2018data, concede == 1))
Rconcede <- nrow(filter(cd2018data, concede == -1))
open18 <- filter(cd2018data, concede == 0, incumbent18 == 0 | grepl("PA",district))
inc18 <- filter(cd2018data, concede == 0, incumbent18 != 0 & !grepl("PA",district)) %>%
  mutate(frosh = incumbent18 * (incumbent16 != incumbent18))
niter <- 20000

# server ----
server <- function(input, output, session) {
  observeEvent(input$info, {
    shinyalert("2018 House Forecasting Models", 
               info_text, html = TRUE, type = "info")
  })
  
  gbmean <- 33.2
  gbsd <- 17
  output$gbhouse <- renderPlot({
    lower.bound <- 150
    upper.bound <- 285
    dist_df <- data.frame(dseats=c(lower.bound:upper.bound)) %>%
      mutate(prob = 10000 * (pnorm(dseats-193.5,mean=gbmean,sd=gbsd) - 
                               pnorm(dseats-194.5,mean=gbmean,sd=gbsd)))
    forecast_df <- data.frame(dseats=rep(dist_df$dseats, dist_df$prob))
    ggplot(aes(x=dseats, fill=(dseats > 217)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in House") + 
      ylab("Probability") + scale_x_continuous(limits=c(lower.bound,upper.bound)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
  output$gb_win_pct <- renderText({
    paste("Probability of Democrats taking house:", 
          round(100 * (1 - pnorm(23.5,mean=gbmean,sd=gbsd))),
          "%")
  })
  output$gb_est <- renderText({
    paste("Estimated number of Democratic seats:", 
          194 + round(gbmean))
  })
  
  gbmeansen <- -2.02
  gbsdsen <- 2.71
  output$gbsenate <- renderPlot({
    lower.bound <- 23
    upper.bound <- 58
    dist_df <- data.frame(dseats=c(lower.bound:upper.bound)) %>%
      mutate(prob = 10000 * (pnorm(dseats-48.5,mean=gbmeansen,sd=gbsdsen) - 
                               pnorm(dseats-49.5,mean=gbmeansen,sd=gbsdsen)))
    forecast_df <- data.frame(dseats=rep(dist_df$dseats, dist_df$prob))
    ggplot(aes(x=dseats, fill=(dseats > 50)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in senate") + 
      ylab("Probability") + scale_x_continuous(limits=c(lower.bound,upper.bound)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican Senate", "Democratic Senate"))
  })
  output$gb_sen_win_pct <- renderText({
    paste("Probability of Democrats taking senate:", 
          round(100 * (1 - pnorm(1.5,mean=gbmeansen,sd=gbsdsen))),
          "%")
  })
  output$gb_sen_est <- renderText({
    paste("Estimated number of Democratic seats:", 
          49 + round(gbmeansen))
  })
  
  
  sitmean <- 68.8
  sitsd <- 17
  output$sithouse <- renderPlot({
    lower.bound <- 150
    upper.bound <- 285
    dist_df <- data.frame(dseats=c(lower.bound:upper.bound)) %>%
      mutate(prob = 10000 * (pnorm(dseats-193.5,mean=sitmean,sd=sitsd) - 
                               pnorm(dseats-194.5,mean=sitmean,sd=sitsd)))
    forecast_df <- data.frame(dseats=rep(dist_df$dseats, dist_df$prob))
    ggplot(aes(x=dseats, fill=(dseats > 217)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in House") + 
      ylab("Probability") + scale_x_continuous(limits=c(lower.bound,upper.bound)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican House", "Democratic House"))
  })
  output$sit_win_pct <- renderText({
    paste("Probability of Democrats taking house:", 
          round(100 * (1 - pnorm(23.5,mean=sitmean,sd=sitsd))),
          "%")
  })
  output$sit_est <- renderText({
    paste("Estimated number of Democratic seats:", 
          194 + round(sitmean))
  })
  
  sitmeansen <- -4.495
  sitsdsen <- 3.54
  output$sitsenate <- renderPlot({
    lower.bound <- 23
    upper.bound <- 58
    dist_df <- data.frame(dseats=c(lower.bound:upper.bound)) %>%
      mutate(prob = 10000 * (pnorm(dseats-48.5,mean=sitmeansen,sd=sitsdsen) - 
                               pnorm(dseats-49.5,mean=sitmeansen,sd=sitsdsen)))
    forecast_df <- data.frame(dseats=rep(dist_df$dseats, dist_df$prob))
    ggplot(aes(x=dseats, fill=(dseats > 50)), data=forecast_df) + 
      geom_histogram(binwidth=1, aes(y=..count../sum(..count..))) + xlab("Democratic seats in senate") + 
      ylab("Probability") + scale_x_continuous(limits=c(lower.bound,upper.bound)) + 
      scale_fill_discrete(name=element_blank(),
                          breaks=c("FALSE", "TRUE"),
                          labels=c("Republican Senate", "Democratic Senate"))
  })
  output$sit_sen_win_pct <- renderText({
    paste("Probability of Democrats taking senate:", 
          round(100 * (1 - pnorm(1.5,mean=sitmeansen,sd=sitsdsen))),
          "%")
  })
  output$sit_sen_est <- renderText({
    paste("Estimated number of Democratic seats:", 
          49 + round(sitmeansen))
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
  
  ###NPDI 
  params18 <- data.frame(adj_genpoll=3.177, president_party=-1)
  dem_share16 <- 49.441-50
  
  observeEvent(input$npdi_reset, {
    updateSliderInput(session, "trump", value=0.42)
    updateSliderInput(session, "open_adv", value=0)
    updateSliderInput(session, "open_stdev", value=6.1)
    updateSliderInput(session, "inc_stdev", value=4.5)
    updateSliderInput(session, "nat_stdev", value=1.8)
  })
  observeEvent(input$trump_info, {
    shinyalert("Percent of Prediction Coming from Trump 2016 Margin", 
               trump_info_text, type = "info")
  })
  observeEvent(input$open_adv_info, {
    shinyalert("Considering Republican Retirements", 
               open_adv_info_dec, type = "info")
  })
  observeEvent(input$open_stdev_info, {
    shinyalert("Open Seat Standard Deviation", 
               open_stdev_desc, type = "info")
  })
  observeEvent(input$inc_stdev_info, {
    shinyalert("Incumbent Seat Standard Deviation", 
               inc_stdev_desc, type = "info")
  })
  observeEvent(input$nat_stdev_info, {
    shinyalert("National Vote Standard Deviation", 
               nat_stdev_desc, type = "info")
  })
  
  # NPDI regression simulation and data
  sim <- reactive({
    set.seed(4747)
    fit <-  lm(vote ~ 0 + adj_genpoll + president_party, data=model)
    interval <- predict.lm(fit, params18, se.fit = TRUE)
    expswing <- interval$fit - dem_share16
    openint <- mean(open18$dem16share) - mean(open18$pres16share) + input$open_adv
    incint <- mean(inc18$dem16share) - mean((1 - input$trump) * inc18$dem16share + input$trump * inc18$pres16share + 2.4 * inc18$frosh) - input$open_adv * nrow(open18)/nrow(inc18)
    open18 <- mutate(open18, exp18 = openint + pres16share) 
    inc18 <- mutate(inc18, exp18 = incint + (1 - input$trump) * dem16share + input$trump * pres16share + 2.4 * frosh)
    open <- matrix(NA,nrow=niter,ncol=nrow(open18))
    inc <- matrix(NA,nrow=niter,ncol=nrow(inc18))
    natswing <- rt(niter, df=interval$df) * input$nat_stdev + expswing
    for (i in 1:niter) {
      open[i,] <- rnorm(nrow(open18), mean = open18$exp18, sd=input$open_stdev) + natswing[i]
      inc[i,] <- rnorm(nrow(inc18), mean = inc18$exp18, sd=input$inc_stdev) + natswing[i]
    }
    cbind(open, inc)
  })
  dseats <- reactive({
    rowSums(sim() > 0) + Dconcede
  })
  npdi_data <- reactive({
    sim_result <- sim()
    dem_win_pct <- round(colSums(sim_result > 0)/nrow(sim_result) * 100)
    dem_share <- signif(colMeans(sim_result) + 50, digits=3)
    dem_share[dem_share > 100] = 100
    conceded <- cd2018data %>% 
      filter(concede == 1 | concede == -1) %>% 
      mutate(dem_share = NA, dem_win_pct = ifelse(concede==1,100,0)) %>% 
      select(district, dem_share, dem_win_pct)
    district <- c(open18$district, inc18$district, conceded$district)
    dem_share <- c(dem_share, conceded$dem_share)
    dem_win_pct <- c(dem_win_pct, conceded$dem_win_pct)
    data.frame(district, dem_share, dem_win_pct)[order(district),]
  })
  
  # NPDI output
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
  output$district_info <- renderDataTable({npdi_data()}, options = list(dom = 'ftpr'))
  output$tossups <- renderDataTable({
    filter(npdi_data(), dem_win_pct > 40, dem_win_pct < 60)
  }, options = list(dom = 'ftpr'))
  
}

shinyApp(ui = ui, server = server)
