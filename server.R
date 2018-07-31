library(shiny)
library(shinyalert)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# data ----
structure <- read_csv("data/structurex.csv") %>%
  mutate(pres_net = pres_app - pres_dis, pres_share = pres_app/(pres_app + pres_dis) * 100) %>%
  filter(year >= 1950)
structurex <- filter(structure, year >= 2006)

seatchange <- read_csv("data/seatchange.csv") %>%
  filter(midterm != 0)
model <- read_csv("data/GenericPolls.csv")  %>%
  filter(mtil >= 121, mtil <= 180) %>%
  filter(!is.na(dem), !is.na(rep)) %>%
  mutate(dem_share_poll = dempct - 50, is_rv = ifelse(is.na(type),TRUE, type == "RV" | type=="A")) %>%
  group_by(year) %>%
  summarise(genpoll = mean(dem_share_poll),pct_rv=mean(is_rv)) %>%
  merge(seatchange, by="year") %>%
  mutate(vote = 100 * nat_vote_dem/(nat_vote_dem + nat_vote_rep) - 50) %>%
  mutate(president_party = -1 * midterm, adj_genpoll = genpoll - 1.277 * pct_rv) %>%
  select(year, genpoll, vote, president_party, pct_rv, adj_genpoll)

cd2018data <- read_csv("data/cd2018data.csv")
Dconcede <- nrow(filter(cd2018data, concede == 1))
Rconcede <- nrow(filter(cd2018data, concede == -1))
open18 <- filter(cd2018data, concede == 0, incumbent18 == 0 | grepl("PA",district))
inc18 <- filter(cd2018data, concede == 0, incumbent18 != 0 & !grepl("PA",district)) %>%
  mutate(frosh = incumbent18 * (incumbent16 != incumbent18))
niter <- 20000

# server ----
function(input, output, session) {
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