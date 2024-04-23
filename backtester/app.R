#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(ggplot2)
library(tidyverse)
library(TTR)
library(plotly)
library(rvest)

my_spy_hold <- function(date=NA,filterDate=FALSE){
  if(filterDate){
    prices <- read_csv(str_glue("../data/nasdaq_prices/etfs/SPY.csv")) %>% 
      filter(Date >= date) %>% 
      select(Date,Close)
  }
  else{
    prices <- read_csv(str_glue("../data/nasdaq_prices/etfs/SPY.csv")) %>% 
      select(Date,Close)
  }
  i <- 2
  accountValVec <- rep(0,nrow(prices))
  accountValue <- 1000
  accountValVec[1] <- 1000
  prevPrice <- prices$Close[1]
  while(i <= nrow(prices)){
    curPrice <- prices$Close[i]
    accountValue <- 1000 * (curPrice/prevPrice)
    accountValVec[i] <- accountValue
  }
  return(data.frame(Date = prices$Date,AccountValue = accountValVec))
}

my_sma <- function(df,n,n2){
  return(df %>% 
           mutate(sma1 = SMA(df$Close,n=n),
                  sma2 = SMA(df$Close,n=n2)))
}

fnames <- dir(path="../data/nasdaq_prices/stocks")
tickers <- lapply(fnames,str_extract,pattern="^[:upper:]+")

my_ma_cross <- function(ticker,ma1,ma2,date=NA,filterDate=FALSE){
  if(filterDate){
    prices <- read_csv(str_glue("../data/nasdaq_prices/stocks/{ticker}.csv")) %>% 
      filter(Date >=date) %>% 
      my_sma(ma1,ma2) %>% 
      select(Date,Close,sma1,sma2) %>%
      drop_na()
  }
  else{
    prices <- read_csv(str_glue("../data/nasdaq_prices/stocks/{ticker}.csv")) %>% 
      my_sma(ma1,ma2) %>% 
      select(Date,Close,sma1,sma2) %>%
      drop_na()
  }
  curPrice <- prices$Close[1]
  prevPrice <- 0
  prevHoldPrice <- curPrice
  account <- 1000
  prevAccount <- 1000
  holdAccount <- 1000
  wins <- 0
  losses <- 0
  winsVec <- rep(0,nrow(prices))
  lossesVec <- rep(0,nrow(prices))
  holdAccountVec <- c(1000,rep(0,nrow(prices)-1))
  accountVec <- c(1000,rep(0,nrow(prices)-1))
  buyVec <- rep(FALSE,nrow(prices))
  sellVec <- rep(FALSE,nrow(prices))
  pct_GainVec <- rep(0,nrow(prices))
  i <- 2
  inTrade <- FALSE
  while(i <= nrow(prices)){
    close <- prices$Close[i]
    curPrice <- close
    s1 <- prices$sma1[i]
    s2 <- prices$sma2[i]
    holdAccount <- 1000 * (curPrice/prevHoldPrice)
    holdAccountVec[i] <- holdAccount
    if(inTrade){
      account <- prevAccount * (curPrice/prevPrice)
      if(s1 < s2){
        inTrade <- FALSE
        sellVec[i] <- TRUE
        pct_GainVec[i] <- (account-prevAccount)/prevAccount*100
        prevAccount <- account
        if(curPrice > prevPrice){
          wins <- wins + 1
        }
        else{
          losses <- losses + 1
        }
      }
    }
    else{
      if(s1 > s2 & prices$sma1[i-1] < prices$sma2[i-1]){
        buyVec[i] <- TRUE
        inTrade <- TRUE
        prevPrice <- curPrice
      }
    }
    winsVec[i] <- wins
    lossesVec[i] <- losses
    accountVec[i] <- account
    i <- i+1
  }
  return(list(Prices = prices, Results = data.frame(Date = prices$Date,Wins = winsVec, Losses = lossesVec, AccountValue = accountVec, Buy = buyVec, Sell = sellVec,pctGain = pct_GainVec,HoldAccountValue=holdAccountVec)))
}

my_sectors <- function(){
  tickers <- rep(NA,110)
  sectorVec <- rep(NA,110)
  j <- 0
  for(sec in sectors){
    print(sec)
    url <- str_glue("https://stockanalysis.com/stocks/sector/{sec}")
    page <- read_html(url)
    res<-page %>% 
      html_elements("td") %>% 
      html_text()
    for(i in 1:10){
      tickers[10*j+i] <- res[2+7*(i-1)]
      sectorVec[10*j+i] <- sec
    }
    j<- j+1
  }
  return(data.frame(Ticker=tickers,Sector=sectorVec))
}
SECTOR_LIST <- read_csv("../data/sectors.csv")

find_min_date <- function(stocks){
  df <-read_csv(str_glue("../data/nasdaq_prices/stocks/{stocks[1]}.csv"))
  date <- df$Date[1]
  for(i in 2:10){
    df <-read_csv(str_glue("../data/nasdaq_prices/stocks/{stocks[i]}.csv"))
    date2 <- df$Date[1]
    if(date2 > date){
      date <- date2
    }
  }
  return(date)
}

my_profit_sectors <- function(sector,ma1,ma2){
  tickers <- SECTOR_LIST %>% filter(Sector == sector)
  stocks <- tickers$Ticker
  dfs <- rep(NA,10)
  date <- find_min_date(stocks)
  res1 <- my_ma_cross(stocks[1],ma1,ma2,date,filterDate = TRUE)$Results %>% 
    select(Date,AccountValue)
  for(i in 2:10){
    res2<-my_ma_cross(stocks[i],ma1,ma2,date,filterDate = TRUE)$Results
    res1$AccountValue <- res1$AccountValue + res2$AccountValue
  }
  return(res1)
}
# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(tabPanel("Pick MA Combination",
      titlePanel("Backtesting Basic SMA Crossover Strategy"),
      sidebarLayout(
        sidebarPanel(
          textInput("ticker","Ticker Symbol:"),
          numericInput("sma1","Shorter Timeframe SMA:",value=10,min=1,max=100,step=1),
          numericInput("sma2","Longer Timeframe SMA:",value=50,min=1,max=200,step=1)
        ),
        mainPanel(
          fluidRow(
            column(12, plotlyOutput("chart",width=1000))),
          fluidRow(
            column(10, plotOutput("profit")),
            column(2, tableOutput("results"))),
        )
      )
    ),
    tabPanel(
      "Multiple MA Combinations"
    ),
    tabPanel(
      "Pick MA Combination by Sector"
    )
  )
) 
# Define server logic required to draw a histogram
server <- function(input, output) {
  res<-reactive({
    validate(
      need(input$ticker %in% tickers, "Please enter a valid ticker as of 2020")
    )
    validate(
      need(input$sma1 < input$sma2, "The longer timeframe SMA must be longer than the shorter timeframe SMA")
    )
    my_ma_cross(input$ticker,input$sma1,input$sma2)
  })
  
  prices <- reactive({
    res()[[1]]})
  results <- reactive({
    res()[[2]]})
  combined <- reactive({
    prices() %>% 
    left_join(results(), by='Date')})
  buys <- reactive({
    combined() %>% 
    filter(Buy==TRUE) %>% 
    select(Date,AccountValue,sma1)})
  sells <- reactive({
    combined() %>% 
    filter(Sell==TRUE) %>% 
    select(Date,AccountValue,sma1)})
  spy <- reactive({
    my_spy_hold(date=combined()$Date[1],filterDate = TRUE)
  })
  
  output$chart <- renderPlotly({
    p<- ggplot()+
      geom_line(data=combined(),aes(x = Date,y = Close, color = 'black'))+
      geom_line(data=combined(),aes(x = Date,y=sma1,color='blue'))+
      geom_line(data=combined(),aes(x = Date,y=sma2,color = 'orange'))+
      geom_point(data=sells(),aes(x = Date,y=sma1),shape=4,size=2,color="red")+
      geom_point(data=buys(),aes(x = Date,y=sma1),shape=4,size=2,color="green")+
      labs(color = "Line Type",y="Price",x="Date",title=str_glue("{input$ticker} Chart"))+
      ylim(c(0,max(combined()$Close)*4))+
      scale_color_manual(values = c('black',"blue", "orange"),
                         labels = c(str_glue('{input$ticker} Price'),str_glue('{input$sma1} SMA'),str_glue('{input$sma2} SMA')))+
      theme_minimal()
    
    # ggplotly(p1, source = "plotly_scatterplot") %>%
    #   event_register("plotly_selected")
    ggplotly(p,dynamicTicks = TRUE)
  })
  
  output$profit <- renderPlot({
    p2<- ggplot()+
      geom_line(data = combined(),aes(x=Date,y=AccountValue,color="black"))+
      geom_line(data = combined(),aes(x=Date,y=HoldAccountValue,color="orange"))+
      geom_line(data = spy(), aes(x=Date,y=AccountValue,color="red"))+
      labs(x = "Date",y="Account Value",title="Profit Using Basic MA Crossover Strategy",subtitle="Starting with $1000, and putting whole account into each trade",color="Type Of Trade")+
      scale_color_manual(values=c("black","orange","red"),
                         labels=c("MA Crossover",str_glue("Holding {input$ticker}"),"Holding SPY"))+
      theme_minimal()
    p2
  })
  
  output$results <- renderTable({
    totals<-results()[nrow(results()),]
    totals<-totals %>% 
      mutate(TotalTrades = Wins+Losses,
             WinPct = Wins/(Losses+Wins),
             AvgPctGain = sum(results()$pctGain)/TotalTrades) %>% 
      select(TotalTrades,Wins,Losses,WinPct,AccountValue,AvgPctGain)
    totals
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
