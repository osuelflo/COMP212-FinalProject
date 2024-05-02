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

fix_na <- function(df){
  bad <-which(is.na(df))
  bad2 <- unique(bad %% nrow(df))
  if(0 %in% bad2){
    df <- df[1:nrow(df)-1,]
  }
  else{
    df[bad2,] <- df[bad2-1,]
  }
  return(df)
}

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
    i <- i +1
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

my_ma_cross <- function(ticker,ma1,ma2,date=NA,date2=NA,filterDate=FALSE){
  if(filterDate){
    prices <- read_csv(str_glue("../data/nasdaq_prices/stocks/{ticker}.csv"))
    prices<- fix_na(prices)
    prices<- prices %>% 
      filter(Date >=date) %>%
      filter(Date <= date2) %>% 
      my_sma(ma1,ma2) %>% 
      select(Date,Close,sma1,sma2) %>%
      drop_na()
  }
  else{
    prices <- read_csv(str_glue("../data/nasdaq_prices/stocks/{ticker}.csv"))
    prices<-fix_na(prices)
    prices<- prices %>% 
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
  for(i in 2:length(stocks)){
    df <-read_csv(str_glue("../data/nasdaq_prices/stocks/{stocks[i]}.csv"))
    date2 <- df$Date[1]
    if(date2 > date){
      date <- date2
    }
  }
  return(date)
}

find_max_date <- function(stocks){
  df <-read_csv(str_glue("../data/nasdaq_prices/stocks/{stocks[1]}.csv"))
  date <- df$Date[nrow(df)]
  for(i in 2:length(stocks)){
    df <-read_csv(str_glue("../data/nasdaq_prices/stocks/{stocks[i]}.csv"))
    date2 <- df$Date[nrow(df)]
    if(date2 < date){
      date <- date2
    }
  }
  return(date)
}

my_profit_sectors <- function(sector,ma1,ma2){
  tickers <- SECTOR_LIST %>% filter(Sector == sector)
  stocks <- tickers$Ticker
  date <- find_min_date(stocks)
  date2 <- find_max_date(stocks)
  res <- my_ma_cross(stocks[1],ma1,ma2,date,date2,filterDate = TRUE)$Results
  prof1 <- res %>% select(Date,AccountValue,HoldAccountValue)
  res1 <- my_collapse_table(res,collapseAPG = TRUE)
  for(i in 2:length(stocks)){
    res <- my_ma_cross(stocks[i],ma1,ma2,date,date2,filterDate = TRUE)$Results
    prof2 <- res %>% select(Date,AccountValue,HoldAccountValue)
    res2 <- my_collapse_table(res,collapseAPG = TRUE)
    prof1$AccountValue <- prof1$AccountValue + prof2$AccountValue
    prof1$HoldAccountValue <- prof1$HoldAccountValue + prof2$HoldAccountValue
    res1$AccountValue <- res1$AccountValue + res2$AccountValue
    res1$TotalTrades <- res1$TotalTrades + res2$TotalTrades
    res1$Wins <- res1$Wins + res2$Wins
    res1$Losses <- res1$Losses + res2$Losses
    res1$AvgPctGain <- res1$AvgPctGain + res2$AvgPctGain
  }
  res1$WinPct <- res1$Wins/res1$TotalTrades*100
  res1$AvgPctGain <- res1$AvgPctGain/res1$TotalTrades
  return(list(Results=res1,Profit=prof1))
}

my_part_ma_cross <- function(ticker){
  winsVec <- rep(0,250)
  lossesVec <- rep(0,250)
  totalTradesVec <- rep(0,250)
  accountVec <- rep(0,250)
  pct_GainVec <- rep(0,250)
  ma1Vec <- rep(0,250)
  ma2Vec <- rep(0,250)
  winRateVec <- rep(0,250)
  j<- 1
  p <- read_csv(str_glue("../data/nasdaq_prices/stocks/{ticker}.csv"))
  for(ma1 in seq(from=5,to=50,by=5)){
    for(ma2 in seq(from=1.2,to=6,by=0.2)){
      prices <- p %>% 
        my_sma(ma1,round(ma1*ma2)) %>% 
        select(Date,Close,sma1,sma2) %>%
        drop_na()
      curPrice <- prices$Close[1]
      prevPrice <- 0
      prevAccount <- 1000
      account <- 1000
      wins <- 0
      losses <- 0
      pctGain <- 0
      i <- 2
      inTrade <- FALSE
      while(i <= nrow(prices)){
        close <- prices$Close[i]
        curPrice <- close
        s1 <- prices$sma1[i]
        s2 <- prices$sma2[i]
        if(inTrade){
          account <- prevAccount * (curPrice/prevPrice)
          if(s1 < s2){
            inTrade <- FALSE
            pctGain <- pctGain+(account-prevAccount)/prevAccount*100
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
            inTrade <- TRUE
            prevPrice <- curPrice
          }
        }
        i <- i+1
      }
      winsVec[j] <- wins
      lossesVec[j] <- losses
      totalTradesVec[j] <- wins+losses
      accountVec[j] <- account
      pct_GainVec[j] <- pctGain/(wins+losses)
      ma1Vec[j] <- ma1
      ma2Vec[j] <- ma2
      winRateVec[j] <- wins/(wins+losses)*100
      print(j)
      j<-j+1
    }
  }
  return(data.frame(MA1 = ma1Vec,
                    MA2 = ma2Vec,
                    Wins = winsVec,
                    Losses = lossesVec,
                    AccountValue = accountVec,
                    AvgPctGain = pct_GainVec,
                    WinRate = winRateVec))
}

my_collapse_table <- function(results,collapseAPG=FALSE){
  if(collapseAPG){
    totals<-results[nrow(results),]
    totals<-totals %>% 
      mutate(TotalTrades = Wins+Losses,
             WinPct = Wins/(Losses+Wins)*100,
             AvgPctGain = sum(results$pctGain)) %>% 
      select(TotalTrades,Wins,Losses,WinPct,AccountValue,AvgPctGain)
    return(totals)
  }
  else{
    totals<-results[nrow(results),]
    totals<-totals %>% 
      mutate(TotalTrades = Wins+Losses,
             WinPct = Wins/(Losses+Wins)*100,
             AvgPctGain = sum(results$pctGain)/TotalTrades) %>% 
      select(TotalTrades,Wins,Losses,WinPct,AccountValue,AvgPctGain)
    return(totals)
  }
}

my_stocks_sector <- function(sector){
  temp <- SECTOR_LIST %>% filter(Sector == sector)
  return(data.frame(Tickers=temp$Ticker))
}

# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(id="tabs",tabPanel("Pick MA Combination", value = "t1",
      titlePanel("Backtesting Basic MA Crossover Strategy"),
      sidebarLayout(
        sidebarPanel(
          textInput("ticker","Ticker Symbol:"),
          numericInput("sma1","Shorter Timeframe SMA:",value=10,min=1,max=100,step=1),
          numericInput("sma2","Longer Timeframe SMA:",value=50,min=1,max=200,step=1),
          h1("MA"),
          p(strong("A moving average, or MA, is a lagging indicator that is popular among traders as a way to take trades in the stock market. A moving average has a length `n`, and is calculated by finding the average of the previous `n` daily closing prices of the stock.")),
          h1("Strategy"),
          p(strong("The Moving Average Crossover Strategy that will be using works as follows: We take two moving averages, designating one as the shorter timeframe moving average, and one as the longer moving average. If the shorter timeframe moving average crosses over the longer timeframe one (i.e. the value of the shorter one goes from less than the longer one to greater than), we buy the stock. Once the shorter moving average crosses back below the longer moving average, we sell."))
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
      "Multiple MA Combinations", value = "t2",
      titlePanel("Comparing Different MA Combinations"),
      sidebarLayout(
        sidebarPanel(
          textInput("ticker2","Ticker Symbol:")
        ),
        mainPanel(
          fluidRow(
            column(12, plotOutput("accountValue",width=800))),
          fluidRow(
            column(12, plotOutput("winRate",width=800))),
          fluidRow(
            column(12, plotOutput("avgPctGain",width=800))),
        )
      )
    ),
    tabPanel(
      "Pick MA Combination by Sector", value = "t3",
      titlePanel("Backtesting MA Crossover Strategy Across Sectors"),
      sidebarLayout(
        sidebarPanel(
          numericInput("sect_sma1","Shorter Timeframe SMA:",value=10,min=1,max=100,step=1),
          numericInput("sect_sma2","Longer Timeframe SMA:",value=50,min=1,max=200,step=1),
          selectInput("sector","Sector:",unique(SECTOR_LIST$Sector)),
          fluidRow(column(2, tableOutput("sectorStocks")))
        ),
        mainPanel(
          fluidRow(
            column(12, plotOutput("sectorProfit",width=600))),
          fluidRow(
            column(12, tableOutput("sectorResults")))
        )
      )
    )
  )
) 
# Define server logic required to draw a histogram
server <- function(input, output) {
  res<-reactive({ # Results for the first tab
    
    # All the validate statements will make sure that valid entries are given for each of the user inputs
    validate(
      need(input$ticker %in% tickers, "Please enter a valid ticker as of 2020")
    )
    validate(
      need(input$sma1 < input$sma2, "The longer timeframe SMA must be longer than the shorter timeframe SMA")
    )
    validate(
      need(input$sma1>0 & input$sma2 > 0, "Please enter positive SMA timeframe lengths")
    )
    validate(
      need(input$sma1<=300 & input$sma2 <= 300, "Please enter SMA timeframe lengths that are less than or equal to 300")
    )
    my_ma_cross(input$ticker,input$sma1,input$sma2)
  })
  
  # Everything must be in a reactive context in order for the app to change depending on user input
  prices <- reactive({ # Price dataframe for the interactive viz on top of first tab
    res()[[1]]})
  results <- reactive({ # Results dataframe for the table and plot on bottom of first tab
    res()[[2]]})
  combined <- reactive({
    prices() %>% 
    left_join(results(), by='Date')})
  buys <- reactive({ # Getting dates when stocks were bought to include for interactive viz on first tab
    combined() %>% 
    filter(Buy==TRUE) %>% 
    select(Date,AccountValue,sma1)})
  sells <- reactive({ # Getting dates when stocks were sold to include for interactive viz on first tab
    combined() %>% 
    filter(Sell==TRUE) %>% 
    select(Date,AccountValue,sma1)})
  spy1 <- reactive({ # Getting data on holding SPY for date depending on ticker chosen by user for first tab
    d <- combined()$Date[1]
    my_spy_hold(date=combined()$Date[1],filterDate = TRUE)
  })
  ma_comb <- reactive({ # All data needed for visualizations in second tab
    if(input$ticker2 == ""){
      if(input$ticker == ""){
        validate(
          need(input$ticker2 %in% tickers, "Please enter a valid ticker as of 2020")
        )
      }
      else{
        my_part_ma_cross(input$ticker)
      }
    }
    else{
      validate(
        need(input$ticker2 %in% tickers, "Please enter a valid ticker as of 2020")
      )
      my_part_ma_cross(input$ticker2)
    }
  })
  
  sector_results <- reactive({ # Sector data for results of strategy by sector for third tab
    validate(
      need(input$sect_sma1 < input$sect_sma2, "The longer timeframe SMA must be longer than the shorter timeframe SMA")
    )
    validate(
      need(input$sect_sma1>0 & input$sect_sma2 > 0, "Please enter positive SMA timeframe lengths")
    )
    validate(
      need(input$sect_sma1<=300 & input$sect_sma2 <= 300, "Please enter SMA timeframe lengths that are less than or equal to 300")
    )
    my_profit_sectors(input$sector,input$sect_sma1,input$sect_sma2)
  })
  sector_stocks <- reactive({
    my_stocks_sector(input$sector)
  })
  
  spy2 <- reactive({ # Getting data on holding SPY for date depending on ticker chosen by user for first tab
    temp <- sector_results()$Profit
    d <- temp$Date[1]
    my_spy_hold(date=d,filterDate = TRUE)
  })
  
  output$chart <- renderPlotly({ # First viz on first tab
    p<- ggplot()+
      geom_line(data=combined(),aes(x = Date,y = Close, color = 'black'))+
      geom_line(data=combined(),aes(x = Date,y=sma1,color='blue'))+
      geom_line(data=combined(),aes(x = Date,y=sma2,color = 'orange'))+
      geom_point(data=sells(),aes(x = Date,y=sma1),shape=4,size=2,color="red")+
      geom_point(data=buys(),aes(x = Date,y=sma1),shape=4,size=2,color="green")+
      labs(color = "Line Type",y="Price",x="Date",title=str_glue("{input$ticker} Chart"),subtitle="Interactive Chart. Green X's represent where trades were taken, and Red X's represent where trades were closed.")+
      ylim(c(0,max(combined()$Close)*4))+
      scale_color_manual(values = c('black',"blue", "orange"),
                         labels = c(str_glue('{input$ticker} Price'),str_glue('{input$sma1} SMA'),str_glue('{input$sma2} SMA')))+
      theme_minimal()
    ggplotly(p,dynamicTicks = TRUE)
  })
  
  output$profit <- renderPlot({ # Profit graph on bottom of first tab
    p2<- ggplot()+
      geom_line(data = combined(),aes(x=Date,y=AccountValue,color="black"))+
      geom_line(data = combined(),aes(x=Date,y=HoldAccountValue,color="orange"))+
      geom_line(data = spy1(), aes(x=Date,y=AccountValue,color="red"))+
      labs(x = "Date",y="Account Value",title="Profit Using Basic MA Crossover Strategy",subtitle="Starting with $1000, and putting whole account into each trade",color="Type Of Trade")+
      scale_color_manual(values=c("black","orange","red"),
                         labels=c("MA Crossover",str_glue("Holding {input$ticker}"),"Holding SPY"))+
      theme_minimal()
    p2
  })
  
  output$results <- renderTable({ # Table on bottom of first tab
    my_collapse_table(results())
  })
  
  output$accountValue <- renderPlot({ # Account value heatmap in second tab
    ma_comb() %>% 
      ggplot()+
      geom_tile(aes(x=MA1,y=MA2,fill=log10(AccountValue)))+
      scale_fill_viridis_b()+
      labs(x="Length of Shorter Moving Average",
           y="Ratio of Longer Moving Average to Shorter Moving Average",
           fill="Base 10 Log of Account Value",
           title="Comparing MA Crossover Strategy Across Different Combinations of MA Lengths",
           subtitle="Each rectangle represents the end account value of the strategy with the corresponding moving averages")+
      theme_classic()
  })
  
  output$winRate <- renderPlot({ # Win rate heatmap in second tab
    ma_comb() %>% 
      ggplot()+
      geom_tile(aes(x=MA1,y=MA2,fill=AvgPctGain))+
      scale_fill_viridis_b()+
      labs(x="Length of Shorter Moving Average",
           y="Ratio of Longer Moving Average to Shorter Moving Average",
           fill="Average Percent \nGain of Each \nTrade",
           subtitle="Each rectangle represents the average percent gain of each trade of the strategy with the corresponding moving averages")+
      theme_classic()
  })
  
  output$avgPctGain <- renderPlot({ # Average pct gain heatmap in second tab
    ma_comb() %>% 
      ggplot()+
      geom_tile(aes(x=MA1,y=MA2,fill=WinRate))+
      scale_fill_viridis_b()+
      labs(x="Length of Shorter Moving Average",
           y="Ratio of Longer Moving Average to Shorter Moving Average",
           fill="Win Rate \n(% of trades\n that made money)",
           subtitle="Each rectangle represents the win rate of the strategy with the corresponding moving averages")+
      theme_classic()
  })
  
  output$sectorProfit <- renderPlot({ # Sector profit chart in third tab
    prof <- sector_results()$Profit
    sp <- spy2() %>% filter(Date >= prof$Date[1])
    val <-1000*nrow(sector_stocks())
    p2<- ggplot()+
      geom_line(data = prof,aes(x=Date,y=AccountValue,color="black"))+
      geom_line(data = prof,aes(x=Date,y=HoldAccountValue,color="orange"))+
      geom_line(data = sp, aes(x=Date,y=AccountValue*nrow(sector_stocks()),color="red"))+
      labs(x = "Date",y="Account Value",title="Comparing Profit Across Different Types of Trades",subtitle=str_glue("Starting with 1000$. and putting whole account into each trade, for each ticker.\n Putting {val}$ into SPY and $1000 into each ticker and holding"),color="Type Of Trade")+
      scale_color_manual(values=c("black","orange","red"),
                         labels=c("MA Crossover",str_glue("Holding {input$sector} Sector"),"Holding SPY"))+
      theme_minimal()
    p2
  })
  
  output$sectorResults <- renderTable({ # Sector results in third tab
    sector_results()$Results
  })
  
  output$sectorStocks <- renderTable({ # List of stocks in sector displayed in sidebar of third tab
    sector_stocks()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
