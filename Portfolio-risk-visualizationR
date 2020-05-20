"
Portfolio Management - IST

Correspondance: karjamatti@gmail.com

PLEASE REFER TO THE ATTACHED README-FILE FOR ADDITIONAL INFORMATION
"


#' ---------------------------------------------------------------------------
#' SETUP AND PARAMETER
#' ---------------------------------------------------------------------------

bw <- 0.001 # Set Bin width for histograms
num.sim <- 2000 # Number of Simulations
num.plot <- 500 # Number of PLotted Paths
num.uncond <- 20000 # Length of simulated unconditional return path

# UPDATE NUMBER OF INDICES HERE!
num.eq <- 3 # NUmber equity indices
num.gb <- 2 # NUmber government bond indices
num.cb <- 2 # NUmber corporate bond indices
num.re <- 2 # NUmber real estate indices


          #' ---------------------------------------------------------------------------
          #' IMPORTS & DIRECTORY
          #' ---------------------------------------------------------------------------
          
          # Load or install and load packages
          'package' <- function(library, repo = getOption('repos')){ 
            if(!is.element(library, .packages(all.available = TRUE))) {install.packages(library)}
            library(library,character.only = TRUE)}
          
          'package'('VineCopula')
          'package'('copula', 'http://R-Forge.R-project.org')
          'package'('scatterplot3d')
          'package'('ggplot2')
          'package'('grid')
          'package'('dplyr')
          'package'('lubridate')
          'package'('psych')
          'package'('rmutil')
          'package'('shiny')
          'package'('MASS')
          'package'('fitdistrplus')
          'package'('stats')
          'package'('metRology')
          'package'('rugarch')
          'package'('TSA')
          'package'('fGarch')
          'package'('mvtnorm')
          'package'('tidyr')
          'package'('PerformanceAnalytics')
          'package'('gridExtra')
          
          
          #' ---------------------------------------------------------------------------
          #' AUTOMATIC DETECTION OF RETURN FILE PATH AND DIRECTORY SETUP 
          #' ---------------------------------------------------------------------------
          
          returnfilename <- 'dailyreturns.csv'
          abspath <- function(file){file.path(normalizePath(dirname(file))) %>% return()}
          dir <- abspath(returnfilename)
          
          # dir <- Z:/Desktop # IF AUTOMATIC DETECTION DOES NOT WORK, DECLARE DIRECTORY HERE AND UN-COMMENT
          
          setwd(dir)
          stopifnot(all.equal(getwd(), gsub("\\", '/', dir, fixed = TRUE)))
          
          
#' ---------------------------------------------------------------------------
#' DATA IMPORT
#' ---------------------------------------------------------------------------

          data <- read.csv('dailyreturns.csv', sep =',', header = TRUE, encoding = 'utf-8') # Load default data
          data$Date <- lubridate::ymd(data$Date) # Convert dates to date objects
          
          equity <- colnames(data)[2:(1+num.eq)]
          govbond <- colnames(data)[(2+num.eq):(1+num.eq+num.cb)]
          corpbond <- colnames(data)[(2+num.eq+num.cb):(1+num.eq+num.cb+num.gb)]
          realestate <- colnames(data)[(2+num.eq+num.cb+num.gb):ncol(data)]
          

#' ---------------------------------------------------------------------------
#' USER DEFINED HELPER FUNCTIONS
#' ---------------------------------------------------------------------------          
          
          
          #' ---------------------------------------------------------------------------
          #' SHORT HELPER FUNCTIONS (LAMBDA-LIKE ONE-LINERS)
          #' ---------------------------------------------------------------------------
          
          'extract residuals' <- function(fit){fit %>% residuals(standardize = TRUE) %>% as.vector() %>% return()} # Extracts the residuals from the GARCH fits
          'fit skew t' <- function(data){fitdistrplus::fitdist(data, "sstd", start = list(mean = mean(data), sd = sd(data), nu = 9, xi = 0.9))$estimate %>% return()} # Fits a skewed t-distribution to the data
          'specify sstd' <- function(data){list(mean = data[1] %>% as.numeric(), sd = data[2] %>% as.numeric(), nu = data[3] %>% as.numeric(), xi = data[4] %>% as.numeric()) %>% return()} # Specifies a list of marginal distribution parameters (skewed t dist)
          'specify params' <- function(fits){apply(fits, 2, 'specify sstd') %>% return()} # Specifies a list of parameter lists for generating copula objects
          'generate mvdc' <- function(c, m, p){copula::mvdc(copula = c, margins = m, paramMargins = p) %>% return()} # Generates mvdc-objects
          'extract parameters' <- function(fit){fit %>% coef() %>% return()} # Extract parameters from the GARCH-fits
          'portfolio' <- function(returns, weights){(weights %>% as.matrix()) %*% (returns %>% t()) %>% t() %>% as.data.frame() %>% return()} # Computes portfolio returns based on asset weights and returns
          'extract volatility' <- function(fit){(fit %>% residuals(standardize = FALSE) %>% as.vector()) / (fit %>% residuals(standardize = TRUE) %>% as.vector()) %>% return()} # Extracts volatility from the GARCH-fits
          'portfolio sd' <- function(weights, cov.mat){(weights %>% as.matrix()) %*% cov.mat %*% (weights %>% as.matrix() %>% t()) %>% sqrt() %>% return()} # Computes the portfolio standard deviation
          
          
          #' ---------------------------------------------------------------------------
          #' INDEX SELECTION Chooses the return time-series corresponding to input from shiny app
          #' ---------------------------------------------------------------------------
          
          'choose indices' <- function(eq, gb, cb, re){
            rets <- data %>% dplyr::select(Date, eq, gb, cb, re) # Returns of asset class indices
            N <- rets %>% ncol() - 1 # Number of Asset Classes
            t <- rets %>% nrow() # Number of observations per asset
            list(rets, N, t) %>% return()}
          
          
          #' ---------------------------------------------------------------------------
          #' GARCH SIMULATOR
          #' ---------------------------------------------------------------------------
          
          'garch sim' <- function(p, r, startstd = 0, startsr = 0, startreturn = 0){
            
            # Specifications and parameters
            t <- length(r)
            mu <- p[1]
            ar <- p[2]
            ma <- p[3]
            omega <- p[4]
            alpha <- p[5]
            beta <- p[6]
            
            # Initialize vectors and set starting points
            var <- std.d <- return <- eps <- numeric(t)
            var[1] <- startstd^2
            eps[1] <- startsr * startstd
            return[1] <- startreturn
            
            for (i in 2:t){ var[i] <- omega + alpha*eps[i-1]^2 + beta*var[i-1]
            std.d[i] <- var[i] %>% sqrt()
            eps[i] <- r[i]*std.d[i]
            return[i] <- mu + ar*return[i-1] + ma*eps[i-1] + eps[i]}
            
            return %>% return()}
          
          
          #' ---------------------------------------------------------------------------
          #' PORTFOLIO GAUSSIAN MOMENTS CLACULATOR
          #' ---------------------------------------------------------------------------
          
          'gaussian moments' <- function(rets, weights){
            cov.mat <- cov(rets %>% dplyr::select(-Date))
            exp.ret <- apply(rets %>% dplyr::select(-Date), 2, mean)
            port.sd <- 'portfolio sd'(weights, cov.mat) # Calculate portfolio return standard deviation
            port.expret <- (weights * exp.ret) %>% sum() # Calculate portfolio expected return
            cbind.data.frame(port.expret, port.sd) %>% return()}
          
          
          #' ---------------------------------------------------------------------------
          #' CONDITIONAL RETURN SIMULATOR Simulates from the garch simulator using the current market situation
          #' ---------------------------------------------------------------------------
          
          'conditional returns' <- function(current, forecast.horizon = 7, copula = t.mvdc, garch.params, weights){
            
            last.vol <- current[[1]]
            last.ret <- current[[2]]
            last.sr <- current[[3]]
          
            forecast.horizon <- forecast.horizon + 1
            forecast.sim <- rMvdc(forecast.horizon, copula)
            forecast.returns <- mapply('garch sim', p = garch.params, r = forecast.sim %>% as.data.frame(), 
                                       startstd = last.vol, startsr = last.sr, startreturn = last.ret) # Simulate
            returns <- 'portfolio'(forecast.returns, weights) %>% as.vector()
            cumrets <- numeric(forecast.horizon)
            for (i in 1:forecast.horizon){cumrets[i] <- Return.cumulative(returns[1:i, 1])}
            cumrets %>% return()
          }

          
#' ---------------------------------------------------------------------------
#' MAIN GARCH-COPULA FUNCTION
#' ---------------------------------------------------------------------------

          'garch copula' <- function(eq, gb, cb, re, updateProgress = NULL){
            
            rets <- data %>% dplyr::select(Date, eq, gb, cb, re) # Returns of asset class indices
            N <- rets %>% ncol() - 1 # Number of Asset Classes
            t <- rets %>% nrow() # Number of observations per asset
            
            
            #' ---------------------------------------------------------------------------
            #' GARCH SPEC
            #' ---------------------------------------------------------------------------
            
            specs <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)), # Srandard GARCH with order (1,1)
                                mean.model = list(armaOrder=c(1,1)), # ARMA(1,1)
                                distribution.model = "std") # Student t disturbances
            
            
            #' ---------------------------------------------------------------------------
            #' GARCH FIT
            #' ---------------------------------------------------------------------------
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Garch Model for:', eq)
              updateProgress(detail = text)}
          
            eq.garchfit <- ugarchfit(specs, rets %>% dplyr::select(eq))
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Garch Model for:', gb)
              updateProgress(detail = text)}
            
            gb.garchfit <- ugarchfit(specs, rets %>% dplyr::select(gb))
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Garch Model for:', cb)
              updateProgress(detail = text)}
            
            cb.garchfit <- ugarchfit(specs, rets %>% dplyr::select(cb))
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Garch Model for:', re)
              updateProgress(detail = text)}
            
            re.garchfit <- ugarchfit(specs, rets %>% dplyr::select(re))
            
            garchfits <- list(eq.garchfit, gb.garchfit, cb.garchfit, re.garchfit) # Construct list for convenience
            garch.params <- mapply('extract parameters', garchfits) %>% as.data.frame() # Extract parameters from the list of fit objects
            colnames(garch.params) <- colnames(rets %>% dplyr::select(-Date)) # Set column names
            
            
            #' ---------------------------------------------------------------------------
            #' STANDARDIZED RESIDUALS
            #' ---------------------------------------------------------------------------
            
            sr <- mapply(garchfits, FUN = 'extract residuals') %>% cbind.data.frame(rets$Date) %>% dplyr::select('rets$Date', everything())
            colnames(sr) <- colnames(rets)
            
            #' ---------------------------------------------------------------------------
            #' FITTING THE MARGINAL DISTRIBUTIONS
            #' ---------------------------------------------------------------------------
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Marginal Distributions')
              updateProgress(detail = text)}
            
            skew.t.fits <- apply(sr %>% dplyr::select(-Date), 2, 'fit skew t')
            
            
            #' ---------------------------------------------------------------------------
            #' FITTING THE COPULAS
            #' ---------------------------------------------------------------------------
            
            if (is.function(updateProgress)){
              text <- paste('Transforming Standardized Residuals into Pseudo-Observations')
              updateProgress(detail = text)}
            
            u <- sr %>% dplyr::select(-Date) %>% VineCopula::pobs() # Compute pseudo-observations
            
            if (is.function(updateProgress)){
              text <- paste('Fitting Copula')
              updateProgress(detail = text)}
            
            t.fit.mpl <- copula::fitCopula(tCopula(dim = N, dispstr = 'un'), u, method = 'mpl', estimate.variance = FALSE) # T-Copula 
            num.rhos <- N*(N-1)/2 # Number of rho-parameters for T Copula
            t.rhos <- numeric(num.rhos) # Rhos for T Copula
            
            for (i in 1:num.rhos){t.rhos[i] <- summary(t.fit.mpl)$coef[i]}
            
            t.df <- summary(t.fit.mpl)$coef[num.rhos+1] # Degrees of freedom for T Copula
            
            
            #' ---------------------------------------------------------------------------
            #' BUILDING THE COPULAS
            #' ---------------------------------------------------------------------------
           
            if (is.function(updateProgress)){
              text <- paste('Building Copula Object')
              updateProgress(detail = text)}
            
            t.copu <- tCopula(t.rhos, dim = N, dispstr = "un", df = t.df)
            
            # Specifying Marginal Distributions
            margins <- rep('sstd', N) # Default marginal family is the fitted skewed & scaled student t
            t.params <- 'specify params'(skew.t.fits) # Builds the list of parameter lists
            
            # Simulating from the Gaussian and T copulas
            t.mvdc <- 'generate mvdc'(t.copu, margins, t.params)
            
            if (is.function(updateProgress)){
              text <- paste('Simulating Standardized Residuals from Copula')
              updateProgress(detail = text)}
            
            t.sim <- rMvdc(num.uncond, t.mvdc)
            
            #' ---------------------------------------------------------------------------
            #' EXTRACTING CURRENT MARKET CONDITIONS
            #' ---------------------------------------------------------------------------
            
            # Extract the volatility from the GARCH fits
            vol <- mapply(garchfits, FUN = 'extract volatility') %>% cbind.data.frame(rets$Date) %>% dplyr::select('rets$Date', everything())
            colnames(vol) <- colnames(rets)
            
            # Specify last values
            last.vol <- vol[t,] %>% dplyr::select(-Date)
            last.ret <- rets[t,] %>% dplyr::select(-Date)
            last.sr <- sr[t,] %>% dplyr::select(-Date)
            current.values <- list(last.vol, last.ret, last.sr)
          
            list(garch.params, t.sim, current.values, t.mvdc) %>% return()
          }
  

#' ---------------------------------------------------------------------------
#' SHINY
#' ---------------------------------------------------------------------------          


#' ---------------------------------------------------------------------------
#' SHINY USER INTERFACE
#' ---------------------------------------------------------------------------

ui <- fluidPage(
  
  # Removes spin wheels from inputs
  tags$style(HTML("input[type=number] {
                          -moz-appearance:textfield;
                    }
                    input[type=number]::{
                          -moz-appearance:textfield;
                    }
                    input[type=number]::-webkit-outer-spin-button,
                    input[type=number]::-webkit-inner-spin-button {
                          -webkit-appearance: none;
                          margin: 0;}")),
  
  titlePanel('Investment Strategy Tool'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('This Tool Simulates Portfolio Returns by applying a GARCH-Copula modeling framework'),
      
      # CHOICES FOR INDICES AND WEIGHTS
      fluidRow( column(8, selectInput('eqi', label = 'Choose Equity Index', choices = equity, selected = equity[1])),
                column(4, numericInput('eqw', label = 'Weight', value = 0.5, step = 0.05))),
      
      fluidRow( column(8, selectInput('gbi', label = 'Choose Gov. Bond Index', choices = govbond, selected = govbond[1])),
                column(4, numericInput('gbw', label = 'Weight', value = 0.2, step = 0.05))),
      
      fluidRow( column(8, selectInput('cbi', label = 'Choose Corp. Bond Index', choices = corpbond, selected = corpbond[1])),
                column(4, numericInput('cbw', label = 'Weight', value = 0.2, step = 0.05))),
      
      fluidRow( column(8, selectInput('rei', label = 'Choose Real Estate Index', choices = realestate, selected = realestate[1])),
                column(4, h3(textOutput('rew')))),
      
      # CHOICE OF FORECAST HORIZON
      fluidRow(column(12, selectInput('fch', label = 'Forecast Horizon', choices = 1:30, selected = 10))),
      
      # CHOICE OF VAR PROBABILITY
      fluidRow(column(12, selectInput('varprob', label = 'Value-at-Risk Probability', choices = list(0.01, 0.025, 0.05, 0.1), selected = 0.01))),
      
      width = 5),
    
    # PlOTS
    mainPanel(
      plotOutput('path'),
      plotOutput('hist'),
      plotOutput('tail'),
      width = 7
    )
  )
)


#' ---------------------------------------------------------------------------
#' SHINY SERVER
#' ---------------------------------------------------------------------------

server <- function(input, output) {

  # REACTIVE FUNCTION FOR RETURNING COPULA-SIMAULATED  VARIATES
  'get copula' <- reactive({
    
    # PROGRESS OBJECT
    progress <- shiny::Progress$new()
    progress$set(message = "GARCH-COPULA fitting in Progress... ", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    arglist <- 'garch copula'(input$eqi, input$gbi, input$cbi, input$rei, updateProgress = updateProgress)
    arglist %>% return()

  })
  
  output$rew <- renderText({1 - input$eqw - input$gbw - input$cbw})
  
  output$path <- renderPlot({
    
    # Input Selections from Shiny App User Interface
    eq <- input$eqi
    gb <- input$gbi
    cb <- input$cbi
    re <- input$rei
    eq.w <- input$eqw
    gb.w <- input$gbw
    cb.w <- input$cbw
    re.w <- 1 - sum(eq.w, gb.w, cb.w)
    weights <- cbind.data.frame(eq.w, gb.w, cb.w, re.w)
    forecast.horizon <- input$fch %>% as.numeric() # Number of days to forecast
    
    # Call 'choose indices' and unpack returned arguments
    arglist <- 'choose indices'(eq, gb, cb, re)
    rets <- arglist[[1]]
    N <- arglist[[2]]
    t <- arglist[[3]]
    
    # Call 'get copula' and unpack returned arguments
    arglist <- 'get copula'()
    garch.params <- arglist[[1]]
    t.sim <- arglist[[2]]
    current.values <- arglist[[3]]
    last.ret <- current.values[[2]]
    t.mvdc <- arglist[[4]]
    
    
    #' ---------------------------------------------------------------------------
    #' PROGRESS FUNCTION
    #' ---------------------------------------------------------------------------
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Conditional Return Simulation in Progress... ", value = 0)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 2
      }
      progress$set(value = value, detail = detail)
    }
    
    
    #' ---------------------------------------------------------------------------
    #' SIMULATING UPCOMING RETURN DISTRIBUTION
    #' ---------------------------------------------------------------------------
    
    if (is.function(updateProgress)){
      text <- paste('Simulating Conditional Returns')
      updateProgress(detail = text)}
    
    # Simulate future returns, conditional on the observed returns
    conditional.portfolio.returns <- replicate(num.sim, 'conditional returns'(current = current.values, forecast.horizon = forecast.horizon, copula = t.mvdc, garch.params = garch.params, weights = weights), 
                                               simplify = FALSE) %>% unlist() %>% cbind.data.frame()
    
    
    #' ---------------------------------------------------------------------------
    #' DATA MANIPULATION FOR GGPLOT
    #' ---------------------------------------------------------------------------
    
    if (is.function(updateProgress)){
      text <- paste('Rendering Distribution Plots for Conditional Returns')
      updateProgress(detail = text)}
    
    # Construct long format data frame for plotting convenience
    dayvec <- rep(0:(forecast.horizon), num.sim)
    simvec <- rep(1:num.sim, each = forecast.horizon + 1 )
    long <- cbind.data.frame(simvec, dayvec, conditional.portfolio.returns)
    colnames(long) <- c('Simulation', 'Day', 'Return')
    longplot <- long[1:(num.plot * (forecast.horizon + 1)),] # Subset of simulated paths for plotting
    
    # Compute Simulated VaR based on quantile of last day forecasted cumulative returns
    p <- input$varprob %>% as.numeric()
    longlast <- subset(long, Day = (forecast.horizon + 1)) # Subset of cumulative returns on final forecast day
    var.ret <- quantile(longlast$Return, p)
    
    # Calculate Gaussian confidence intervals
    gaus.moments <- 'gaussian moments'(rets, weights)
    port.expret <- gaus.moments[,1] %>% as.numeric()
    port.sd <- gaus.moments[,2] %>% as.numeric()
    
    # Construct Gaussian confidence intervals
    dayz <- seq(0, forecast.horizon, 0.01)
    evaluation <- mapply(sqrt, seq(0, forecast.horizon, 0.01)) * (port.sd %>% as.numeric())
    gaus.lower <- mapply(qnorm, p = p, mean = (last.ret %>% as.matrix()) %*% (weights %>% as.matrix() %>% t()), sd = evaluation) + exp(dayz * log(1 + port.expret)) - 1
    gaus.upper <- mapply(qnorm, p = (1-p), mean = (last.ret %>% as.matrix()) %*% (weights %>% as.matrix() %>% t()), sd = evaluation) + exp(dayz * log(1 + port.expret)) - 1
    gaus <- cbind.data.frame(dayz, gaus.lower, gaus.upper)
    
    # Cumulative return path plot
    pathplot <- ggplot(longplot, aes(x = Day)) +
      geom_hline(yintercept = 0, color ="#000000", size = 0.4) +
      geom_line(stat = "identity", aes(y = Return, color = as.factor(Simulation)), alpha = 0.3) +
      theme(legend.position = "none") + ylab('Cumulative return') + xlab('Day') + labs(title = 'Simulated Cumulative Return Paths\nGaussian Confidence Interval based on specified VaR in black') +
      geom_line(data = gaus, aes(y = gaus.lower, x = dayz), size = 1.5) + geom_line(data = gaus, aes(y = gaus.upper, x = dayz), size = 1.5)
    
    # Cumulative return histrogram
    pathhist <- ggplot(data = longlast, aes(Return)) +
      geom_histogram(colour = 'black', fill = 'white', binwidth = bw) +
      geom_density(aes(y = bw * ..count..), alpha = 0.5, fill = 'yellow') +
      stat_function(fun = function(x) dnorm(x, mean = port.expret, sd = evaluation[length(evaluation)]) * bw * nrow(longlast), geom = 'area', fill = 'red', alpha = 0.4) + 
      ylab("") + xlab('Cumulative Return') + labs(title = paste('Histogram of Conditional Cumulative Returns for next', forecast.horizon, 'trading days\nGARCH-Copula VaR (black line):', round(var.ret*100, digits = 2),'%\nGaussian VaR (red line):', round(gaus.lower[length(gaus.lower)]*100, digits = 2), '%')) + 
      geom_vline(xintercept = var.ret, color = 'black') + geom_vline(xintercept = gaus.lower[length(gaus.lower)], color = 'red') +
      scale_y_continuous(breaks = NULL) + 
      scale_fill_continuous(guide = guide_legend())
    
    grid.arrange(pathplot, pathhist, ncol=2)

  })

    output$hist <- renderPlot({
      
      # Input Selections from Shiny App User Interface
      eq <- input$eqi
      gb <- input$gbi
      cb <- input$cbi
      re <- input$rei
      eq.w <- input$eqw
      gb.w <- input$gbw
      cb.w <- input$cbw
      re.w <- 1 - sum(eq.w, gb.w, cb.w)
      weights <- cbind.data.frame(eq.w, gb.w, cb.w, re.w)
      
      # Call 'choose indices' and unpack returned arguments
      arglist <- 'choose indices'(eq, gb, cb, re)
      rets <- arglist[[1]]
      N <- arglist[[2]]
      t <- arglist[[3]]
      
      # Call 'get copula' and unpack returned arguments
      arglist <- 'get copula'()
      garch.params <- arglist[[1]]
      t.sim <- arglist[[2]]
      
      # Calculate Gaussian confidence intervals
      gaus.moments <- 'gaussian moments'(rets, weights)
      port.expret <- gaus.moments[1,1] %>% as.numeric()
      port.sd <- gaus.moments[1,2] %>% as.numeric()
      
      
      #' ---------------------------------------------------------------------------
      #' PROGRESS FUNCTION
      #' ---------------------------------------------------------------------------
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Unconditional Return Simulation in Progress... ", value = 0)
      on.exit(progress$close())
      
      # Create a callback function to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 2
        }
        progress$set(value = value, detail = detail)
      }
      
      #' ---------------------------------------------------------------------------
      #' UNCONDITIONAL PORTFOLIO RETURNS FROM THE GARCH-COPULA MODEL
      #' ---------------------------------------------------------------------------
      
      
      if (is.function(updateProgress)){
        text <- paste('Simulating Unconditional Returns')
        updateProgress(detail = text)}
      
      # Simulate returns
      sim.returns <- mapply('garch sim', p = garch.params, r = t.sim %>% as.data.frame()) # Simulate
      port.sim <- 'portfolio'(sim.returns, weights)
      colnames(port.sim) <- c('Return')
      
      if (is.function(updateProgress)){
        text <- paste('Rendering Plots for Unconditional Returns')
        updateProgress(detail = text)}
      
      #' ---------------------------------------------------------------------------
      #' HISTOGRAM OF UNCONDITIONAL RETURNS
      #' ---------------------------------------------------------------------------
      
      # Compute VaR figures
      p <- input$varprob %>% as.numeric()
      var.garch <- quantile(port.sim$Return, p)
      var.gaus <- qnorm(p, mean = port.expret, sd = port.sd)
      
      # PLot unconditional dist
      baseplot <- ggplot(data = port.sim, aes(Return)) +
        geom_histogram(colour = 'black', fill = 'white', binwidth = bw) +
        geom_density(aes(y = bw * ..count..), alpha = 0.5, fill = 'yellow') +
        stat_function(fun = function(x) dnorm(x, mean = port.expret, sd = port.sd) * bw * num.uncond, geom = 'area', fill = 'red', alpha = 0.4) +
        geom_vline(xintercept = var.garch , color = 'black') + geom_vline(xintercept = var.gaus, color = 'red') +
        ylab("") + labs(title = paste('Simulated Unconditional Daily Returns\nGARCH-Copula VaR (black line):', round(var.garch*100, digits = 2),'%\nGaussian VaR (red line):', round(var.gaus*100, digits = 2), '%')) + 
        scale_y_continuous(breaks = NULL) + 
        scale_fill_continuous(guide = guide_legend())
      
      # 'Full' histrogram
      uncond.dist <- baseplot + xlab("Unconditional Portfolio Daily Returns") + 
                                xlim(-0.05, 0.05)

      # Tail close-up
      uncond.tail <- baseplot + xlab("Close-up of Tail") + 
                                xlim(min(port.sim$Return, na.rm=TRUE), var.gaus)
                                
      grid.arrange(uncond.dist, uncond.tail, ncol = 2)
  
    })
    
  }


#' ---------------------------------------------------------------------------
#' RUN APP
#' ---------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
