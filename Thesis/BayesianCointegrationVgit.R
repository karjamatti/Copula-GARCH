#' --------------------------------------
#' A BAYESIAN COINTEGRATION TESTING PROCEDURE WITH CONDITIONAL PRIOR ODDS
#' --------------------------------------

# Load or install and load packages
'package' <- function(library, repo = getOption('repos')){ 
  if(!is.element(library, .packages(all.available = TRUE))) {install.packages(library)}
  library(library,character.only = TRUE)}

'package'('dplyr')
'package'('ggplot2')
'package'('lubridate')
'package'('Rfast')
'package'('data.table')
'package'('bit64')
'package'('ff')
'package'('fastDummies')

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS PARAMETERS
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Safety parameters (the values of these will determine whether computationally demanding procedures are performned)
update_functions = TRUE # The functions can be "locked" by setting this to false
resample = FALSE # This determines whether the Bayes factors in the training sample are re-calculated
retest = TRUE # This determines whether the Bayes factors in the testing sample are re-calculated

# Sample start and end dates
training.start <- lubridate::ymd('2009-01-01')
training.end <- lubridate::ymd('2013-12-31')
testing.start <- lubridate::ymd('2014-01-01')
testing.end <- lubridate::ymd('2018-12-31')

# Evaluation period lengths
p.len = 12 # lenth of testing evaluation (total)
p.freq = 6 # Frequency of testing evaluations
train.p.len <- train.p.freq <- 12 # training evaluation period length
N.samples = 9 # Total number of samples in testing data

GLOBAL.uselog = FALSE # Use log prices?
GLOBAL.usecoefs = FALSE # Use precalculated coefficients in Bayes factor calculations?

dashline <- '--------------------------------------------------' # Used as a separator when printing out script execution status 

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA DATA 
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

if ('crsp' %>% {!exists(.)}){
  
  dir <- 'C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Processed_Data'
  setwd(dir)
  stopifnot(all.equal(getwd(), dir))
  
  crsp <- fread('processed_data.csv', sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  

  crsp$Date <- lubridate::ymd(crsp$Date) # Convert dates to date objects
  crsp$Month <- lubridate::ymd(crsp$Month)
}

head(crsp) # Check structure

#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS FUNCTIONS
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------

#'-----------------------------------------------------
#' Partition Data into Sample
#'-----------------------------------------------------

if ('Get Sample' %>% {!exists(.)} || update_functions){
  'Get Sample' <- function(df, monthlist, p.num, p.len, p.freq, training = TRUE){
    
    first <- (p.num - 1) * p.freq + 1
    last <- first + p.len - 1
    months <- monthlist[first:min(last, monthlist %>% length())]
    
    if (!training){
      inv <- p.len - p.freq + 1 # Investment point
      cusips <- subset(df, (Month == months[inv]) & (ml >= bp1)) %>% {pull(., Cusip)}
    } else {
      cusips <- subset(df, (Month == months[1]) & (ml < bp1) & (ml >= bp2)) %>% {pull(., Cusip)}
    }
    
    sample <- subset(df, (Month %in% months) & (Cusip %in% cusips)) # filter, if date in months
    sample %>% return()
  }
}

#'-----------------------------------------------------
#' Get CUSIP pairs
#'-----------------------------------------------------

if ('Get Pairs' %>% {!exists(.)} || update_functions){
  'Get Pairs' <- function(sample){
    
    cusips <- sample$Cusip %>% unique()
    combn(cusips, 2, simplify = TRUE) %>% return()
  }
}

#'-----------------------------------------------------
#' LINEAR REGRESSION
#'-----------------------------------------------------

if ('Linear Regression' %>% {!exists(.)} || update_functions){
  'Linear Regression' <- function(x, y){
    
    slope <- cor(x, y)*(sd(y)/sd(x))
    intercept <- mean(y) - (slope*mean(x))
    std_eta <- sd(y - intercept - slope*x)
    list('slope' = slope,
         'intercept' = intercept,
         'std_eta' = std_eta) %>% return()
  }
}

#'-----------------------------------------------------
#' LOG SUM EXP
#'-----------------------------------------------------

if ('Log Sum Exp' %>% {!exists(.)} || update_functions){
  'Log Sum Exp' <- function(a, b){
    
    amax <- a %>% Re() %>% max()
    if (amax == -Inf) {amax <- 0}
    ((exp(a - amax)*b) %>% sum() %>% as.complex() %>% log() + amax) %>% return()
  }
}

#'-----------------------------------------------------
#' GET LOG AREA
#'-----------------------------------------------------

if ('Get Log Area' %>% {!exists(.)} || update_functions){
  'Get Log Area' <- function(logf, logF){
    
    lncdf <- pnorm(c(1, -1),
                   mean = logf %>% exp() %>% Re(),
                   sd = (0.5*logF) %>% exp() %>% Re(), 
                   log.p = TRUE)
    logarea <- ('Log Sum Exp'(lncdf, c(1, -1)) - log(2)) %>% Re()
    logarea %>% return()
  }
}

#'-----------------------------------------------------
#' CALCULATE MOMENTS LOG
#'-----------------------------------------------------

if ('Get Moments Log' %>% {!exists(.)} || update_functions){
  'Get Moments Log' <- function(logf, logF, logarea){
    
    lnpdf <- dnorm(c(1, -1),
                   mean = logf %>% exp() %>% Re(),
                   sd = (0.5*logF) %>% exp() %>% Re(),
                   log = TRUE)
    
    logmoment1 <- 'Log Sum Exp'(c(lnpdf[1] + logF - logarea,
                                  lnpdf[2] + logF - logarea,
                                  logf), 
                                c(-0.5, 0.5, 1))
    
    logmoment2 <- 'Log Sum Exp'(c(logF + logf + lnpdf[1] - logarea,
                                  logF + logf + lnpdf[2] - logarea,
                                  logF + lnpdf[1] - logarea,
                                  logF + lnpdf[2] - logarea,
                                  2*logf,
                                  logF),
                                c(-0.5, 0.5, -0.5, -0.5, 1, 1))
    
    list('moment1' = logmoment1 %>% exp() %>% Re(), 
         'moment2' = logmoment2 %>% exp() %>% Re()) %>% return()
  }
}

#'-----------------------------------------------------
#' FILTERING
#'-----------------------------------------------------

if ('Filter function' %>% {!exists(.)} || update_functions){
  'Filter function' <- function(V,std_eta){
    
    T <- length(V)
    
    # DIRECT METHOD
    logft <- (V[2:T]*V[1:T-1]) %>% sum() %>% as.complex() %>% log() - V[1:T-1]^2 %>% sum() %>% log()
    logFt <- 2*log(std_eta) - V[1:T-1]^2 %>% sum() %>% log()
    logft %>% {stopifnot(!is.nan(.))} #logft must be real
    logFt %>% {stopifnot(!is.nan(.))} #logFt must be real
    logFt %>% {stopifnot(is.double(.))} #logFt must be real
    
    logarea <- 'Get Log Area'(logft, logFt)
    loglik <- -0.5*log(sum(V[1:T-1]^2)) - 0.5*(T-2)*log(2*pi*std_eta^2) + logarea - (sum(V[2:T]^2) - sum(V[2:T]*V[1:T-1])^2/sum(V[1:T-1]^2))/(2*std_eta^2)
    
    # calculate moments
    moments <- 'Get Moments Log'(logft, logFt, logarea)
    
    list('loglik' = loglik,
         'moment1' = moments$moment1,
         'moment2' = moments$moment2,
         'phi_mean' = logft %>% exp()) %>% return()
  }
}

#'-----------------------------------------------------
#' UPDATE EXPECTATION MAXIMATION
#'-----------------------------------------------------

if ('Update EM' %>% {!exists(.)} || update_functions){
  'Update EM' <- function(x,y,moment1,moment2){
    
    T <- length(x)
    xt <- x[2:T]
    xtm1 <- x[1:T-1]
    yt <- y[2:T]
    ytm1 <- y[1:T-1]
    
    # find the coefficients
    a <- 2*(T-1)*moment1 - (T-1)*moment2 - (T-1)
    b <- moment1*sum(xt + xtm1) - moment2*sum(xtm1) - sum(xt)
    c <- moment2*sum(ytm1) - moment1*sum(yt + ytm1) + sum(yt)
    d <- 2*moment1*sum(xt*xtm1) - moment2*sum(xtm1^2) - sum(xt^2)
    e <- moment2*sum(xtm1*ytm1) - moment1*sum(xtm1*yt + xt*ytm1) + sum(xt*yt)
    
    # solve simultaneous equations
    slope <- ((a*e) - (c*b)) / ((b^2) - (a*d))
    intercept <- (-slope*d/b) - (e/b)
    
    # now find optimal sigma
    eps <- y - intercept - slope * x
    ept <- eps[2:T]
    eptm1 <- eps[1:T-1]
    std_eta <- sqrt((sum(ept^2) - 2*moment1*sum(ept*eptm1) + moment2*sum(eptm1^2))/(T-1))
    
    std_eta %>% {stopifnot(. > 0)} #Standard deviation must be positive
    std_eta %>% {stopifnot(is.double(.))} #Standard deviation must be real
    
    list('slope' = slope, 
         'intercept' = intercept, 
         'std_eta' = std_eta) %>% return()
  }
}

#'-----------------------------------------------------
#' INFERENCE FUNCTION
#'-----------------------------------------------------

if ('Get Inference' %>% {!exists(.)} || update_functions){
  'Get Inference' <- function(epsilon, std_eta, x, y){
    
    Filtering <- 'Filter function'(epsilon, std_eta)
    update <- 'Update EM'(x, y, Filtering$moment1, Filtering$moment2)
    
    list('loglik' = Filtering$loglik,
         'slope' = update$slope,
         'intercept' = update$intercept,
         'std_eta' = update$std_eta,
         'phi_mean' = Filtering$phi_mean) %>% return()
  }
}

#'-----------------------------------------------------
#' Bayesian Cointegration Tests
#'-----------------------------------------------------

# WITH UNKNOWN PARAMETERS

if ('Bayesian Test' %>% {!exists(.)} || update_functions){
  'Bayesian Test' <- function(x, y, coefs = FALSE){
    
    T <- length(x)
    ols <- 'Linear Regression'(x,y)
    slope <- ols$slope
    intercept <- ols$intercept
    std_eta_coint <- ols$std_eta
    
    # cointegrated case - learn slope, intercept, std by ML
    logliks <- c(-Inf)
    for (i in 1:1000){
      intercept %>% {stopifnot(!is.nan(.))} #Intercept cannot be nan
      slope %>% {stopifnot(!is.nan(.))} #Slope cannot be nan
      std_eta_coint %>% {stopifnot(is.double(.))} #Standard deviation must be real
      std_eta_coint %>% {stopifnot(. > 0)} #Standard deviation must be greater than 0
      
      inference <- 'Get Inference'(y - intercept - slope*x, std_eta_coint, x, y)
      slope <- inference$slope
      intercept <- inference$intercept
      std_eta_coint <- inference$std_eta
      phi_mean <- inference$phi_mean
      
      if (inference$loglik - tail(logliks, n=1) < 0.00001){break}
      logliks <- c(logliks, inference$loglik)
    }
    
    # non-cointegrated case - use above slope, intercept, use ML std
    epsilon <- y - intercept - slope*x
    std_eta_p1 <- (epsilon[2:T]-epsilon[1:T-1])^2 %>% mean() %>% sqrt()
    loglik_p1 <- dnorm(epsilon[2:T], mean = epsilon[1:T-1], sd = std_eta_p1, log = TRUE) %>% sum()
    
    bayes_factor <- exp(inference$loglik - loglik_p1)
    if (!coefs){bayes_factor %>% return()} else {c(bayes_factor, intercept, slope, phi_mean %>% Re(), std_eta_coint) %>% return()}
  }
}

# WITH PREDETERMINED PARAMETERS

if ('Bayesian ReTest' %>% {!exists(.)} || update_functions){
  'Bayesian ReTest' <- function(x, y, slope, intercept, std_eta_coint){
    
    T <- length(x)

    # cointegrated case - learn slope, intercept, std by ML
    intercept %>% {stopifnot(!is.nan(.))} #Intercept cannot be nan
    slope %>% {stopifnot(!is.nan(.))} #Slope cannot be nan
    std_eta_coint %>% {stopifnot(is.double(.))} #Standard deviation must be real
    std_eta_coint %>% {stopifnot(. > 0)} #Standard deviation must be greater than 0
    
    inference <- 'Get Inference'(y - intercept - slope*x, std_eta_coint, x, y)
    phi_mean <- inference$phi_mean
    
    # non-cointegrated case - use above slope, intercept, use ML std
    epsilon <- y - intercept - slope*x
    std_eta_p1 <- (epsilon[2:T]-epsilon[1:T-1])^2 %>% mean() %>% sqrt()
    loglik_p1 <- dnorm(epsilon[2:T], mean = epsilon[1:T-1], sd = std_eta_p1, log = TRUE) %>% sum()
    
    bayes_factor <- exp(inference$loglik - loglik_p1)
    c(bayes_factor, intercept, slope, phi_mean %>% Re(), std_eta_coint) %>% return()
  }
}

#'-----------------------------------------------------
#' Get Bayes factor of cointegration for a pair of stocks
#'-----------------------------------------------------

if ('Get BF' %>% {!exists(.)} || update_functions){
  'Get BF' <- function(sample, cusips, get_coefs = FALSE, pseudo = FALSE){
    
    C1 <- cusips[1]
    C2 <- cusips[2]
    
    if (GLOBAL.uselog){
      ts1 <- sample %>% {subset(., Cusip == C1)} %>% {pull(., LogTruePrice)}
      ts2 <- sample %>% {subset(., Cusip == C2)} %>% {pull(., LogTruePrice)}
    } else {
      ts1 <- sample %>% {subset(., Cusip == C1)} %>% {pull(., TruePrice)}
      ts2 <- sample %>% {subset(., Cusip == C2)} %>% {pull(., TruePrice)}
    }
    
    if (get_coefs == FALSE){
      if ((ts1 %>% length()) == (ts2 %>% length())){
        if (!pseudo){
          'Bayesian Test'(ts1, ts2, coefs = FALSE) %>% return()  
        } else {1 %>% return()}
      } else {-1 %>% return()}
    } else {
      if ((ts1 %>% length()) == (ts2 %>% length())){
        if (!pseudo){
          'Bayesian Test'(ts1, ts2, coefs = TRUE) %>% return()  
        } else {c(1, 0, 0) %>% return()}
      } else {c(-1,0,0) %>% return()}
    }
  }
}

#'-----------------------------------------------------
#' General Attribute retrieval
#'-----------------------------------------------------

if ('Get Attribute' %>% {!exists(.)} || update_functions){
  'Get Attribute' <- function(cusip, attribute, sample, as_char = TRUE){
    
    output <- sample %>% {dplyr::filter(., Cusip == cusip)} %>% pull(attribute) %>% {dplyr::first(.)}
    if(as_char){output %>% as.character() %>% return()} else {output %>% return()}
  }  
}

#'-----------------------------------------------------
#' NAICS Similarity
#'-----------------------------------------------------

if ('NAIC' %>% {!exists(.)} || update_functions){
  'NAIC' <- function(cusip, sample){
    
    naic <- 'Get Attribute'(cusip, 'NAIC', sample)
    sector <- naic %>% {substr(., 1, 2)}
    subsector <- naic %>% {substr(., 1, 3)}
    indgroup <- naic %>% {substr(., 1, 4)}
    industry <- naic %>% {substr(., 1, 5)}
    natind <- naic %>% {substr(., 1, 6)}
    list(sector, subsector, indgroup, industry, natind) %>% return()
  }  
}

if ('NAIC similarity' %>% {!exists(.)} || update_functions){
  'NAIC similarity' <- function(cusip1, cusip2, sample){
    
    sector.weight = 1
    subsector.weight = 1
    indgroup.weight = 1
    industry.weight = 1
    natind.weight = 1
    X.NAIC <- 'NAIC'(cusip1, sample)
    Y.NAIC <- 'NAIC'(cusip2, sample)
    sector <- ifelse(X.NAIC[[1]] == Y.NAIC[[1]], sector.weight, 0)
    subsector <- ifelse(X.NAIC[[2]] == Y.NAIC[[2]], subsector.weight, 0)
    indgroup <- ifelse(X.NAIC[[3]] == Y.NAIC[[3]], indgroup.weight, 0)
    industry <- ifelse(X.NAIC[[4]] == Y.NAIC[[4]], industry.weight, 0)
    natind <- ifelse(X.NAIC[[5]] == Y.NAIC[[5]], natind.weight, 0)
    sum(sector, subsector, indgroup, industry, natind) %>% return()
  }
}

#'-----------------------------------------------------
#' General Numeric Similarity
#'-----------------------------------------------------

if ('Similarity' %>% {!exists(.)} || update_functions){
  'Similarity' <- function(cusip1, cusip2, sample, attribute){
    
    X <- 'Get Attribute'(cusip1, attribute, sample, as_char = FALSE)
    Y <- 'Get Attribute'(cusip2, attribute, sample, as_char = FALSE)
    
    similarity <- 10 - abs(X - Y)
    similarity %>% return()
  }
}

#'-----------------------------------------------------
#' Get Candidate pairs
#'-----------------------------------------------------

if ('Test BF' %>% {!exists(.)} || update_functions){
  'Test BF' <- function(sample, training = FALSE, pseudo = FALSE){
    
    cusip1 <- vector(mode = 'character')
    cusip2 <- vector(mode = 'character')
    bf <- naic.similarity <- mc.similarity <- pb.similarity <- numeric()
    cusips <- 'Get Pairs'(sample)
    if (!training){intercepts <- slopes <- phi_means <- std_etas <- numeric()}
    
    for (i in 1:(cusips %>% ncol())){
      
      if (i %% 1000 == 0){cat("Evaluation", i, "of", cusips %>% ncol(), '\n')}
      if (training){
        bayes <- 'Get BF'(sample, cusips[, i], get_coefs = FALSE, pseudo = pseudo)
      } else {
        input <- 'Get BF'(sample, cusips[, i], get_coefs = TRUE, pseudo = pseudo)
        bayes <- input[1]
        inter <- input[2]
        slope <- input[3]
        phi_mean <- input[4]
        std_eta <- input[5]
      }
      
      if (!pseudo){
        if (bayes >= 0){
          cusip1 <- cusips[1, i] %>% as.character() %>% {c(cusip1, .)}
          cusip2 <- cusips[2, i] %>% as.character() %>% {c(cusip2, .)}
          bf <- c(bf, bayes)
          naic.similarity <- c(naic.similarity, 'NAIC similarity'(cusips[1, i], cusips[2, i], sample))
          mc.similarity <- c(mc.similarity, 'Similarity'(cusips[1, i], cusips[2, i], sample, 'MC.dec'))
          pb.similarity <- c(pb.similarity, 'Similarity'(cusips[1, i], cusips[2, i], sample, 'PB.dec'))
          
          if (!training){
            intercepts <- c(intercepts, inter)
            slopes <- c(slopes, slope)
            phi_means <- c(phi_means, phi_mean)
            std_etas <- c(std_etas, std_eta)
          }
        }  
      } else {
        cusip1 <- cusips[1, i] %>% as.character() %>% {c(cusip1, .)}
        cusip2 <- cusips[2, i] %>% as.character() %>% {c(cusip2, .)}
        bf <- c(bf, bayes)
      }
      
    }
    
    if (pseudo){
      output <- cbind.data.frame(cusip1,
                                 cusip2,
                                 bf)
    } else {
      if (training){output <- cbind.data.frame(cusip1,
                                               cusip2, 
                                               bf, 
                                               naic.similarity, 
                                               mc.similarity, 
                                               pb.similarity)
      
      } else {output <- cbind.data.frame(cusip1,
                                         cusip2,
                                         bf,
                                         naic.similarity,
                                         mc.similarity,
                                         pb.similarity,
                                         intercepts,
                                         slopes,
                                         phi_means,
                                         std_etas)}
    }
    
    output %>% return()
  }
}

#'-----------------------------------------------------
#' Training a Logistic Model
#'-----------------------------------------------------

if ('Train Model' %>% {!exists(.)} || update_functions){
  'Train Model' <- function(pairlist, threshold = 1, dummies = FALSE){
    
    pairlist$Y <- ifelse(pairlist$bf > threshold, 1, 0)
    
    assign("c", sum(pairlist$Y) / nrow(pairlist), envir = .GlobalEnv)
    
    if (dummies){
      
      pairlist <- dummy_cols(pairlist, 
                             select_columns = "naic.similarity", 
                             remove_first_dummy = TRUE,
                             remove_selected_columns = TRUE)
      
      logmodel <- glm(Y ~ naic.similarity_1 +
                        naic.similarity_2 +
                        naic.similarity_3 +
                        naic.similarity_4 +
                        naic.similarity_5 +
                        mc.similarity +
                        pb.similarity,
                        family = binomial(link='logit'), data = pairlist) %>% summary()
    } else {
      logmodel <- glm(Y ~ naic.similarity +
                        mc.similarity +
                        pb.similarity,
                        family = binomial(link='logit'), data = pairlist) %>% summary()
    }

    logmodel$coefficients %>% return()
  }
}

#'-----------------------------------------------------
#' Training Sampler
#'-----------------------------------------------------

if ('Training Sampler' %>% {!exists(.)} || update_functions){
  'Training Sampler' <- function(data, p.len = train.p.len, p.freq = train.p.freq, pooled = TRUE, pseudo = FALSE){
    
    mlist <- data %>% {pull(., Month)} %>% unique() %>% sort()
    counter <- ((length(mlist) - p.len) / p.freq) %>% ceiling() + 1
    
    if (pooled){
      
      if (!pseudo){
        pooled.sample <- data.frame(cusip1 = character(0), 
                                    cusip2 = character(0), 
                                    bf = numeric(0), 
                                    naic.similarity = numeric(0), 
                                    mc.similarity = numeric(0),
                                    pb.similarity = numeric(0))
      } else {
        pooled.sample <- data.frame(cusip1 = character(0), 
                                    cusip2 = character(0), 
                                    bf = numeric(0))
      }
      
      for (sample in 1:counter){
        cat("Bayesian Cointegration Testing: Working on subsample number", sample, "of", counter, '\n')
        testsample <- 'Get Sample'(data, mlist, p.num = sample, p.len, p.freq)
        testlist <- 'Test BF'(testsample, training = TRUE, pseudo = pseudo)
        pooled.sample <- rbind(pooled.sample, testlist)
        cat(dashline, '\n')
      }
      
      if (GLOBAL.uselog){
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Training_Data/LogTruePrice/', p.len, 'm')  
      } else {
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Training_Data/TruePrice/', p.len, 'm')
      }
      
      setwd(dir)
      stopifnot(all.equal(getwd(), dir))
      
      if (!pseudo){
        filename <- 'pooled_sample_200.csv'  
      } else {
        filename <- 'pooled_pseudo_200.csv'
      }
      
      fwrite(pooled.sample, filename) # Export processed data
      pooled.sample %>% return()
      
    } else {
      # SOMETHING HERE MAYBE LATER...
    }
  }
}


#'-----------------------------------------------------
#' Testing Sampler
#'-----------------------------------------------------

if ('Testing Sampler' %>% {!exists(.)} || update_functions){
  'Testing Sampler' <- function(data, p.len = 12, p.freq = 6){
    
    if (GLOBAL.uselog){
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/LogTruePrice/', p.freq, 'm')  
    } else {
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/TruePrice/', p.freq, 'm')  
    }
    
    setwd(dir)
    stopifnot(all.equal(getwd(), dir))
    
    mlist <- data %>% {pull(., Month)} %>% unique() %>% sort()
    counter <- ((length(mlist) - p.len) / p.freq) %>% ceiling() + 1
    
    for (sample in 1:counter){
      cat("Working on subsample number", sample, "of", counter, '\n')
      sample.window <- 'Get Sample'(data, mlist, p.num = sample, p.len, p.freq, training = FALSE)
      
      sample.mlist <- sample.window %>% {pull(., Month)} %>% unique() %>% sort()
      inv.point <- sample.mlist[p.len - p.freq + 1]
      eval.sample <- sample.window %>% {dplyr::filter(., (Month < inv.point))}
      inv.sample <- sample.window %>% {dplyr::filter(., (Month >= inv.point))}
      
      eval.list <- 'Test BF'(eval.sample, training = FALSE)
      
      inv.filename <- paste0('inv_sample_', sample, '.csv')
      fwrite(inv.sample, inv.filename) # Export processed data  
      
      eval.filename <- paste0('eval_list_', sample, '.csv')
      fwrite(eval.list, eval.filename) # Export processed data  
      
      rm(sample.mlist, inv.point, eval.sample, inv.sample, eval.list, inv.filename, eval.filename)
    } 
  }
}

#'-----------------------------------------------------
#' BF persistence
#'-----------------------------------------------------

if ('BF ReTest' %>% {!exists(.)} || update_functions){
  'BF ReTest' <- function(candidates, sample, j, coefs = FALSE){
    
    candidates1 <- candidates %>% pull(cusip1)
    candidates2 <- candidates %>% pull(cusip2)
    slopes <- candidates %>% pull(slopes)
    intercepts <- candidates %>% pull(intercepts)
    std_etas <- candidates %>% pull(std_etas)
    pair.names <- paste0(candidates1, '_', candidates2)
    
    counter <- candidates %>% nrow()
    insample.bfs <- counter %>% numeric() # Initalize vector
    insample.phis <- counter %>% numeric() # Initalize vector
    
    for (i in 1:counter){
      
      if (i %% 100 == 0){cat("Bayes factor ReTesting: Evaluation", i, "of", counter, '\n')}
      
      if (GLOBAL.uselog){
        x.ts <- sample %>% {dplyr::filter(., Cusip == candidates1[i])} %>% pull(LogTruePrice)
        y.ts <- sample %>% {dplyr::filter(., Cusip == candidates2[i])} %>% pull(LogTruePrice)
      } else {
        x.ts <- sample %>% {dplyr::filter(., Cusip == candidates1[i])} %>% pull(TruePrice)
        y.ts <- sample %>% {dplyr::filter(., Cusip == candidates2[i])} %>% pull(TruePrice)  
      }
      
      if ((x.ts %>% length()) == (y.ts %>% length())){
        
        if (coefs){
          input <- 'Bayesian ReTest'(x.ts, y.ts, slopes[i], intercepts[i], std_etas[i])  
        } else {
          input <- 'Bayesian Test'(x.ts, y.ts, coefs = TRUE)  
        }
        
        insample.bfs[i] <- input[1]
        insample.phis[i] <- input[4]
      }
    }
    
    ReTest.df <- data.frame(Key = pair.names,
                            BF2 = insample.bfs,
                            phi2 = insample.phis)
    
    if (GLOBAL.uselog){
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/LogTruePrice/', p.freq, 'm')  
    } else {
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/TruePrice/', p.freq, 'm') 
    }

    setwd(dir)
    stopifnot(all.equal(getwd(), dir))
    
    if (coefs){
      filename <- paste0('BF_ReTest_OldCoef_', j, '.csv')
    } else {
      filename <- paste0('BF_ReTest_NewCoef_', j, '.csv')    
    }
    
    fwrite(ReTest.df, filename) # Export processed data
  }
}


#'-----------------------------------------------------
#' Trading Signals
#'-----------------------------------------------------

if ('ReTest Sampler' %>% {!exists(.)} || update_functions){
  'ReTest Sampler' <- function(inv.samples = NULL, threshold = 0){
    
    counter <- inv.samples
    
    for (i in 1:counter){ 
      
      if (GLOBAL.uselog){
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/LogTruePrice/', p.freq, 'm')  
      } else {
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/TruePrice/', p.freq, 'm') 
      }
      
      setwd(dir)
      stopifnot(all.equal(getwd(), dir))
      
      inv.file <- paste0('inv_sample_', i, '.csv')
      inv.sample <- fread(inv.file, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
      
      eval.file <- paste0('eval_list_', i, '.csv')
      eval.list <- fread(eval.file, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
      
      cat("Calculating In-sample BFs... Working on sample", i, "of", counter, '\n')
      cat(dashline, '\n')
      
      if (GLOBAL.usecoefs){
        'BF ReTest'(eval.list, inv.sample, j = i, coefs = TRUE)
      } else {
        'BF ReTest'(eval.list, inv.sample, j = i, coefs = FALSE) 
      }
      
      cat(dashline, '\n\n', dashline, '\n')
    }
  }
}

if ('ReOdds Sampler' %>% {!exists(.)} || update_functions){
  'ReOdds Sampler' <- function(inv.samples = NULL, model, threshold = 0){
    
    counter <- inv.samples
    
    for (i in 1:counter){ 
      
      if (GLOBAL.uselog){
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/LogTruePrice/', p.freq, 'm')  
      } else {
        dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Testing_Data/TruePrice/', p.freq, 'm') 
      }
      
      setwd(dir)
      stopifnot(all.equal(getwd(), dir))
      
      eval.file <- paste0('eval_list_', i, '.csv')
      eval.list <- fread(eval.file, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
      
      log.int <- model$Estimate[1]
      naic.coef <- model$Estimate[2]
      mc.coef <- model$Estimate[3]
      pb.coef <- model$Estimate[4]
      
      reg.val <- log.int + naic.coef*eval.list$naic.similarity + mc.coef*eval.list$mc.similarity + pb.coef*eval.list$pb.similarity
      
      prior.pr <- 1 / (1 + exp(-(reg.val)))
      eval.list$priorodds <- odds <- prior.pr / (1 - prior.pr)
      scaler <- c/(1-c)
      eval.list$scaledodds <- odds / scaler
      
      cat("Calculating odds... Working on sample", i, "of", counter, '\n')
      cat(dashline, '\n')
      
      if (GLOBAL.usecoefs){
        BF.filename <- paste0('BF_ReTest_OldCoef_', i, '.csv')
      } else {
        BF.filename <- paste0('BF_ReTest_NewCoef_', i, '.csv')    
      }

      BF.sample <- fread(BF.filename, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data
      
      eval.list$BF2 <- BF.sample$BF2
      eval.list$phi2 <- BF.sample$phi2
      names(eval.list)[names(eval.list) == 'bf'] <- 'BF1' # Rename column for 'Persistence.R'
      
      if (GLOBAL.usecoefs){
        out.filename <- paste0('ReTest_OldCoef_', i, '.csv')
      } else {
        out.filename <- paste0('ReTest_NewCoef_', i, '.csv')    
      }
      
      fwrite(eval.list, out.filename) # Export processed data
      
      cat(dashline, '\n\n', dashline, '\n')
    }
  }
}


#'----------------------------------------------------------------------------------------------------------------------------------------------------------------
#' TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST
#'----------------------------------------------------------------------------------------------------------------------------------------------------------------


#'-----------------------------------------------------
#' Training Data
#'-----------------------------------------------------


# TRAINING


rm(testvals, testpool)

if ('testpool' %>% {!exists(.)}){
  if (resample){
    
    training.data <- crsp %>% {dplyr::filter(., (Date >= training.start) & (Date <= training.end))}
    
    if ('testvals' %>% {!exists(.)}){testvals <- 'Training Sampler'(training.data, pseudo = FALSE)}
  } else {
    
    if (GLOBAL.uselog){
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Training_Data/LogTruePrice/', train.p.len, 'm')  
    } else {
      dir <- paste0('C:/Users/Matti/Documents/Aalto Biz/5. vuosi/Thesis/Training_Data/TruePrice/', train.p.len, 'm')
    }
    setwd(dir)
    stopifnot(all.equal(getwd(), dir))
    
    filename <- 'pooled_sample_200.csv'
    testvals <- fread(filename, sep =',', header = TRUE, stringsAsFactors = F) %>% as.data.frame() # Load data  
  }
  TRAIN.THRESH <- 20
  testpool <- 'Train Model'(testvals, threshold = TRAIN.THRESH, dummies = FALSE) %>% as.data.frame()
  testpool
}

th <- seq(1, 250, by = 2.5)
ind <- mkt <- btm <- numeric()
ind.se <- mkt.se <- btm.se <- numeric()

for (i in 1:length(th)){
  tp <- 'Train Model'(testvals, threshold = th[i])
  ind <- c(ind, tp[2,1])
  mkt <- c(mkt, tp[3,1])
  btm <- c(btm, tp[4,1])
  
  ind.se <- c(ind.se, tp[2,2])
  mkt.se <- c(mkt.se, tp[3,2])
  btm.se <- c(btm.se, tp[4,2])
}
coef.df <- data.frame('Threshold' = th,
                      'Industry' = ind, 
                      'MktCap' = mkt,
                      'BtM' = btm)

ribbon.ind <- data.frame(Threshold = th,
                          ind.025 = qnorm(0.025) * ind.se + ind,
                          ind.25 = qnorm(0.25) * ind.se + ind,
                          ind.75 = qnorm(0.75) * ind.se + ind,
                          ind.975 = qnorm(0.975) * ind.se + ind)

ribbon.mkt <- data.frame(Threshold = th,
                           mkt.025 = qnorm(0.025) * mkt.se + mkt,
                           mkt.25 = qnorm(0.25) * mkt.se + mkt,
                           mkt.75 = qnorm(0.75) * mkt.se + mkt,
                           mkt.975 = qnorm(0.975) * mkt.se + mkt)

ribbon.btm <- data.frame(Threshold = th,
                         btm.025 = qnorm(0.025) * btm.se + btm,
                         btm.25 = qnorm(0.25) * btm.se + btm,
                         btm.75 = qnorm(0.75) * btm.se + btm,
                         btm.975 = qnorm(0.975) * btm.se + btm)


coef.melt <- coef.df %>% {melt(., id = 'Threshold')}
plot <- ggplot() +
  geom_line(data = coef.melt, aes(x = Threshold, y = value, color = variable, size = variable)) +
  scale_color_manual(values = c('darkorange', 'darkgreen', 'navyblue')) +
  geom_ribbon(data = ribbon.ind, aes(x = Threshold, ymin = ind.025, ymax = ind.975), fill = 'darkorange', alpha = 0.2) +
  geom_ribbon(data = ribbon.ind, aes(x = Threshold, ymin = ind.25, ymax = ind.75), fill = 'darkorange', alpha = 0.2) +
  geom_ribbon(data = ribbon.mkt, aes(x = Threshold, ymin = mkt.025, ymax = mkt.975), fill = 'darkgreen', alpha = 0.2) +
  geom_ribbon(data = ribbon.mkt, aes(x = Threshold, ymin = mkt.25, ymax = mkt.75), fill = 'darkgreen', alpha = 0.2) +
  geom_ribbon(data = ribbon.btm, aes(x = Threshold, ymin = btm.025, ymax = btm.975), fill = 'navyblue', alpha = 0.2) +
  geom_ribbon(data = ribbon.btm, aes(x = Threshold, ymin = btm.25, ymax = btm.75), fill = 'navyblue', alpha = 0.2) +
  scale_size_manual(values = c(0.75, 0.75, 0.75)) + theme_bw() + xlab('Classification threshold') + ylab('Coefficient value') +
  scale_y_continuous(breaks = seq(-0.5, 0.2, 0.1)) +
  theme(legend.position = 'bottom', legend.title = element_blank(), legend.text=element_text(size=11), text=element_text(family="serif"))
plot

#'-----------------------------------------------------
#' Testing Data
#'-----------------------------------------------------


if (retest){
  testing.data <- crsp %>% {dplyr::filter(., (Date >= testing.start) & (Date <= testing.end))}
  'Testing Sampler'(data = testing.data, p.len = p.len, p.freq = p.freq)
}


# Use this if you want to re-calculate the Bayes factors
'ReTest Sampler'(inv.samples = N.samples, threshold = 0)

# Use this if you want to only recalculate the conditional prior odds
'ReOdds Sampler'(inv.samples = N.samples, model = testpool, threshold = 0)
