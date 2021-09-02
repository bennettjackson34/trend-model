getSMMA = function(xt,n_smooth=3){
  # calculate smooth moving average of price
  x = as.vector(xt)
  x_valid = x[!is.na(x)]
  if(length(x_valid)<n_smooth){
    warning('Smooth length is longer than data length.')
    return(x*NA)
  }else{
    skipped = which(!is.na(x))[1] - 1
    SMMA = x*NA
    n_valid = 0
    last = NA
    for(i in (skipped + 1):length(x)){
      if(!is.na(x[i])){
        n_valid = n_valid + 1
        if(n_valid==n_smooth){
          SMMA[i] = mean(x[1:i],na.rm = T)
          last = SMMA[i]
          # print(last)
        }else if(n_valid>n_smooth){
          SMMA[i] = (last * (n_smooth-1) + x[i]) / n_smooth
          last = SMMA[i]
        }
      }
    }
    #warning(paste(skipped),' data points were skipped.')
    return(SMMA)
    
  }
}


getATR = function(data, n_smooth = 20){
  valid = !is.na(rowSums(data[,c('Open','High','Low','Close')]))
  res =  matrix(NA,nrow = dim(data)[1],ncol = 1,dimnames = list(NULL,'ATR'))
  if(sum(valid)>0){
    hi = data[,'High']
    lo = data[,'Low']
    cl = data[,'Close']
  }else{
    hi = data[,'Close']
    lo = data[,'Close']
    cl = data[,'Close']
  }
  
  
  truehigh = hi
  temph = c(hi[1],cl[1:(length(cl)-1)])
  truehigh[hi<temph] = temph[hi<temph]
  truelow = lo
  templ = c(lo[1],cl[1:(length(cl)-1)])
  truelow[lo>templ] = templ[lo>templ]
  
  atr = getSMMA(truehigh - truelow,n_smooth)
  res =  matrix(NA,nrow = dim(data)[1],ncol = 1,dimnames = list(NULL,'ATR'))
  res[] = atr
  
  return(res)
}


# TODO 

getConfirmationDates <- function(x,peaks){
  res <- list()
  for(day in as.numeric(unlist(peaks))){
    # 1 == peak low
    # -1 == peak high
    
    type <- ifelse(day %in% peaks$Highs, -1, 1)
    extreme_pos <- ifelse(type==1,"High","Low")
    extreme_neg <- ifelse(type==-1,"Low","High")
    
    range <- c((day+1):nrow(x))
    threshold <- type*min(type*x[day:(day+1),extreme_pos])
    confirm_day <- range[first(which(type*x[range,extreme_pos]  > type*threshold))]
    
    res[[as.character(day)]] <- confirm_day
  }
  
  return(sort(unlist(res)))
  
}

getChannels <- function(x,range, peaks, trend){
  data <- x[range,]
  extreme_pos <- ifelse(trend==1,"High","Low")
  extreme_neg <- ifelse(trend==1,"Low","High")
  trend_extreme <- as.numeric(data[1,extreme_neg])
  trend_extreme_day <- 1
  
  #browser()
  res <- list(days=c(),size=c(), current_rotation_size = 0, current_rotation_day=0)
  for(day in 2:length(range)){
    #browser()
    if(trend* as.numeric(data[day,extreme_pos]) > trend* trend_extreme){
      # We've made a new trend extreme
      new_trend_extreme <- as.numeric(data[day, extreme_pos])
      # look for the peak that separate the drive to new extreme
      candidate_peaks <- peaks[which(peaks > range[trend_extreme_day] & peaks <= range[day])]
      
      if(length(candidate_peaks)){
        # we have a peak between the two trend extreme
        #browser()
        channel_day <- candidate_peaks[first(which(trend* x[candidate_peaks,extreme_neg] == min(trend* x[candidate_peaks,extreme_neg])))]
        channel_size <- abs(trend_extreme - as.numeric(x[channel_day,extreme_neg]))
        res$days <- c(res$days, channel_day)
        res$size <- c(res$size, channel_size)
      }
      
      trend_extreme <- new_trend_extreme
      trend_extreme_day <- day
    }
    
    
  }
  
  # Find the size of the current roation
  current_rotation_range <- c(range[trend_extreme_day]:last(range))
  current_roation_peaks <- peaks[which(peaks %in% current_rotation_range)]
  
  if(length(current_roation_peaks)){
    current_peak_day <- current_roation_peaks[first(which(trend* x[current_roation_peaks,extreme_neg] == min(trend* x[current_roation_peaks,extreme_neg])))]
    current_rotation_size <- abs(as.numeric(x[current_peak_day,extreme_neg]) - trend*max(trend*x[c(current_peak_day:last(range)),extreme_pos]))
    
    res$current_rotation_day <- current_peak_day
    res$current_rotation_size <- current_rotation_size
  }
  
  return(res)
}

getStoch = function(hlc,n_period=20 ,n_smooth1=3 ,n_smooth2=3,re = 'both'){
  valid = !is.na(rowSums(hlc[,c('Open','High','Low','Close')]))
  res = matrix(NA,nrow = dim(hlc)[1],ncol = 2,dimnames = list(NULL,c('SlowK','SlowD')))
  if(sum(valid) > 0){
    hi = hlc[valid,'High']
    lo = hlc[valid,'Low']
    cl = hlc[valid,'Close']
    hmax = runMax(hi,n_period)
    lmin = runMin(lo,n_period)
    num = cl - lmin
    den = hmax - lmin
    fastK = num/den
    SlowK = getSMMA(fastK,n_smooth = n_smooth1)*100
    SlowD = getSMMA(SlowK,n_smooth = n_smooth2)
    res[valid,] = cbind(SlowK,SlowD)
  }
  if(re == 'both'){
    return(res)
  }else if(re == 'K'){
    return(res[,'SlowK'])
  }else if(re == 'D'){
    return(res[,'SlowD'])
  }
}

getPeakHigh <- function(x, day, res, env){
  if(!env$replace){
    hi <- x[,'High']
    lo <- x[,'Low']
    
    upper_threshold <- hi[day]
    
    #if(day == 141) browser()
    ### Check left side of peak ###
    left_lower_threshold <- max(lo[c(day,day-1)])
    left_range <- c(1:c(day-1))
    
    # get the first day to confirm the left side of the peak
    left_lower_confirm_day <- left_range[last(which(lo[left_range] < left_lower_threshold))]
    left_upper_confirm_day <- left_range[last(which(hi[left_range] < upper_threshold))]
    left_confirm_day <- min(left_lower_confirm_day,left_upper_confirm_day)
    
    # get the first day to violate the left side of the peak
    left_negate_day <- left_range[last(which(hi[left_range] > upper_threshold))]
    
    # check if confirm date is closer than negate date
    # left = T | F
    left <- ifelse(is.na(left_confirm_day),F, ifelse(is.na(left_negate_day), T , left_confirm_day >= left_negate_day))
    
    ### Check right side of peak ###
    right_lower_threshold <- max(lo[c(day,day+1)])
    right_range <- c((day+1):length(lo)) #candidate_peak_highs[min(length(candidate_peak_highs),which(candidate_peak_highs == day)+1)]) # only search up until the next candidate peak
    
    # get the first day to confirm the right side of the peak (only search up until the next candidate peak)
    right_lower_confirm_day <- right_range[first(which(lo[right_range] < right_lower_threshold))]
    right_upper_confirm_day <- right_range[first(which(hi[right_range] < upper_threshold))]
    right_confirm_day <- max(right_upper_confirm_day,right_lower_confirm_day)
    
    # get the first day to violate the right side of the peak
    right_negate_day <- right_range[first(which(hi[right_range] > upper_threshold))]
    
    # check if confirm date is closer than negate date
    # left = T | F
    right <- ifelse(is.na(right_confirm_day), F, ifelse(is.na(right_negate_day), T, right_confirm_day <= right_negate_day))
    
    if(left & right){
      if(env$last_peak == "High"){
        # last peak was a high, need to consider if this should overwrite the previous one 
        # overwrite if this new peak high is equal to or greater than the previous 
        env$replace <- hi[day] > hi[last(res$peaks$Highs)]
        
        if(env$replace){
          # clear out old ZZ level on the series and repalce with new 
          res$series[last(res$peaks$Highs)] <- NA
          res$series[day] <- hi[day]
          # replace peak high with today
          res$peaks$Highs[length(res$peaks$Highs)] <- day
        }
      }else{
        res$peaks$Highs <- c(res$peaks$Highs, day)
        res$series[day] <- hi[day]
        env$last_peak <- "High"
      }
    }
  }
  
  
  return(res)
}

getPeakLow <- function(x, day, res, env){
  if(!env$replace){
    # if we didnt overwrite a peak already 
    hi <- x[,'High']
    lo <- x[,'Low']
    
    lower_threshold <- lo[day]
    
    #if(day == 141) browser()
    ### Check left side of peak ###
    left_upper_threshold <- min(hi[c(day,day-1)])
    left_range <- c(1:c(day-1))
    
    # get the first day to confirm the left side of the peak
    left_upper_confirm_day <- left_range[last(which(hi[left_range] > left_upper_threshold))]
    left_lower_confirm_day <- left_range[last(which(lo[left_range] > lower_threshold))]
    left_confirm_day <- min(left_upper_confirm_day,left_lower_confirm_day)
    
    # get the first day to violate the left side of the peak
    left_negate_day <- left_range[last(which(lo[left_range] < lower_threshold))]
    
    # check if confirm date is closer than negate date
    # left = T | F
    left <- ifelse(is.na(left_confirm_day),F, ifelse(is.na(left_negate_day), T , left_confirm_day >= left_negate_day))
    
    ### Check right side of peak ###
    right_upper_threshold <- min(hi[c(day,day+1)])
    right_range <- c((day+1):length(lo)) #candidate_peak_highs[min(length(candidate_peak_highs),which(candidate_peak_highs == day)+1)]) # only search up until the next candidate peak
    
    # get the first day to confirm the right side of the peak (only search up until the next candidate peak)
    right_upper_confirm_day <- right_range[first(which(hi[right_range] > right_upper_threshold))]
    right_lower_confirm_day <- right_range[first(which(lo[right_range] > lower_threshold))]
    right_confirm_day <- max(right_upper_confirm_day,right_lower_confirm_day)
    
    # get the first day to violate the right side of the peak
    right_negate_day <- right_range[first(which(lo[right_range] < lower_threshold))]
    
    # check if confirm date is closer than negate date
    # left = T | F
    right <- ifelse(is.na(right_confirm_day), F, ifelse(is.na(right_negate_day), T, right_confirm_day <= right_negate_day))
    
    if(left & right){
      if(env$last_peak=="Low"){
        # last peak was a high, need to consider if this should overwrite the previous one 
        # overwrite if this new peak high is equal to or greater than the previous 
        env$replace <- lo[day] < lo[last(res$peaks$Lows)]
        
        if(env$replace){
          # clear out ZZ level on the series and replace with new 
          res$series[last(res$peaks$Lows)] <- NA
          res$series[day] <- lo[day]
          
          # replace peak high with today
          res$peaks$Lows[length(res$peaks$Lows)] <- day
        }
        
      }else{
        res$peaks$Lows <- c(res$peaks$Lows, day)
        res$series[day] <- lo[day]
        env$last_peak <- "Low"

      }
      
      
    }
  }
  
  
  return(res)
}

getZigZag2 <- function(x, dateindex=NULL){

  
  hi <- x[,"High"]
  prev_hi <- lag(hi)
  next_hi <- lead(hi)
  
  # all days with lower highs to the left and right 
  candidate_peak_highs <- which(hi >= prev_hi & hi > next_hi)
  
  lo <- x[,"Low"]
  prev_lo <- lag(lo)
  next_lo <- lead(lo)
  
  # all days with higher lows to the left and right 
  candidate_peak_lows <- which(lo <= prev_lo & lo < next_lo)
  
  all_candidate_peaks <- unique(c(candidate_peak_highs,candidate_peak_lows)[order(c(candidate_peak_highs,candidate_peak_lows))])
  
  res <- list (series = hi*NA, peaks = list(Highs=c(), Lows=c()))
  
  test.env <- new.env()
  test.env$last_peak <- ""
  
  for(day in all_candidate_peaks){
    test.env$replace <- F
    
    if(test.env$last_peak == "Low"){
      res <- getPeakLow(x,day,res, test.env)
      res <- getPeakHigh(x,day,res, test.env)
      
    }else{
      res <- getPeakHigh(x,day,res, test.env)
      res <- getPeakLow(x,day,res, test.env)
    }

  }
  
  
  res$series <- na.approx(res$series,na.rm = F)
  
  #write.csv(res$series, "G:\\Forest Research\\Trend Model\\TY_ZigZags.csv", row.names = dateindex,na = "")
  
  return(res)
  
}
