workspace_dir = getwd() # current workspace directory
folder_dir <- paste0(workspace_dir,'/scripts/') # get function folder directory

r_names <- list.files(folder_dir, pattern = '\\.R$', recursive = TRUE) # find all .R file names

for(i in 1:length(r_names)){
  source(paste0(workspace_dir,'/scripts/',r_names[i]))
}


data <- getData(c("USDJPY Curncy"),start = "1998-01-01")

results <- list()
start_buffer <- 252
save_ZZ <- F

run <- function(data){
  start <- proc.time()
  
  res <- lapply(names(data), function(asset){
    
    x <- data[[asset]]
    x[rowAnyNAs(x),] <- x[rowAnyNAs(x),'Close']
    
    dateindex <- index(x)
    holiday_list <- c("Christmas", "NewYears")
    holidays <- which(dateindex %in% as.Date(as.character(holidays(unique(year(dateindex)))[which(names(holidays(unique(year(dateindex)))) %in% holiday_list)]),"%Y%m%d"))
    ZZs <- getZigZag2(as.data.frame(x),dateindex, asset, save=save_ZZ)
    ZZpeaks <- ZZs$peaks
    confirm_dates <- getConfirmationDates(x,ZZpeaks)
    monthly_avg <- getSMMA(x[,'Close'],20)
    ATR <- getATR(x, 20)
    
    n <- nrow(x)
    
    res <- data.frame('Date' = dateindex, 'Notes' = character(length(dateindex)))
    # start at first ZZ
    start <- min(first(ZZpeaks$Highs[ZZpeaks$Highs > start_buffer]), first(ZZpeaks$Lows[ZZpeaks$Lows > start_buffer]))
    trend_start_day <- start_buffer
    reversal_date <- trend_start_day
    trend_defining_day <- start
    pvf_peak_day <- start
    pvf <- 0
    new_pvf <- 0
    one_time_shift_rule <- F
    trend_defining_level <- NULL 
    trend_extreme_day <- trend_start_day
    prev_trend_extreme_day <- trend_extreme_day
    new_trend_extreme_day <- trend_extreme_day
    current_short_side <- NULL
    trend <- NULL
    shift <- F
    double_battle_line <- F
    battle_line_1_day <- 0
    battle_line_2_day <- 0
    new_extreme_battle_line <- F
    MA_threshold <- 1.5
    pattern_override <- F
    MA_override <- F
    
    #### Filter parameters ####
    bad_trend_count <- 0
    bad_trend_threshold <- 3
    bad_trend <- T
    bad_trend_filter <- F
    bad_trend_shift <- F
    bad_trend_probe <- F
    bad_trend_reference_day <- trend_start_day
    
    use_extended_impulse_filter <- T
    extended_impulse_on_threshold <- 5
    extended_impulse_off_threshold <- 1.5
    extended_impulse_filter <- F
    extended_impulse_start_day <- 0
    extended_impulse_channel_count <- 0 
    
    use_large_rotation_filter <- T
    # How big the long side of the rotation needs to be
    large_rotation_filter_threshold_1 <- 5
    # How far apart the subsequent peak needs to be from the rotation extreme (at least)
    large_rotation_filter_threshold_2 <- 2.5
    # Large rotation filter toggle
    large_rotation_filter <- F
    
    
    if(start %in% ZZpeaks$Highs){
      # If first ZZ is a peak high, start in a bear trend
      trend <- -1
      trend_defining_level <- as.numeric(x[trend_defining_day,'High'])
      pvf <- abs(x[trend_start_day, 'Low'] - trend_defining_level)
      pvf_peak <- as.numeric(x[pvf_peak_day,'High'])
      
    }else{
      # othewrwise start in a bull trend 
      trend <- 1
      trend_defining_level <- as.numeric(x[trend_defining_day,'Low'])
      pvf <- abs(x[trend_start_day, 'High'] - trend_defining_level)
      pvf_peak <- as.numeric(x[pvf_peak_day,'Low'])
      
    }
    
    
    for(day in c((start+1):n)){
      extreme_pos <- ifelse(trend==1,"High","Low")
      extreme_neg <- ifelse(trend==1, "Low","High")
      ZZ_key <- ifelse(trend==1,'Lows','Highs')
      range <- x[day,'High'] - x[day,'Low'] 
      bad_trend_shift <- F
      new_extreme_battle_line <- F
      
      # Everything is in terms of a bull trend, adjusted with the `trend` multiplier 
      # Step 1 Check for trend reversal:
      # - Full day below trend defining level or monthly average
      # - Close < upper quartile 

      full_day_pattern <- as.numeric(x[day,extreme_pos] * trend) < (trend * min(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]) | extended_impulse_filter,Inf,trend*monthly_avg[day]))) * trend 
      full_day_monthly <- as.numeric(x[day,extreme_pos] * trend) < ifelse(is.na(monthly_avg[day]) ,Inf, trend * monthly_avg[day])
      # full_day <- ifelse(pattern_override, full_day_pattern,
      #                    ifelse(MA_override,full_day_monthly,
      #                           ifelse(abs(monthly_avg[day] - trend_defining_level)/ATR[day] > MA_threshold, full_day_pattern | full_day_monthly, full_day_pattern & full_day_monthly)))
  
      full_day <- ifelse(pattern_override, full_day_pattern, 
                         ifelse(MA_override,full_day_monthly, full_day_pattern & full_day_monthly))
      
      quartile <- x[day,'Close'] * trend < trend * x[day, extreme_pos] - (range/4) 
      reversal <- full_day & quartile 
      
      if(full_day & !quartile){
        res[day, "Notes"] <- paste0(c(res[day,'Notes'],"Quartile rule prevented reversal"), collapse="; ")
      }
      
      if(reversal & day %in% holidays ){
        res[day, "Notes"] <- paste0(c(res[day,'Notes'],"Holiday prevented reversal"), collapse="; ")
        reversal <- F
      }
      
      if(reversal){
        
        res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0(ifelse(trend==-1,"Bullish", "Bearish"), " reversal")), collapse="; ")
        
        one_time_shift_rule <- F
        double_battle_line <- F
        shift <- F
        extended_impulse_filter <- F
        extended_impulse_channel_count <- 0
        MA_override <- F
        pattern_override <- F
        
        if(bad_trend){
          # if you were previously in a bad trend
          bad_trend_filter <- !bad_trend_filter
          bad_trend_count <- bad_trend_count + 1
        }else{
          bad_trend_filter <- F
          bad_trend_count <- 0
        }
        
        new_trend <- -trend
        
        extreme_pos <- ifelse(new_trend==1,"High","Low")
        extreme_neg <- ifelse(new_trend==1, "Low","High")
        
        
        
        
        # find channels since last trend extreme 
        ZZ_key <- ifelse(new_trend==1,'Lows','Highs')
        channel_range <- c(trend_extreme_day:day)
        channels <- getChannels(x,channel_range, ZZpeaks[[ZZ_key]][which(ZZpeaks[[ZZ_key]] > trend_extreme_day & ZZpeaks[[ZZ_key]] < day)], trend = new_trend)
        
        if(length(channels$days) > 1){
          # We have multiple channels 
          # if we have a channel since the previous trend extreme this sets the pvf
          if(channels$current_rotation_size > rev(channels$size)[1]){
            trend_defining_day <- channels$current_rotation_day
            
          }else if(rev(channels$size)[1] > rev(channels$size)[2]){
            trend_defining_day <- rev(channels$days)[1]
            
          }else{
            trend_defining_day <- rev(channels$days)[2]
            double_battle_line <- T
            battle_line_2_day <- rev(channels$days)[1]
            battle_line_1_day <- trend_defining_day
            
          }
          
          pvf <- rev(channels$size)[1]
          pvf_peak_day <- rev(channels$days)[1]
          
        }else if(length(channels$days)){
          
         
          if(pvf > channels$size){
            if(channels$current_rotation_day){
              # we have a current rotation
              rotation_extreme <- channels$current_rotation_day
              if(channels$current_rotation_size > channels$size){
                trend_defining_day <- channels$current_rotation_day
              }else{
                # 
                trend_defining_day <-  channels$days
                double_battle_line <- T
                battle_line_1_day <- channels$days
                battle_line_2_day <- channels$current_rotation_day
              }
            }else{
              # no current rotation 
              trend_defining_day <- trend_extreme_day
              double_battle_line <- T
              battle_line_1_day <- trend_extreme_day
              battle_line_2_day <- channels$days
            }
          }else{
            # channel is larger than previous pvf
            trend_defining_day <-  channels$days
          }
          
          pvf <- channels$size
          pvf_peak_day <- channels$days
          
          
        }else{
          # No rotations so trend defining level is set to the previous trend extreme 
          # unless the current rotation is larger than the pvf
          
          trend_defining_day <- ifelse(channels$current_rotation_size >= pvf,channels$current_rotation_day ,trend_extreme_day)
          pvf_peak_day <- trend_extreme_day
          
        }
        
        
        bad_reference_day_1 <- c(trend_start_day:day)[last(which(trend * x[c(trend_start_day:day),extreme_pos] > trend * monthly_avg[c(trend_start_day:day)]))]
        
        if(is.na(bad_reference_day_1)){
          bad_trend_reference_day <- day
          #print(c(dateindex[day], NA, dateindex[day]))
        }else{
          bad_trend_reference_day <- c(bad_reference_day_1:day)[first(which(trend * x[c(bad_reference_day_1:day),extreme_neg] < trend * monthly_avg[c(bad_reference_day_1:day)]))]
          #print(c(dateindex[day], dateindex[bad_reference_day_1], ifelse(is.na(bad_trend_reference_day), NA, dateindex[bad_trend_reference_day])))
          
        }
        
        bad_trend <- abs(as.numeric(x[day,extreme_pos]) - as.numeric(x[ifelse(is.na(bad_trend_reference_day),day,bad_trend_reference_day), 'Close']))/ATR[day] < bad_trend_threshold
        
        if(bad_trend_filter & !bad_trend){
          res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0(ifelse(trend==1,"Trend support kept wide", "Trend resistance kept wide"), " (count: ", bad_trend_count,")")), collapse="; ")
          res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0("poor trend filter removed (0 days)")), collapse="; ")
          bad_trend_filter <- F
        }
        
        
        if(bad_trend_filter) trend_defining_day <- trend_extreme_day
        
        # Set trend defining level
        trend_defining_level <- as.numeric(x[trend_defining_day, extreme_neg])
        rotation_extreme_day <- ifelse(channels$current_rotation_day,channels$current_rotation_day,pvf_peak_day)
        rotation_extreme <- as.numeric(x[rotation_extreme_day,extreme_neg])
        
        
        
        
        # force trend defining to the pattern instead of the monthly
        pattern_override <- bad_trend_filter
        trend <- new_trend
        trend_start_day <- trend_extreme_day
        trend_extreme_day <- (trend_start_day:day)[first(which(trend * x[trend_start_day:day,extreme_pos] == max(trend * x[trend_start_day:day,extreme_pos])))]
        prev_trend_extreme_day <- trend_extreme_day#(trend_defining_day:trend_defining_day)[first(which(trend * x[trend_defining_day:day,extreme_pos] == max(trend * x[trend_defining_day:day,extreme_pos])))
        pvf_peak <- x[pvf_peak_day,extreme_neg]
        current_short_side <- channels$current_rotation_size
        reversal_date <- day
        
        if(bad_trend_filter){
          res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0(ifelse(trend==1,"Trend support kept wide", "Trend resistance kept wide"), " (count: ", bad_trend_count,")")), collapse="; ")
        }else if(bad_trend_count){
          res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0("poor trend filter removed (count: ", bad_trend_count,")")), collapse="; ")
          
        }
        
        
        
        
        
        res[day,c("Trend","Pattern Level", "Moving Average", "Trend Defining Level","Pattern Level Day", "ATR")] <- c(trend,
                                                                                                               trend_defining_level,
                                                                                                               monthly_avg[day],
                                                                                                               # ifelse(pattern_override, trend_defining_level,
                                                                                                               #        ifelse(MA_override, monthly_avg[day],
                                                                                                               #               ifelse(abs(monthly_avg[day] - trend_defining_level)/ATR[day] > MA_threshold,
                                                                                                               #                      trend * max(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),-Inf,trend*monthly_avg[day])),
                                                                                                               #                      trend * min(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),Inf,trend*monthly_avg[day]))))), 
                                                                                                               
                                                                                                               trend * min(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),Inf,trend*monthly_avg[day])), 
                                                                                                               as.character(dateindex[trend_defining_day]),
                                                                                                               ATR[day])
        
        next
      }
      
      
      
      if(use_extended_impulse_filter & !extended_impulse_filter){
        if(abs(x[day, extreme_pos] - monthly_avg[day])/ATR[day] > extended_impulse_on_threshold & (as.numeric(trend * x[day,extreme_pos]) > as.numeric(trend * x[trend_extreme_day,extreme_pos]))){
          one_time_shift_rule <- F
          double_battle_line <- F
          extended_impulse_filter <- T
          pattern_override <- T
          first_extended_shift <- T
          extended_impulse_start_day <- day
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Extended impulse filter enabled"), collapse="; ")
          
        }
      }
      
      
      # Step 2 update PVF
      trend_range <- c(trend_start_day:day)
      new_trend_extreme_day <- trend_range[first(which(trend*x[trend_range,extreme_pos] == max(trend*x[trend_range,extreme_pos])))]
      
      if(new_trend_extreme_day > trend_extreme_day){
        #browser()
        # If we make a new trend extreme 
        one_time_shift_rule <- F
        
        # Check if there is a peak high in between
        candidate_peaks <- ZZpeaks[[ifelse(trend==1,'Lows','Highs')]][which(ZZpeaks[[ifelse(trend==1,'Lows','Highs')]] > trend_extreme_day & ZZpeaks[[ifelse(trend==1,'Lows','Highs')]] <= new_trend_extreme_day)]
        
        confirmed_candidate_peaks <- candidate_peaks[confirm_dates[as.character(candidate_peaks)] <= day]
        
        # take care of the case of a rotation extreme also being a trend extreme
        if(new_trend_extreme_day %in% candidate_peaks & !(new_trend_extreme_day %in% confirmed_candidate_peaks)){
          # Do nothing here 
          #browser()
          x[day,extreme_pos] <- x[trend_extreme_day, extreme_pos]
        }else{
          
          # check if the current trend has gone the required ATR multiple 
          bad_trend <- abs(as.numeric(x[day,extreme_pos]) - as.numeric(x[ifelse(is.na(bad_trend_reference_day),reversal_date,bad_trend_reference_day), 'Close']))/ATR[day] < bad_trend_threshold
          
          if(bad_trend_filter){
            if(bad_trend){
              # check if we've probed trend defining AND made a new high
              if(min(trend * x[c((trend_extreme_day):day), extreme_neg]) < trend * trend_defining_level){
                #browser()
                bad_trend_probe <- T
              }
            }else{
              res[day, "Notes"] <- paste0(c(res[day,'Notes'],paste0("poor trend filter removed (",day-reversal_date ," days)")), collapse="; ")
              bad_trend_shift <- T
              bad_trend_filter <- F
              pattern_override <- F
            }
          }
          
          

          if(length(confirmed_candidate_peaks)){
            prev_trend_extreme_day <- trend_extreme_day
            
            new_pvf_peak_day <- confirmed_candidate_peaks[first(which(trend*x[confirmed_candidate_peaks,extreme_neg] == min(trend*x[confirmed_candidate_peaks,extreme_neg])))]
            new_pvf_peak <- as.numeric(x[new_pvf_peak_day,extreme_neg])
            new_pvf <- as.numeric(abs(x[prev_trend_extreme_day,extreme_pos] - as.numeric(new_pvf_peak)))
            
            if(extended_impulse_filter) extended_impulse_channel_count <- extended_impulse_channel_count + 1
            
            #browser()
            if(new_pvf >= pvf){
              # We've formed a new channel AND the current channel extreme is not already trend defining OR 
              if(new_pvf_peak != trend_defining_level){
                shift <- T
                double_battle_line <- F
              }
            }else if(double_battle_line & battle_line_2_day > trend_defining_day){
              if(trend * new_pvf_peak >= trend * pvf_peak){
                shift <- T
                new_extreme_battle_line <- T
                battle_line_1_day <- battle_line_2_day
                battle_line_2_day <- new_pvf_peak_day
              }else{
                browser()
                double_battle_line <- F
              }
              
            }else{
              double_battle_line <- T
              # only shift on a double battle line if we've previously been in a bad trend 
              shift <- bad_trend_shift
              battle_line_2_day <- new_pvf_peak_day
              battle_line_1_day <- pvf_peak_day
              #res[day, "Notes"] <- paste0(c(res[day,'Notes'],"Double battle line established"), collapse="; ")
            }
            
            pvf <- new_pvf
            pvf_peak_day <- new_pvf_peak_day
            pvf_peak <- new_pvf_peak
            current_short_side <- 0
            rotation_extreme_day <- pvf_peak_day
            rotation_extreme <- pvf_peak
            current_short_side_day <- pvf_peak_day
            current_short_side_peak <- pvf_peak
            
          }else if(bad_trend_shift & pvf_peak_day != trend_defining_day){
            # We've removed the PTF 
            # Shfit as long as the pvf day is not the same as trend defining
            shift <- T
            current_short_side <- 0
            rotation_extreme_day <- pvf_peak_day
            rotation_extreme <- pvf_peak
            current_short_side_day <- pvf_peak_day
            current_short_side_peak <- pvf_peak
            
          }
          
          trend_extreme_day <- new_trend_extreme_day
          
        }
        
        
        
        
        
        
      }else{
        # no new trend extreme, recalculate the current short side 
        # short side = rotation extreme - 
        # need to check that peaks are confirmed on or before `day`
        candidate_peaks <- ZZpeaks[[ifelse(trend==1,'Lows','Highs')]][which(ZZpeaks[[ZZ_key]] > trend_extreme_day & ZZpeaks[[ZZ_key]] < day)]
        candidate_peaks <- candidate_peaks[confirm_dates[as.character(candidate_peaks)] <= day]
        candidate_peaks <- candidate_peaks[!is.na(candidate_peaks)]
        
        if(length(candidate_peaks)){
          #browser()
          # take the "most extreme" peak for this rotation
          #Vif(day == 3588) browser()
          rotation_extreme_day <- candidate_peaks[first(which(trend*x[candidate_peaks,extreme_neg] == min(trend*x[candidate_peaks,extreme_neg])))]
          rotation_extreme <- as.numeric(x[rotation_extreme_day,extreme_neg])
          long_side <- abs(as.numeric(x[trend_extreme_day, extreme_pos] - rotation_extreme))/ATR[rotation_extreme_day]
          
          # find the "most extreme" value SINCE the rotation extreme
          new_short_side <- abs(trend*max(trend*x[c((rotation_extreme_day+1):day),extreme_pos]) - as.numeric(rotation_extreme))
          current_short_side <- max(current_short_side, new_short_side)
          
          if(new_short_side == current_short_side){
            current_short_side_day <- rotation_extreme_day
            current_short_side_peak <- rotation_extreme
            
            if(current_short_side_peak != trend_defining_level){
              if(current_short_side >= pvf){
                shift <- T
                double_battle_line <- F
                
              }else if(double_battle_line & battle_line_2_day > trend_defining_day){
                if(trend * current_short_side_peak >= trend * pvf_peak){
                  shift <- T
                }else{
                  double_battle_line <- F
                }
              } 
            }
            
          }
          
          
          
          if(use_large_rotation_filter & long_side > large_rotation_filter_threshold_1 & !bad_trend_filter){
            # Long side of the rotation is large enough, need to start looking for new peak that are > X (2) ATR from the rotation extreme
            
            
            channel_range <- c(rotation_extreme_day:day)
            channels <- getChannels(x,channel_range, candidate_peaks[candidate_peaks > rotation_extreme_day], trend = trend)
            
            
            if((length(channels$days) & channels$current_rotation_day != 0) | length(channels$days) > 1){
              subsequent_peak_day <- last(c(channels$days,channels$current_rotation_day[channels$current_rotation_day != 0])) #last(candidate_peaks[candidate_peaks > rotation_extreme_day])
              
              if(!is.na(subsequent_peak_day)){
                if((trend * as.numeric(x[subsequent_peak_day,extreme_neg]) - trend * rotation_extreme)/ATR[subsequent_peak_day] > large_rotation_filter_threshold_2){
                  #browser()
                  
                  
                  double_battle_line <- F
                  trend_start_day <- rotation_extreme_day
                  trend_range <- c(trend_start_day:day)
                  trend_extreme_day <- first(trend_range[which(x[trend_range,extreme_pos] ==  trend * max(trend * x[trend_range,extreme_pos]))])
                  current_short_side <- channels$current_rotation_size
                  
                  res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Large rotation filter enabled"), collapse="; ")
                  res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts")), collapse="; ")
                  
                  rotation_extreme_day <- rev(channels$day)[1]
                  
                  if(length(channels$days) > 1){
                    # multiple full formed channels, select the largert of the last two
                    
                    if(channels$current_rotation_size > rev(channels$size)[1]){
                      trend_defining_day <- channels$current_rotation_day
                      
                    }else if(rev(channels$size)[1] > rev(channels$size)[2]){
                      trend_defining_day <- rev(channels$days)[1]
                      
                    }else if(channels$current_rotation_size){
                      browser()
                      trend_defining_day <- rev(channels$days)[1]
                      double_battle_line <- T
                      battle_line_2_day <- channels$current_rotation_day
                      battle_line_1_day <- trend_defining_day
                      
                    }else{
                      trend_defining_day <- rev(channels$days)[2]
                      double_battle_line <- T
                      battle_line_2_day <- rev(channels$days)[1]
                      battle_line_1_day <- trend_defining_day
                    }
                    
                    pvf <- rev(channels$size)[1]
                    pvf_peak_day <- rev(channels$day)[1]
                    
                    
                  }else if(trend * as.numeric(x[channels$current_rotation_day,extreme_neg]) > trend *  as.numeric(x[channels$days,extreme_neg])){
                    # only one fully formed channel
                    trend_defining_day <- ifelse(channels$current_rotation_size > channels$size, channels$current_rotation_day, channels$days)
                    
                    pvf <- channels$size
                    pvf_peak_day <- channels$days
                  }else{
                    #browser()
                  }
                  
                  if(channels$current_rotation_day){
                    # override rotation extreme day with the current rotation if there is one
                    rotation_extreme_day <- channels$current_rotation_day
                    
                  }
                  
                  rotation_extreme <- as.numeric(x[rotation_extreme_day, extreme_neg])
                  trend_defining_level <- as.numeric(x[trend_defining_day, extreme_neg])
                  pvf_peak <- as.numeric(x[pvf_peak_day,extreme_neg])
                  shift <- F
                  one_time_shift_rule <- F
                }
              }
            }
            
            
          }
          
        }
      }
      
      if(extended_impulse_filter){
        # first, check to see if the latest trend defining level is with X ATR of the monthly average
        #extended_impulse_filter <- trend * (x[trend_defining_day, extreme_neg] - monthly_avg[day])/ATR[day] > extended_impulse_off_threshold | first_extended_shift
        
        
        
        if(extended_impulse_filter){
          # Make sure trend defining level is the last peak (regardless of size)
           
          #if(day >= 390)browser()
          
          # channel_range <- c(trend_start_day:day)
          # channels <- getChannels(x,channel_range, ZZpeaks[[ZZ_key]][which(ZZpeaks[[ZZ_key]] > trend_start_day & ZZpeaks[[ZZ_key]] < day)], trend = trend)
          # 
          # candidate_peaks <- ZZpeaks[[ZZ_key]][ZZpeaks[[ZZ_key]] < day]
          #last_confirmed_peak_day <- rotation_extreme_day #last(candidate_peaks[confirm_dates[as.character(candidate_peaks)] <= day])
          
          if(rotation_extreme_day > trend_defining_day){

            if(trend * as.numeric(rotation_extreme) > trend * trend_defining_level){
              trend_defining_day <- rotation_extreme_day
              trend_defining_level <- as.numeric(rotation_extreme)
              one_time_shift_rule <- F
              
              res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts")), collapse="; ")
              
              if(!grepl("Extended impulse filter enabled",res[day,'Notes'])){
                res[day,'Notes'] <- paste0(c(res[day,'Notes'], "extended impulse filter"), collapse="; ")
              }
              
              if(first_extended_shift){
                first_extended_shift <- F
              }

            }else if(!one_time_shift_rule){
              # can only shift opposite of the trend once so this needs to be FALSE
              # marker to allow shifting or not
              ots <- F
              # check if the current rotation is large enough 
              if(trend_defining_day > trend_extreme_day | trend_extreme_day == day){
                # this is either not a fully formed roation or we've made a new trend extreme
                if(current_short_side_day == rotation_extreme_day){
                  ots <- T
                }
              }else{
                # compare the current short side to the pvf, most recent channel
                if(current_short_side >= pvf){
                  ots <- T
                } 
              }
              
              if(ots){
                trend_defining_day <- rotation_extreme_day
                trend_defining_level <- as.numeric(rotation_extreme)
                
                # set one time shift rule to TRUE until we shift in the correct direction
                one_time_shift_rule <- T
                res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts, OTSR enabled")), collapse="; ")
                
                if(!grepl("Extended impulse filter enabled",res[day,'Notes'])){
                  res[day,'Notes'] <- paste0(c(res[day,'Notes'], "extended impulse filter"), collapse="; ")
                }
                
                if(first_extended_shift){
                  first_extended_shift <- F
                }
              }
              
                
            }else if(as.numeric(trend * x[day,extreme_pos]) > as.numeric(max(trend*x[c(trend_defining_day:(day-1)),extreme_pos]))){
              # we've made a new POSITIVE extreme for the roation, resetting the pattern
              trend_defining_day <- rotation_extreme_day
              trend_defining_level <- as.numeric(rotation_extreme)
              
              res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts, OTSR enabled")), collapse="; ")
              
              if(!grepl("Extended impulse filter enabled",res[day,'Notes'])){
                res[day,'Notes'] <- paste0(c(res[day,'Notes'], "extended impulse filter"), collapse="; ")
              }
              
              if(first_extended_shift){
                first_extended_shift <- F
              }
              
            }else{
              res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Can't shift due to one time shift rule"), collapse="; ")
              
            }
            
          }else if(first_extended_shift){
            # Extended impulse filter is enbaled but we have not yet been able to shift 
            # check if monhtly average is close by at least 1 ATR
            # if(trend * monthly_avg[day] > trend * (trend_defining_level + trend*ATR[day])){
            #   # set monhtly average
            #   MA_override <- T
            #   pattern_override <- F
            #   res[day,'Notes'] <- paste0(c(res[day,'Notes'],"extended impulse filter using MA"), collapse="; ")
            # 
            # }else{
            #   MA_override <- F
            #   
            # }
          }
          
          
          
          #extended_impulse_filter <- trend * (x[trend_defining_day, extreme_neg] - monthly_avg[day])/ATR[day] > extended_impulse_off_threshold | first_extended_shift
          if(trend * (x[trend_defining_day, extreme_neg] - monthly_avg[day])/ATR[day] > extended_impulse_off_threshold){
            # pattern level is > X ATR above (below) the moving average in a bull (bear ) trend
            pattern_override <- T
            res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Pattern override"), collapse="; ")
            
          }else if(trend * (monthly_avg[day] - x[trend_defining_day, extreme_neg])/ATR[day] > extended_impulse_off_threshold){
            # monthly average is > X ATR above (below) the pattern level in a bull (bear ) trend
            MA_override <- T
            pattern_override <- F
            
          }else{
            pattern_override <- F
            MA_override <- F
          }
          
        }
        
        if((extended_impulse_channel_count >= 2 & !pattern_override) | (trend * x[day, extreme_neg] < trend * monthly_avg[day])){
          extended_impulse_channel_count <- 0
          extended_impulse_filter <- F
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Extended impulse filter removed"), collapse="; ")
          shift <- F
          pattern_override <- F
          # MA_override <- F
        }
        
      
        
      }else if(shift & (!bad_trend_filter | bad_trend_probe)){
          # skip shifting if we are still in a bad trend 
        
        if(trend * current_short_side_peak > trend * trend_defining_level){
          # check to see if we are shifting in the direction of the trend (lower for bear trend, higher for bull trend)
          
          # shift direction is ok
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts")), collapse="; ")
          
          # set trend defining parameters
          if(double_battle_line){
            if(bad_trend_shift){
              # double battle line rule has not been enacted yet        
              trend_defining_day <- battle_line_1_day
            }else{
              trend_defining_day <- ifelse(new_extreme_battle_line,battle_line_1_day,battle_line_2_day)
              new_extreme_battle_line <- F

              res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Double battle line"), collapse="; ")
            }
          }else{
            trend_defining_day <- rotation_extreme_day
          }                     
          
          trend_defining_level <- as.numeric(x[trend_defining_day, extreme_neg])
          
          # reset one time shift rule
          one_time_shift_rule <- F
          
        }else if(!one_time_shift_rule){
          #browser()
          # can only shift opposite of the trend once so this needs to be FALSE
          # set trend defining parameters
          trend_defining_level <- as.numeric(rotation_extreme)
          trend_defining_day <- rotation_extreme_day
          
          # set one time shift rule to TRUE until we shift in the correct direction
          one_time_shift_rule <- T
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts, OTSR enabled")), collapse="; ")
          
        }else if(as.numeric(trend * x[day,extreme_pos]) > as.numeric(max(trend*x[c(trend_defining_day:(day-1)),extreme_pos]))){
          # we've made a new POSITIVE extreme for the roation, resetting the pattern
          trend_defining_level <- as.numeric(rotation_extreme)
          trend_defining_day <- rotation_extreme_day
          
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],paste0("Trend ", ifelse(trend==1,"support","resistance"), " shifts, OTSR enabled")), collapse="; ")
          
        }else{
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],"Can't shift due to one time shift rule"), collapse="; ")
          
        }
        
        shift <- F
        bad_trend_probe <- F
        
        # reset to FALSE unless its a bad trend shift reset
        # double_battle_line <- double_battle_line & bad_trend_shift
        
      }
      
      
      if(MA_override){
        MA_override <- trend * (monthly_avg[day] - x[trend_defining_day, extreme_neg])/ATR[day] > extended_impulse_off_threshold
        if(MA_override){
          res[day,'Notes'] <- paste0(c(res[day,'Notes'],"MA override"), collapse="; ")
        }
      }
      
      res[day,c("Trend","Pattern Level", "Moving Average", "Trend Defining Level","Pattern Level Day", 'ATR')] <- c(trend,
                                                                                                             trend_defining_level,
                                                                                                             monthly_avg[day],
                                                                                                             # ifelse(pattern_override, trend_defining_level,
                                                                                                             #        ifelse(MA_override, monthly_avg[day],
                                                                                                             #               ifelse(abs(monthly_avg[day] - trend_defining_level)/ATR[day] > MA_threshold,
                                                                                                             #                      trend * max(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),-Inf,trend*monthly_avg[day])),
                                                                                                             #                      trend * min(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),Inf,trend*monthly_avg[day]))))), 
                                                                                                             
                                                                                                             ifelse(pattern_override, trend_defining_level,
                                                                                                                    ifelse(MA_override,monthly_avg[day],trend * min(trend*trend_defining_level,ifelse(is.na(monthly_avg[day]),Inf,trend*monthly_avg[day])))),
                                                                                                             as.character(dateindex[trend_defining_day]),
                                                                                                             ATR[day])
      
    }
    
    # Results cleanup 
    res[,'Notes'] <- str_remove(res[,'Notes'], "; ")
    res[,c("Trend","Pattern Level", "Moving Average", "Trend Defining Level", "ATR")] <- apply(res[,c("Trend","Pattern Level", "Moving Average", "Trend Defining Level", "ATR")],2,function(x){
      return(round(as.numeric(x),4))
    })
    
    res[,'Date'] <- as.character(res[,'Date'])
    convert_to32 <- F
    
    if(convert_to32){
      res[,c("Pattern Level", "Moving Average", "Trend Defining Level", "ATR")] <- apply(res[,c("Pattern Level", "Moving Average", "Trend Defining Level", "ATR")],2,function(x){
        x[!is.na(x)] <- decimal2thirtyseconds(as.numeric(x[!is.na(x)]))
        return(as.numeric(x))
      })
    }
    
    res <- res[,c('Date',"Trend","Pattern Level", "Moving Average", "Trend Defining Level","Pattern Level Day", "ATR", "Notes")]
    res[,c("close","open","high","low")] <- x
    
    print(tail(res,1))
    
    return(res)
    
  })
  
  #browser()
  end <- proc.time()
  
  print(end-start)
  return(res)
}


result <- run(data)
names(result) <- names(data)

shouldFormat <- T
negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
matchingStyle <- createStyle(bgFill = "yellow")
options("openxlsx.dateFormat" = "mm/dd/yyy")

wb <- createWorkbook()

sapply(names(result), function(asset){
  
  addWorksheet(wb, asset)
  writeData(wb,asset,result[[asset]])
  
  
  if(shouldFormat){
    ncol <- ncol(result[[asset]])
    nrow <- nrow(result[[asset]])
    
    freezePane(wb,asset,firstRow = T, firstCol = T)
    
    addStyle(wb, asset, style = createStyle(numFmt = "DATE"), rows = 2:nrow, cols = 1) 
    
    setColWidths(wb, asset, cols = 1:ncol, widths = "auto")
    
    conditionalFormatting(wb, asset,
                          cols = 1:ncol, rows=2:nrow,
                          rule = "$B2=1", style=posStyle
    )
    conditionalFormatting(wb, asset,
                          cols = 1:ncol, rows=2:nrow,
                          rule = "$B2=-1", style=negStyle
    )
    
    conditionalFormatting(wb, asset,
                          cols = 3, rows=2:nrow,
                          rule = 'AND(C2=E2,C2!="")', style=matchingStyle
    )
    conditionalFormatting(wb, asset,
                          cols = 4, rows=2:nrow,
                          rule = 'AND(D2=E2,D2!="")', style=matchingStyle
    )
  }
  
  #browser()
  write.csv(result[[asset]]$Trend, paste0("G:\\Forest Research\\Trend Model\\Trends\\",asset," Trend.csv"), row.names = result[[asset]]$Date, na = "")
})

saveWorkbook(wb, "Trend Model v8.8.1.xlsx", overwrite = T)

