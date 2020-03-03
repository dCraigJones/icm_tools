library(dplyr)
library(lubridate)
library(pumpR)
library(ggplot2); theme_set(theme_bw())
library(stringr)


min_pump_speed <- 25 #hz


load_scada <- function(input, FUN=dmy_hms) {
  error_msg <- c("CommFail", "Comm Fail", "NotConnect", "I/OTimeout", "IntfShut", "Shutdown", "Configure","Tagnotfound", "[-10722]PINET:TimeoutonPIRPCorSystemCall.")
  
  # get subset of data to export
  subset <- read.csv(input, header=TRUE, nrows=1)
  
  # get total number of columns in subset
  number_of_cols <- ncol(subset)
  
  # find columns with "RUN" in tag
  char_cols <- grep("RUN", toupper(names(subset)))
  
  # find columns for VFDs
  dbl_cols <- setdiff(2:number_of_cols, char_cols)
  
  # set column class
  col_type <- rep("character", number_of_cols)
  col_type[dbl_cols] <- "numeric"
  
  tmp <- read.csv(input
                  , colClasses=col_type
                  , na.strings=error_msg
  )
  
  datetime <- FUN(tmp[,1])
  #tmp <- tmp[,c(2:number_of_cols)]
  
  tmp$datetime <- datetime
  
  return(tmp)
}

translate_to_hz <- function(tag) {
  if(class(tag)=="character") {
      tag_hz <- rep(0, length(tag))
      
      tag_hz[tag=="RUN" | tag=="START"] <- 60

  } else {tag_hz <- tag}
  
  return(tag_hz)
}

impute_missing_values <- function(tag) {
  mu <- mean(tag, na.rm=TRUE)
  
  tmp <- tag
  
  tmp[is.na(tag)] <- mu
  
  return(tmp)
}

limit_pump_hz <- function(tag) {
  tmp <- tag
  
  tmp[is.na(tag)] <- 0
  tmp[tag>60] <- 60
  tmp[tag<min_pump_speed] <- 0
  
  return(tmp)
  
}

assign_scada <- function(datetime, level, stfmp, p1, p2, p3=rep(0,length(datetime)), p4=rep(0,length(datetime)), level_correction=0) {
  
  . <- translate_to_hz(p1)
  p1_hz <- limit_pump_hz(.)
  
  . <- translate_to_hz(p2)
  p2_hz <- limit_pump_hz(.)
  
  . <- translate_to_hz(p3)
  p3_hz <- limit_pump_hz(.)
  
  . <- translate_to_hz(p4)
  p4_hz <- limit_pump_hz(.)

  wll <- impute_missing_values(level)
  prs <- impute_missing_values(stfmp)
  tdh <- prs*2.31-wll+level_correction
  
  tmp <- data.frame(datetime=datetime, level_ft=wll, header_psi=prs, tdh_ft=tdh, p1_hz=p1_hz, p2_hz=p2_hz, p3_hz=p3_hz, p4_hz=p4_hz)
  
  return(tmp)
}

calc_pump_timesteps <- function(scada) {
  scada %>%
    mutate(n1=p1_hz>0, n2=p2_hz>0, n3=p3_hz>0, n4=p4_hz>0) %>%
    mutate(n=n1+n2+n3+n4) %>%
    filter(n>0) %>%
    mutate(mu_hz=(p1_hz+p2_hz+p3_hz+p4_hz)/n)
}

calc_pump_flow <- function(scada, pump) {
  hq <- data.frame(datetime=scada$datetime, q=rep(0,nrow(scada)), h=scada$tdh_ft)
  
  for(i in 1:(nrow(scada)-1)) {
    adjust_pump_curve <- VFD(pump, scada$mu_hz[i])
    
    combine_pump <- adjust_pump_curve
    
    combine_pump[,1] <- combine_pump[,1]*scada$n[i]
    
    hq$q[i] <- Get.Flow(combine_pump, hq$h[i])
  }
  
  return(hq)
}

assign_scada_wo_columns <- function(tmp, level_correction_ft) {
  tmp_col_names <- tolower(colnames(tmp))
  
  col_datetime <- which(str_detect(tmp_col_names, "datetime"))
  col_level <- which(str_detect(tmp_col_names, "wll"))
  col_header <- which(str_detect(tmp_col_names, "stfmp|press"))
  col_p1 <- which(str_detect(tmp_col_names, "p1"))
  col_p2 <- which(str_detect(tmp_col_names, "p2"))
  col_p3 <- which(str_detect(tmp_col_names, "p3"))
  col_p4 <- which(str_detect(tmp_col_names, "p4"))
  
  export <- assign_scada(tmp[,col_datetime]
               , tmp[,col_level]
               , tmp[,col_header]
               , tmp[,col_p1]
               , tmp[,col_p2]
               , ifelse(length(col_p3)>0,tmp[,col_p3],0)
               , ifelse(length(col_p4)>0,tmp[,col_p4],0)
               , level_correction_ft
  )
  
  return(export)
  
}


calc_pump_flow_from_scada <- function(input, pump, level_correction_ft=0, FUN=dmy_hms) {
  data <- load_scada(input, FUN)
  
  use <- assign_scada_wo_columns(data, level_correction_ft)
  
  use2 <- calc_pump_timesteps(use)
  
  # use2 %>% 
  #   mutate(date=date(datetime)) %>% 
  #   group_by(date) %>% 
  #   #filter(n==1) %>%
  #   count(n) %>%
  #   ggplot(aes(x=date, y=nn)) +
  #   geom_col(aes(fill=n))
  
  flow <- calc_pump_flow(use2,pump)
  
  export <- flow %>% right_join(use2, by="datetime")
  
  #return(flow)
  return(export)
  
  
}


# Bay Harbour -------------------------------------------------------------

p <- New.Pump(c(
  0,	145.0000,
  400,	115.0000,
  800,	101.0000,
  1200,	90.0000,
  1600,	75.0000,
  2000,	53.0000,
  2400,	30.0000
))

flow <- calc_pump_flow_from_scada("./data-test/bay4568.csv", p)


# 
# 
# 
#     data <- load_scada("./data-test/bay4568.csv")
#     
# 
#     
#     use <- assign_scada(data$datetime
#                         , data$wwl.east.bay4568_wllevel
#                         , data$wwl.east.bay4568_stfmp
#                         , data$wwl.east.bay4568_P1RUN
#                         , data$wwl.east.bay4568_P2RUN
#                         , data$wwl.east.bay4568_P3RUN
#                         ,
#                         , 18
#     )
#     
#     use2 <- calc_pump_timesteps(use)
#     
#     use2 %>% 
#       mutate(date=date(datetime)) %>% 
#       group_by(date) %>% 
#       #filter(n==1) %>%
#       count(n) %>%
#       ggplot(aes(x=date, y=nn)) +
#       geom_col(aes(fill=n))
#     
    #flow <- calc_pump_flow(use2,p)
    
    Draw.Graph(10000,2000,500, 150, 50, 10)
    Draw.VFD(p, 3)
    points(flow$q, flow$h, pch=".")
    
    flow %>% 
      mutate(date=date(datetime)) %>% 
      group_by(date) %>% 
      summarize(gpd=sum(q))
    
    flow %>%
      ggplot(aes(q)) +
      geom_histogram()


# Kingsbury - Variable Speed ----------------------------------------------


    p <- New.Pump(c(
      0.00, 70.53665,
      500.00, 67.71203,
      1000.00, 64.94768,
      1500.00, 62.13744,
      2000.00, 59.20250,
      2500.00, 56.06974,
      3000.00, 52.68339,
      3500.00, 49.00597,
      4000.00, 45.01202,
      4500.00, 40.67541,
      5000.00, 36.01812,
      5500.00, 31.03503,
      6000.00, 25.77772,
      6039.33, 25.35787
    ))

    flow <- calc_pump_flow_from_scada("./data-test/kin4140.csv", p)
    # 
    # 
    # data <- load_scada("./data-test/kin4140.csv")
    # 
    # use <- assign_scada(data$datetime
    #                     , data$wwl.west.kin4140_wllevel
    #                     , data$wwl.west.kin4140_stfmp
    #                     , data$wwl.west.kin4140_p1cSPD
    #                     , data$wwl.west.kin4140_p2cSPD
    #                     , data$wwl.west.kin4140_p3cSPD
    #                     ,
    #                     , 0
    # )
    # 
    # use2 <- calc_pump_timesteps(use)
    # 
    # use2 %>% 
    #   mutate(date=date(datetime)) %>% 
    #   group_by(date) %>% 
    #   #filter(n==1) %>%
    #   count(n) %>%
    #   ggplot(aes(x=date, y=nn)) +
    #   geom_col(aes(fill=n))
    # 
    # flow <- calc_pump_flow(use2,p)
    
    Draw.Graph(10000,2000,500, 150, 50, 10)
    Draw.VFD(p, 3)
    points(flow$q, flow$h, pch=".")
    
    flow %>% 
      mutate(date=date(datetime)) %>% 
      group_by(date) %>% 
      summarize(gpd=sum(q))
    
    flow %>%
      ggplot(aes(q)) +
      geom_histogram()
