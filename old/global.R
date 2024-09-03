# Define functions ############
# Define 4 models ------------
# Not fixed
ll_4_1 <- function(df){
  Response <- colnames(df)[3]
  Conc <- colnames(df)[1]
  drm(as.formula(paste0(Response, "~", Conc)), data = df,
      fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("b", "Bottom", "Top", "IC50")))
} 
# T100
ll_4_2 <- function(df){
  Response <- colnames(df)[3]
  Conc <- colnames(df)[1]
  drm(as.formula(paste0(Response, "~", Conc)), data = df,
      fct = LL.4(fixed = c(NA, NA, 100, NA), names = c("b", "Bottom", "Top", "IC50")))
} 
# B0
ll_4_3 <- function(df){
  Response <- colnames(df)[3]
  Conc <- colnames(df)[1]
  drm(as.formula(paste0(Response, "~", Conc)), data = df,
      fct = LL.4(fixed = c(NA, 0, NA, NA), names = c("b", "Bottom", "Top", "IC50")))
} 
# B0 & T100
ll_4_4 <- function(df){
  Response <- colnames(df)[3]
  Conc <- colnames(df)[1]
  drm(as.formula(paste0(Response, "~", Conc)), data = df,
      fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("b", "Bottom", "Top", "IC50")))
}

# Reed-and-Muench method -------------------
RM_method <- function(df){
  if(df$Conc_low == 0){
    x1 <- 0
  }else{
    x1 <- log10(df$Conc_low)
  }
  x2 <- log10(df$Conc_high)
  y1 <- df$Res_low
  y2 <- df$Res_high
  k <- (y2-y1)/(x2-x1)
  b <- ((y1+y2)-(x1+x2)*k)/2
  IC50 <- 10 ^ ((50-b)/k)
}

# new function to select the right model--------------------------
m_select_LL <- function(df, crtrn_selected){
  fctList <- c("ll_4_1", "ll_4_2", "ll_4_3", "ll_4_4")
  lenFL <- length(fctList) #13
  retMat <- matrix(0, lenFL, 5)
  for (i in 1:lenFL){
    tempObj<- try(eval(parse(text = paste("ll_4_", i, "(df)", sep = ""))))
    if (!inherits(tempObj, "try-error")){
      retMat[i, 1] <- logLik(tempObj)
      retMat[i, 2] <- AIC(tempObj)  # AIC(tempObj)
      retMat[i, 3] <- BIC(tempObj)  # BIC(tempObj)
      retMat[i, 4] <- modelFit(tempObj)[2, 5]
      retMat[i, 5] <- summary(tempObj)$"resVar"
    }else{
      retMat[i, 1] <- NA
      retMat[i, 2] <- NA
      retMat[i, 3] <- NA
      retMat[i, 4] <- NA
      retMat[i, 5] <- NA
    }
  }
  
  colnames(retMat) <- c("logLik", "AIC", "BIC", "Lack_of_fit", "Res_var")
  retMat <- as.data.frame(retMat) %>% mutate("FctList" = fctList)
  if(crtrn_selected == "Lack_of_fit") {
    eval(parse(text = paste0("retMat <- retMat %>% arrange(desc(", crtrn_selected, "))")))
  } else {
    eval(parse(text = paste0("retMat <- retMat %>% arrange(", crtrn_selected, ")")))
  }
  
  return(retMat)
}


# Find the best constraints --------------------
best_m <- function(df){
  df$FctList[1]
}

# Calculating the ED50 -----------------------
M_fit_ED50 <- function(DF, EDtype){
  retMat <- matrix(0, nrow(DF), 6)
  for (i in 1:nrow(DF)) {
    df <- DF$data[[i]]
    colnames(df) <- c("Conc", "Replicate", "Response")
    n <- as.numeric(n_distinct(df$Replicate))
    
    Df <- df %>%
      group_by(Conc) %>%
      summarise(mean = mean(Response)) %>% 
      arrange(Conc)
    Df <- Df%>% 
      mutate("Conc_order" = rownames(Df))
    
    # whether the IC50 is out of the concentration range the user provided
    if (all(Df$mean > 50) | all(Df$mean < 50)){
      # Estimate
      retMat[i, 1] <- paste("Out-of-the-range ( <", round(min(Df$Conc), 2), "or > ", round(max(Df$Conc), 2), ")")
      # Std. Dev
      retMat[i, 2] <- "NaN"
      # CV
      retMat[i, 3] <- "NaN"
      # Std. Error
      retMat[i, 4] <- "NaN"
      # Confidence Interval
      retMat[i, 5] <- "NaN"
      # Estimation Method
      retMat[i, 6] <- "NaN"
      
    }else{
      n_conc <- as.numeric(n_distinct(Df$Conc))
      over_50 <- as.numeric(Df %>% filter(mean > 50) %>% pull("Conc_order"))
      less_50 <- as.numeric(Df %>% filter(mean < 50) %>% pull("Conc_order"))
      
      sup_over_50 <- seq(from = min(over_50), length.out = length(over_50), by = 1)
      sup_less_50 <- seq(from = min(less_50), length.out = length(less_50), by = 1)
      
      log_over_50 <- c(over_50 == sup_over_50)
      log_less_50 <- c(less_50 == sup_less_50)
      
      n_FALSE_over <- length(log_over_50[log_over_50 == FALSE])
      n_FALSE_less <- length(log_less_50[log_less_50 == FALSE])
      
      # whether the log-logistic model should be used
      if (length(over_50) != 1 & length(less_50) != 1 & n_FALSE_over == 0 & n_FALSE_less == 0) {
        tempObj <- try(eval(parse(text = paste(DF$LL4_Constraints[i], "(df)", sep =""))))
        if (!inherits(tempObj, "try-error")){
          # Estimate
          retMat[i, 1] <- round(as.data.frame(ED(tempObj, 50, type = EDtype))[1, 1], 2)
          # Std. Error
          retMat[i, 4] <- round(as.data.frame(ED(tempObj, 50, type = EDtype))[1, 2], 2)
          # Std. Dev
          retMat[i, 2] <- round(as.numeric(retMat[i, 4])*sqrt(n), 2)
          # Confidence Interval
          ci_l <- as.numeric(retMat[i, 1]) - as.numeric(retMat[i, 2])*1.96/sqrt(n)
          ci_u <- as.numeric(retMat[i, 1]) + as.numeric(retMat[i, 2])*1.96/sqrt(n)
          retMat[i, 5] <- paste(round(ci_l, 2), "to", round(ci_u, 2))
          # CV
          retMat[i, 3] <- round(as.numeric(retMat[i, 2])/as.numeric(retMat[i, 1])*100, 2)
        } else {
          retMat[i, 1] <- "NaN-"
          retMat[i, 4] <- "NaN-"
          retMat[i, 2] <- "NaN-"
          retMat[i, 5] <- "NaN-"
          retMat[i, 3] <- "NaN-"
        }
        # Estimation Method
        retMat[i, 6] <- "Log-logistic"
      } else {
        # 'Reed-and-Muench method'('two-point method') should be used
        # identify the two points that bracket the ED50
        if (Df$mean[1] > 50) {
          j <- 1
          while(Df$mean[j] > 50) {j <- j +1}
          ic50_max <- Df$Conc[j - 1]
          ic50_min <- Df$Conc[j]
        } else {
          j <- 1
          while(Df$mean[j] < 50) {j <- j +1}
          ic50_max <- Df$Conc[j]
          ic50_min <- Df$Conc[j - 1]
        }
        
        tempMat <- df %>% filter(Conc == ic50_max | Conc == ic50_min) %>% spread(Conc, Response) %>% mutate("L" = ic50_min, "H" = ic50_max)
        colnames(tempMat) <- c("Replicate", "Res_low", "Res_high", "Conc_low", "Conc_high")
        tempMat <- tempMat %>% group_by(Replicate) %>% nest() %>% mutate("ED50" = map(data, RM_method))
        tempMean <- mean(as.numeric(tempMat$ED50))
        tempSD <- sd(as.numeric(tempMat$ED50))
        # Estimate
        retMat[i, 1] <- paste0(round(tempMean, 2), "*")
        # Std. Dev
        retMat[i, 2] <- paste0(round(tempSD, 2), "*")
        # CV
        retMat[i, 3] <- paste0(round((tempSD/tempMean)*100, 2), "*")
        # Std. Error
        retMat[i, 4] <- paste0(round(tempSD/sqrt(n), 2), "*")
        # Confidence Interval
        ci_l <- tempMean - tempSD*1.96/sqrt(n)
        ci_u <- tempMean + tempSD*1.96/sqrt(n)
        retMat[i, 5] <- paste0(round(ci_l, 2), " to ", round(ci_u, 2), "*")
        # Estimation Method
        retMat[i, 6] <- "Reed-and-Muench"
      }
    }
  }
  
  colnames(retMat) <- c("ED50_Mean", "ED50_SD", "ED50_CV", "ED50_SE", "ED50_95%CI", "Estimation_Method")
  
  retMat <- as.data.frame(retMat)
  
}



# New functions to calculate ED50 -----------------------------------------













# ColorPicker ------------
# https://rdrr.io/cran/colourpicker/api/

# Calculate the luminance of a colour
getLuminance <- function(colhex) {
  colrgb <- grDevices::col2rgb(colhex)
  lum <- lapply(colrgb, function(x) {
    x <- x / 255;
    if (x <= 0.03928) {
      x <- x / 12.92
    } else {
      x <- ((x + 0.055) / 1.055)^2.4
    }
  })
  lum <- lum[[1]]*0.2326 + lum[[2]]*0.6952 + lum[[3]]*0.0722;
  lum
}

# Determine if a colour is dark or light
isColDark <- function(colhex) {
  rgba <- hex2rgba(colhex)
  if (!is.null(rgba[['a']]) && rgba[['a']] < 0.5) {
    return(FALSE)
  }
  return(getLuminance(colhex) <= 0.22)
}

# Return a list of numeric rgb (or rgba) values for a HEX code
# Assumes a 6 or 8 digit HEx code with leading # symbol
hex2rgba <- function(hex) {
  # If a colour name instead of hex is given
  if (substring(hex, 1, 1) != "#") {
    rgb <- grDevices::col2rgb(hex)
    return(
      list(
        r = rgb[1],
        g = rgb[2],
        b = rgb[3]
      )
    )
  }
  hex <- substring(hex, 2)
  
  result <- list(
    r = as.integer(as.hexmode(substring(hex, 1, 2))),
    g = as.integer(as.hexmode(substring(hex, 3, 4))),
    b = as.integer(as.hexmode(substring(hex, 5, 6)))
  )
  if (nchar(hex) == 8) {
    a <- as.integer(as.hexmode(substring(hex, 7, 8)))
    if (a < 255) {
      result[['a']] <- a / 255
    }
  }
  result
}

# Return a CSS-like rgb() or rgba() value for a HEX code
# Assumes a 6 or 8 digit HEx code with leading # symbol
hex2rgba_str <- function(hex) {
  rgba <- hex2rgba(hex)
  if (is.null(rgba[['a']]) || rgba[['a']] == 255) {
    return(sprintf("rgb(%s, %s, %s)", rgba[['r']], rgba[['g']], rgba[['b']]))
  } else {
    return(sprintf("rgba(%s, %s, %s, %s)", rgba[['r']], rgba[['g']], rgba[['b']], rgba[['a']]))
  }
}
