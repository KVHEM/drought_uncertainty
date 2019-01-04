# Set of functions for ploting tile graph of drought series and for their comparison

wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    rc <- grDevices::n2mfrow(n)
    nrow <- rc[2]
    ncol <- rc[1]
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  stopifnot(nrow * ncol >= n)
  
  c(nrow, ncol)
}

plot_tiles <- function(data, rank_threshold, num_members, num_threshold, reg, var, prop){
  #' Plot tile graph of drought ranks of ensemble members for given thresholds
  #'
  #' @param data input data
  #' @param rank_threshold threshold for ensemble members (0-250)
  #' @param num_members number of ensemble members over the rank threshold (0-100)
  #' @param num_threshold given thresholds for color classes (vector of lenght 4)
  #' @param reg region either "CEU" or "MED" (character only) 
  #' @param var variable either "s" (soil moisture) or "q" (runoff) (character only)
  #' @param prop property of the drought either "Area" or "Severity" (character only) 
  #'
  #' @return plot
  #'
  #' @examples plot_tiles(data = x, rank_threshold = 225, num_members = 10, 
  #'                      num_threshold = c(125, 224, 247, 250), reg = "CEU", 
  #'                      var = "s", prop = "Severity")
  
  library(data.table)
  library(ggplot2)
  library(grid)
  library(cowplot)
  
  dta <- data
  rank_threshold <- rank_threshold
  num_members <- num_members
  num_threshold <- num_threshold 
  #col_num <- col_num
  reg <- reg
  var <- var
  prop <- prop
  
  heavy_rain <- "white"; strong_rain <- '#eca776'; mean_rain <- '#e07020'; light_rain <- '#8b4513'
  strong_drought_s <- '#8b4513'; mean_drought_s <- '#e07020'; light_drought_s <- '#eca776'
  strong_drought_q <- '#8b4513'; mean_drought_q <- '#e07020'; light_drought_q <- '#eca776'
  
  if (prop == "Area"){
    uncer_noise_area <- dta[RANK_AREA > rank_threshold,]
    uncer_noise_area[,NOISE:= .N, by = .(YR, REG, VAR)]
  } else {
    uncer_noise_area <- dta[RANK_SEV > rank_threshold,]
    uncer_noise_area[,NOISE:= .N, by = .(YR, REG, VAR)]
  }
  
  ######################### PLOT ##############################
  years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > num_members,]$YR)
  years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > num_members,]$YR)
  years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > num_members,]$YR)
  years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > num_members,]$YR)
  
  if (reg == "CEU"){
    if (var == "s"){
      ggyears <- years_ceu_s
    } else {
      ggyears <- years_ceu_q
    }
  } else { 
    if (var == "s"){
      ggyears <- years_med_s
    } else {
      ggyears <- years_med_q
    }
  }
  
  if (prop == "Area"){
    base_plot <- ggplot(dta[REG == reg & VAR == var & RANK_AREA >= num_threshold[1] & YR %in% ggyears,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = num_threshold)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      #facet_wrap(~YR, ncol = col_num) +
      facet_wrap(~YR) +
      scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
      ggtitle(if (reg == "CEU"){
        if (var == "s"){
          "CEU Soil moisture drought / Area"
        } else {
          "CEU Runoff drought / Area"
        }
      } else { 
        if (var == "s"){
          "MED Soil moisture drought / Area"
        } else {
          "MED Runoff drought / Area"
        }
      }) +
      theme(strip.text = element_text(size = 7, colour = 'black'),
            legend.position = "none", 
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 12)) +
      panel_border(colour = "black")
  } else {
    base_plot <- ggplot(dta[REG == reg & VAR == var & RANK_SEV >= num_threshold[1] & YR %in% ggyears,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = num_threshold)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      #facet_wrap(~YR, ncol = col_num) +
      facet_wrap(~YR) +
      scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
      ggtitle(if (reg == "CEU"){
        if (var == "s"){
          "CEU Soil moisture drought / Severity"
        } else {
          "CEU Runoff drought / Severity"
        }
      } else { 
        if (var == "s"){
          "MED Soil moisture drought / Severity"
        } else {
          "MED Runoff drought / Severity"
        }
      }) +
      theme(strip.text = element_text(size = 7, colour = 'black'),
            legend.position = "none", 
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 12)) +
      panel_border(colour = "black")
  }
  #grid.newpage()
  #grid.draw(base_plot)
  base_plot_g <- ggplot_gtable(ggplot_build(base_plot))
  stript <- which(grepl('strip-t', base_plot_g$layout$name))
  yr_vec <- c()
  
  for (i in 1:length(stript)){
    k <- base_plot_g$grobs[which(grepl('strip-t', base_plot_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
    if (is.null(k)){
      k <- NA
      yr_vec <- append(yr_vec, k)
    } else {
      yr_vec <- append(yr_vec, k)
    }
  }
  
  num_nas <- length(which(is.na(yr_vec) == T))
  stript <- stript[!is.na(yr_vec)]
  yr_vec <- yr_vec[complete.cases(yr_vec)]
  yr_vec <- data.frame(YR = as.numeric(yr_vec))
  yr_vec$ORD <- 1:nrow(yr_vec)
  
  tab_col <- dta[REG == reg & VAR == "p" & YR %in% yr_vec$YR,]
  tab_col[RANK_SEV <= num_threshold[1] ,SEV_COL:= heavy_rain]
  tab_col[RANK_SEV > num_threshold[1] & RANK_SEV <= num_threshold[2] ,SEV_COL:= strong_rain]
  tab_col[RANK_SEV > num_threshold[2] & RANK_SEV <= num_threshold[3] ,SEV_COL:= mean_rain]
  tab_col[RANK_SEV > num_threshold[3] & RANK_SEV <= num_threshold[4] ,SEV_COL:= light_rain]
  
  tab_col[RANK_AREA <= num_threshold[1], AREA_COL:= heavy_rain]
  tab_col[RANK_AREA > num_threshold[1] & RANK_AREA <= num_threshold[2] ,AREA_COL:= strong_rain]
  tab_col[RANK_AREA > num_threshold[2] & RANK_AREA <= num_threshold[3] ,AREA_COL:= mean_rain]
  tab_col[RANK_AREA > num_threshold[3] & RANK_AREA <= num_threshold[4] ,AREA_COL:= light_rain]
  
  tab_col2 <- merge(x = tab_col, y = yr_vec, by.x = "YR", by.y = "YR")
  if (reg == "CEU"){
    common_years <- intersect(years_ceu_q, years_ceu_s)
  } else {
    common_years <- intersect(years_med_q, years_med_s)
  }
  common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
  
  tab_col2[YR %in% common_years, COM_COL:= "black"]
  tab_col2[YR %in% common_years, COM_THI:= 2]
  tab_col2[YR %in% common_years, COM_LTY:= 2]
  tab_col2[YR %in% common_all, COM_LTY:= 1]
  tab_col2[is.na(COM_LTY), COM_LTY:=0]
  tab_col3 <- tab_col2[order(tab_col2$ORD)]
  
  if (prop == "Area"){
    fills <- tab_col3$AREA_COL
  } else {
    fills <- tab_col3$SEV_COL
  }
  
  frames <- tab_col3$COM_COL
  frames_lty <- tab_col3$COM_LTY
  frames_thi <- tab_col3$COM_THI
  
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', base_plot_g$grobs[[i]]$grobs[[1]]$childrenOrder))
    base_plot_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  stript <- which(grepl('panel', base_plot_g$layout$name))
  common_yr_matr <- matrix(stript, nrow = wrap_dims(n = length(ggyears))[1], byrow = F)
  nrow_matr <- nrow(common_yr_matr)
  common_yr_vec <- c()
  
  for (i in nrow_matr:1){
    common_yr_vec <- append(common_yr_vec, common_yr_matr[i,])
  }
  
  not <- tail(common_yr_matr[max(nrow_matr),], num_nas)
  
  k <- 1
  for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
    j <- which(grepl('border', base_plot_g$grobs[[i]]$childrenOrder))
    base_plot_g$grobs[[i]]$children[[j]]$gp$col <- frames[k]
    base_plot_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_thi[k]
    base_plot_g$grobs[[i]]$children[[j]]$gp$lty <- frames_lty[k]
    k <- k+1
  }
  
  grid.newpage()
  grid.draw(base_plot_g)
}

source_tiles <- function(data, rank_threshold, num_members, num_threshold, reg, var, prop){
  #' Source tile graph of drought ranks of ensemble members for given thresholds
  #'
  #' @param data input data
  #' @param rank_threshold threshold for ensemble members (0-250)
  #' @param num_members number of ensemble members over the rank threshold (0-100)
  #' @param num_threshold given thresholds for color classes (vector of lenght 4)
  #' @param reg region either "CEU" or "MED" (character only) 
  #' @param var variable either "s" (soil moisture) or "q" (runoff) (character only)
  #' @param prop property of the drought either "Area" or "Severity" (character only) 
  #'
  #' @return source gtable of the graph
  #'
  #' @examples source_tiles(data = x, rank_threshold = 225, num_members = 10, 
  #'                      num_threshold = c(125, 224, 247, 250), reg = "CEU", 
  #'                      var = "s", prop = "Severity")
  
  library(data.table)
  library(ggplot2)
  library(grid)
  library(cowplot)
  
  dta <- data
  rank_threshold <- rank_threshold
  num_members <- num_members
  num_threshold <- num_threshold 
  reg <- reg
  var <- var
  prop <- prop
  
  heavy_rain <- "white"; strong_rain <- '#eca776'; mean_rain <- '#e07020'; light_rain <- '#8b4513'
  strong_drought_s <- '#8b4513'; mean_drought_s <- '#e07020'; light_drought_s <- '#eca776'
  strong_drought_q <- '#8b4513'; mean_drought_q <- '#e07020'; light_drought_q <- '#eca776'
  
  if (prop == "Area"){
    uncer_noise_area <- dta[RANK_AREA > rank_threshold,]
    uncer_noise_area[,NOISE:= .N, by = .(YR, REG, VAR)]
  } else {
    uncer_noise_area <- dta[RANK_SEV > rank_threshold,]
    uncer_noise_area[,NOISE:= .N, by = .(YR, REG, VAR)]
  }
  
  ######################### PLOT ##############################
  years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > num_members,]$YR)
  years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > num_members,]$YR)
  years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > num_members,]$YR)
  years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > num_members,]$YR)
  
  if (reg == "CEU"){
    if (var == "s"){
      ggyears <- years_ceu_s
    } else {
      ggyears <- years_ceu_q
    }
  } else { 
    if (var == "s"){
      ggyears <- years_med_s
    } else {
      ggyears <- years_med_q
    }
  }
  
  if (prop == "Area"){
    base_plot <- ggplot(dta[REG == reg & VAR == var & RANK_AREA >= num_threshold[1] & YR %in% ggyears,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = num_threshold)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      #facet_wrap(~YR, ncol = col_num) +
      facet_wrap(~YR) +
      scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
      ggtitle(if (reg == "CEU"){
        if (var == "s"){
          "CEU Soil moisture drought / Area"
        } else {
          "CEU Runoff drought / Area"
        }
      } else { 
        if (var == "s"){
          "MED Soil moisture drought / Area"
        } else {
          "MED Runoff drought / Area"
        }
      }) +
      theme(strip.text = element_text(size = 7, colour = 'black'),
            legend.position = "none", 
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 12)) +
      panel_border(colour = "black")
  } else {
    base_plot <- ggplot(dta[REG == reg & VAR == var & RANK_SEV >= num_threshold[1] & YR %in% ggyears,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = num_threshold)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      #facet_wrap(~YR, ncol = col_num) +
      facet_wrap(~YR) +
      scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
      ggtitle(if (reg == "CEU"){
        if (var == "s"){
          "CEU Soil moisture drought / Severity"
        } else {
          "CEU Runoff drought / Severity"
        }
      } else { 
        if (var == "s"){
          "MED Soil moisture drought / Severity"
        } else {
          "MED Runoff drought / Severity"
        }
      }) +
      theme(strip.text = element_text(size = 7, colour = 'black'),
            legend.position = "none", 
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 12)) +
      panel_border(colour = "black")
  }
  #grid.newpage()
  #grid.draw(base_plot)
  base_plot_g <- ggplot_gtable(ggplot_build(base_plot))
  stript <- which(grepl('strip-t', base_plot_g$layout$name))
  yr_vec <- c()
  
  for (i in 1:length(stript)){
    k <- base_plot_g$grobs[which(grepl('strip-t', base_plot_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
    if (is.null(k)){
      k <- NA
      yr_vec <- append(yr_vec, k)
    } else {
      yr_vec <- append(yr_vec, k)
    }
  }
  
  num_nas <- length(which(is.na(yr_vec) == T))
  stript <- stript[!is.na(yr_vec)]
  yr_vec <- yr_vec[complete.cases(yr_vec)]
  yr_vec <- data.frame(YR = as.numeric(yr_vec))
  yr_vec$ORD <- 1:nrow(yr_vec)
  
  tab_col <- dta[REG == reg & VAR == "p" & YR %in% yr_vec$YR,]
  tab_col[RANK_SEV <= num_threshold[1] ,SEV_COL:= heavy_rain]
  tab_col[RANK_SEV > num_threshold[1] & RANK_SEV <= num_threshold[2] ,SEV_COL:= strong_rain]
  tab_col[RANK_SEV > num_threshold[2] & RANK_SEV <= num_threshold[3] ,SEV_COL:= mean_rain]
  tab_col[RANK_SEV > num_threshold[3] & RANK_SEV <= num_threshold[4] ,SEV_COL:= light_rain]
  
  tab_col[RANK_AREA <= num_threshold[1], AREA_COL:= heavy_rain]
  tab_col[RANK_AREA > num_threshold[1] & RANK_AREA <= num_threshold[2] ,AREA_COL:= strong_rain]
  tab_col[RANK_AREA > num_threshold[2] & RANK_AREA <= num_threshold[3] ,AREA_COL:= mean_rain]
  tab_col[RANK_AREA > num_threshold[3] & RANK_AREA <= num_threshold[4] ,AREA_COL:= light_rain]
  
  tab_col2 <- merge(x = tab_col, y = yr_vec, by.x = "YR", by.y = "YR")
  if (reg == "CEU"){
    common_years <- intersect(years_ceu_q, years_ceu_s)
  } else {
    common_years <- intersect(years_med_q, years_med_s)
  }
  common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
  
  tab_col2[YR %in% common_years, COM_COL:= "black"]
  tab_col2[YR %in% common_years, COM_THI:= 2]
  tab_col2[YR %in% common_years, COM_LTY:= 2]
  tab_col2[YR %in% common_all, COM_LTY:= 1]
  tab_col2[is.na(COM_LTY), COM_LTY:=0]
  tab_col3 <- tab_col2[order(tab_col2$ORD)]
  
  if (prop == "Area"){
    fills <- tab_col3$AREA_COL
  } else {
    fills <- tab_col3$SEV_COL
  }
  
  frames <- tab_col3$COM_COL
  frames_lty <- tab_col3$COM_LTY
  frames_thi <- tab_col3$COM_THI
  
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', base_plot_g$grobs[[i]]$grobs[[1]]$childrenOrder))
    base_plot_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  stript <- which(grepl('panel', base_plot_g$layout$name))
  common_yr_matr <- matrix(stript, nrow = wrap_dims(n = length(ggyears))[1], byrow = F)
  nrow_matr <- nrow(common_yr_matr)
  common_yr_vec <- c()
  
  for (i in nrow_matr:1){
    common_yr_vec <- append(common_yr_vec, common_yr_matr[i,])
  }
  
  not <- tail(common_yr_matr[max(nrow_matr),], num_nas)
  
  k <- 1
  for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
    j <- which(grepl('border', base_plot_g$grobs[[i]]$childrenOrder))
    base_plot_g$grobs[[i]]$children[[j]]$gp$col <- frames[k]
    base_plot_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_thi[k]
    base_plot_g$grobs[[i]]$children[[j]]$gp$lty <- frames_lty[k]
    k <- k+1
  }
  
  return(base_plot_g)
}

drought_compare <- function(data, rank_threshold, num_members, num_threshold, reg, prop){
  #' Funcion that plots multiple tile graphs allowing for comparison between CEU and MED region /
  #' runoff and soil moisture / Area and Severity
  #' 
  #' @param data input data
  #' @param rank_threshold threshold for ensemble members (0-250)
  #' @param num_members number of ensemble members over the rank threshold (0-100)
  #' @param num_threshold given thresholds for color classes (vector of lenght 4)
  #' @param reg region either "CEU" or "MED" or "BOTH" (character only) 
  #' @param prop property of the drought either "Area" or "Severity" or "Both" (character only) 
  #'
  #' @return comparison plot
  #'
  #' @examples drought_compare(data = x, rank_threshold = 240, num_members = 50,
  #'                           num_threshold = c(60, 220, 247, 250), reg = "CEU", 
  #'                           prop = "BOTH")
  
  library(data.table)
  library(ggplot2)
  library(grid)
  library(cowplot)
  
  if (prop == "Area"){
    if (reg == "CEU"){
      plot_q <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "q", prop = prop)
      plot_s <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "s", prop = prop)
      mat <- matrix(list(plot_q, plot_s), ncol = 2)
      z <- matrix(c(1, 2), ncol = 2)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(1, "null"), z = z))
    } else if (reg == "MED") {
      plot_q <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "q", prop = prop)
      plot_s <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "s", prop = prop)
      mat <- matrix(list(plot_q, plot_s), ncol = 2)
      z <- matrix(c(1, 2), ncol = 2)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(1, "null"), z = z))
    } else {
      plot_q_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "q", prop = prop)
      plot_s_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "s", prop = prop)
      plot_q_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "q", prop = prop)
      plot_s_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "s", prop = prop)
      mat <- matrix(list(plot_q_ceu, plot_s_ceu, plot_q_med, plot_s_med), nrow = 2)
      z <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(c(1, 1), "null"), z = z))
    }
  } else if (prop == "Severity"){
    if (reg == "CEU"){
      plot_q <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "q", prop = prop)
      plot_s <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "s", prop = prop)
      mat <- matrix(list(plot_q, plot_s), ncol = 2)
      z <- matrix(c(1, 2), ncol = 2)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(1, "null"), z = z))
    } else if (reg == "MED") {
      plot_q <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "q", prop = prop)
      plot_s <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                             num_threshold = num_threshold, reg = reg, var = "s", prop = prop)
      mat <- matrix(list(plot_q, plot_s), ncol = 2)
      z <- matrix(c(1, 2), ncol = 2)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(1, "null"), z = z))
    } else {
      plot_q_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "q", prop = prop)
      plot_s_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "s", prop = prop)
      plot_q_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "q", prop = prop)
      plot_s_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "s", prop = prop)
      mat <- matrix(list(plot_q_ceu, plot_s_ceu, plot_q_med, plot_s_med), nrow = 2)
      z <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(c(1, 1), "null"), z = z))
    }
  } else {
    if (reg == "CEU"){
      plot_q_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "q", prop = "Area")
      plot_s_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "s", prop = "Area")
      plot_q_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "q", prop = "Severity")
      plot_s_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "CEU", var = "s", prop = "Severity")
      mat <- matrix(list(plot_q_ceu, plot_s_ceu, plot_q_med, plot_s_med), nrow = 2)
      z <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(c(1, 1), "null"), z = z))
    } else {
      plot_q_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "q", prop = "Area")
      plot_s_ceu <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "s", prop = "Area")
      plot_q_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "q", prop = "Severity")
      plot_s_med <- source_tiles(data = data, rank_threshold = rank_threshold, num_members = num_members,
                                 num_threshold = num_threshold, reg = "MED", var = "s", prop = "Severity")
      mat <- matrix(list(plot_q_ceu, plot_s_ceu, plot_q_med, plot_s_med), nrow = 2)
      z <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
      grid.newpage()
      grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                      heights = unit(c(1, 1), "null"), z = z))
    } 
  }
}
