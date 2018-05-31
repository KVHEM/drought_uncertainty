#' Function for visual investigation of mHM model simulation results, 
#' which reconstruct drought over Europe for 1766-2015 CE at 0.5 deg resolution.
#'
#' Function creates tile graph of ranked drought events according to their 
#' characteristics modeled in mHM model with optinal timeline of modeled drought events together with 
#' drought events mentioned in scientific literature.
#'
#' @param data dataframe with ranked drought events  
#' @param thresh_rain thresholds for precipitation ranks
#' @param rain_wet colour for the "wettest" group of ranks
#' @param rain_dry colour for the "driest" group of ranks
#' @param thresh_drought thresholds for drought ranks
#' @param drought_wet colour for the "wettest" group of ranks
#' @param drought_dry colour for the "driest" group of ranks
#' @param thresh fixed threshold for selection most driest events
#' @param noise threshold for selection years with certain number of extreme events
#' @param lit_droughts NULL = NO timeline, vector of years = shows timeline
#' @param char_drought characteristic of drought "Severity"/"Area", "S"/"A" (not case sensitive)
#' @param type_drought type of drought "S"/"Q" (not case sensitive)
#' @param region "CEU" Central Europe, "MED" Mediterranean, "EUR" Europe
#'
#' @return tile graph of modeled events with optional timeline of literature drought events
#'
#' @examples droughts2tiles <- function(data = dta, thresh_rain = c(125, 224, 247), 
#'                                      rain_wet = "blue", rain_dry = "red", 
#'                                      thresh_drought = c(125, 224, 247),
#'                                      drought_wet = "yellow", drought_dry = "darkred", 
#'                                      thresh = 225, noise = 10, lit_droughts = c(1952, 1956),
#'                                      char_drought = "area", type_drought = "q", region = "CEU")

droughts2tiles <- function(data = dta, thresh_rain = quantile(1:250, probs = c(0.5, 0.75, 0.99)), 
                           rain_wet = "blue", rain_dry = "red", 
                           thresh_drought = quantile(1:250, probs = c(0.5, 0.75, 0.99)),
                           drought_wet = "yellow", drought_dry = "darkred", 
                           thresh = quantile(1:250, probs = c(0.9)), noise = 10, 
                           lit_droughts = c(1952, 1956, 1976, 1976, 1964),
                           char_drought = "area", type_drought = "q", region = "CEU"){
  
  #' Function for visual investigation of mHM model simulation results, 
  #' which reconstruct drought over Europe for 1766-2015 CE at 0.5 deg resolution.
  #'
  #' Function creates tile graph of ranked drought events according to their 
  #' characteristics modeled in mHM model with optinal timeline of modeled drought events together with 
  #' drought events mentioned in scientific literature.
  #'
  #' @param data dataframe with ranked drought events  
  #' @param thresh_rain thresholds for precipitation ranks
  #' @param rain_wet colour for the "wettest" group of ranks
  #' @param rain_dry colour for the "driest" group of ranks
  #' @param thresh_drought thresholds for drought ranks
  #' @param drought_wet colour for the "wettest" group of ranks
  #' @param drought_dry colour for the "driest" group of ranks
  #' @param thresh fixed threshold for selection most driest events
  #' @param noise threshold for selection years with certain number of extreme events
  #' @param lit_droughts NULL = NO timeline, vector of years = shows timeline
  #' @param char_drought characteristic of drought "Severity"/"Area", "S"/"A" (NOT CASE SENSITIVE)
  #' @param type_drought type of drought "s"/"q" (CASE SENSITIVE)
  #' @param region "CEU" Central Europe, "MED" Mediterranean, "EUR" Europe
  #'
  #' @return tile graph of modeled events with optional timeline of literature drought events
  #'
  #' @examples droughts2tiles <- function(data = dta, thresh_rain = c(125, 224, 247), 
  #'                                      rain_wet = "blue", rain_dry = "red", 
  #'                                      thresh_drought = c(125, 224, 247),
  #'                                      drought_wet = "yellow", drought_dry = "darkred", 
  #'                                      thresh = 225, noise = 10, lit_droughts = c(1952, 1956),
  #'                                      char_drought = "area", type_drought = "q", region = "CEU")
  
  library(data.table)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(ggpubr)
  library(cowplot)
  
  data <- data.table(data)
  # create colour palettes for precipitation and drought ranks
  rain_col <- colorRampPalette(c(rain_wet, rain_dry))(length(thresh_rain) + 1) 
  drought_col <- colorRampPalette(c(drought_wet, drought_dry))(length(thresh_drought))
  
  ##### Prepare raw tile graph #####
  
  if (toupper(substring(char_drought, 1, 1)) == "A"){  # if drought char is "AREA"
    data_noise <- data[RANK_AREA > thresh,] # select ranks above certain threshold
    data_noise[,NOISE:= .N, by = .(YR, REG, VAR)] # calculate number of events in one year
    # flag years with number of drought events above certain threshold
    years <- unique(data_noise[REG == region & VAR == type_drought & NOISE > noise, ]$YR)
    thresh_rain <- c(0, thresh_rain, max(data$RANK_AREA)) # edit thresholds for ggplot
    thresh_drought <- c(thresh_drought, max(data$RANK_AREA)) # edit thresholds for ggplot
    
    # tile ggplot of drought events
    plot <- ggplot(data[REG == region & VAR == type_drought & RANK_AREA >= min(thresh_drought) & YR %in% years,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = thresh_drought)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      facet_wrap(~YR, nrow = 3) +
      scale_fill_manual(values = drought_col) +
      theme(strip.text = element_text(size = 15, colour = 'black'),
            legend.position = "bottom", 
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18)) +
      labs(fill = "DROUGHT RANK")
    
    # extract legend from tile ggplot
    drought_legend <- get_legend(plot)
    
    if (length(lit_droughts) == 0){ # if no drought timeline, creates tile graph with title
      
      plot <- ggplot(data[REG == region & VAR == type_drought & RANK_AREA >= min(thresh_drought) & YR %in% years,]) +
        geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = thresh_drought)), colour = "white") +
        scale_x_continuous(breaks = c(1,5,10)) +
        scale_y_continuous(breaks = c(1,5,10)) +
        facet_wrap(~YR, nrow = 3) +
        scale_fill_manual(values = drought_col) +
        theme(strip.text = element_text(size = 15, colour = 'black'),
              legend.position = "none", 
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)) +
        labs(fill = "DROUGHT RANK", 
             title = ifelse(test = toupper(type_drought) == "S", 
                            yes = paste0(region, " ", "Soil drought", " ", "/", " ", "Area"),
                            no = paste0(region, " ", "Discharge drought", " ", "/", " ", "Area")))
    
      } else { # else creates tile graph without title
      
      plot <- ggplot(data[REG == region & VAR == type_drought & RANK_AREA >= min(thresh_drought) & YR %in% years,]) +
        geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = thresh_drought)), colour = "white") +
        scale_x_continuous(breaks = c(1,5,10)) +
        scale_y_continuous(breaks = c(1,5,10)) +
        facet_wrap(~YR, nrow = 3) +
        scale_fill_manual(values = drought_col) +
        theme(strip.text = element_text(size = 15, colour = 'black'),
              legend.position = "none", 
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)) +
        labs(fill = "DROUGHT RANK")
      }
    
  } else { # else (if) drought char is "SEVERITY"
    data_noise <- data[RANK_SEV > thresh,] # select ranks above certain threshold
    data_noise[,NOISE:= .N, by = .(YR, REG, VAR)] # calculate number of events in one year
    # flag years with number of drought events above certain threshold
    years <- unique(data_noise[REG == region & VAR == type_drought & NOISE > noise, ]$YR)
    thresh_rain <- c(0, thresh_rain, max(data$RANK_SEV)) # edit thresholds for ggplot
    thresh_drought <- c(thresh_drought, max(data$RANK_SEV)) # edit thresholds for ggplot
    
    # tile ggplot of drought events
    plot <- ggplot(data[REG == region & VAR == type_drought & RANK_SEV >= min(thresh_drought) & YR %in% years,]) +
      geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = thresh_drought)), colour = "white") +
      scale_x_continuous(breaks = c(1,5,10)) +
      scale_y_continuous(breaks = c(1,5,10)) +
      facet_wrap(~YR, nrow = 3) +
      scale_fill_manual(values = drought_col) +
      theme(strip.text = element_text(size = 15, colour = 'black'),
            legend.position = "bottom", 
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18)) +
      labs(fill = "DROUGHT RANK")
    
    # extract legend from tile ggplot
    drought_legend <- get_legend(plot)
    
    if (length(lit_droughts) == 0){ # if no drought timeline creates tile graph with title
      
      plot <- ggplot(data[REG == region & VAR == type_drought & RANK_SEV >= min(thresh_drought) & YR %in% years,]) +
        geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = thresh_drought)), colour = "white") +
        scale_x_continuous(breaks = c(1,5,10)) +
        scale_y_continuous(breaks = c(1,5,10)) +
        facet_wrap(~YR, nrow = 3) +
        scale_fill_manual(values = drought_col) +
        theme(strip.text = element_text(size = 15, colour = 'black'),
              legend.position = "none", 
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)) +
        labs(fill = "DROUGHT RANK",
             title = ifelse(test = toupper(type_drought) == "Q", 
                    yes = paste0(region, " ", "Discharge drought", " ", "/", " ", "Severity"),
                    no = paste0(region, " ", "Soil drought", " ", "/", " ", "Severity")))
    
      } else { # else create tile graph without title
      
      plot <- ggplot(data[REG == region & VAR == type_drought & RANK_SEV >= min(thresh_drought) & YR %in% years,]) +
        geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = thresh_drought)), colour = "white") +
        scale_x_continuous(breaks = c(1,5,10)) +
        scale_y_continuous(breaks = c(1,5,10)) +
        facet_wrap(~YR, nrow = 3) +
        scale_fill_manual(values = drought_col) +
        theme(strip.text = element_text(size = 15, colour = 'black'),
              legend.position = "none", 
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18)) +
        labs(fill = "DROUGHT RANK")
    }
  }
  
  plot_gtable <- ggplot_gtable(ggplot_build(plot)) # create grob from ggplot
  
  ##### Label edit #####
  # positions (indexes) of labels of tile graph in grob file
  stript <- which(grepl('strip-t', plot_gtable$layout$name)) 
  yr_vec <- c() # empty vector
  
  # for-cycle for extraction labels from tile graph (creates vector of labels)
  for (i in 1:length(stript)){
    k <- plot_gtable$grobs[which(grepl('strip-t', plot_gtable$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
    if (is.null(k)){
      k <- NA
      yr_vec <- append(yr_vec, k)
    } else {
      yr_vec <- append(yr_vec, k)
    }
  }
  
  num_nas <- length(which(is.na(yr_vec)==T)) # number of NAs in label vector
  stript <- stript[!is.na(yr_vec)] # indexes of NA labels
  yr_vec <- yr_vec[complete.cases(yr_vec)] # exclude NA labels
  yr_vec <- data.frame(YR = as.numeric(yr_vec)) # create dataframe
  yr_vec$ORD <- 1:nrow(yr_vec) # add order of the labels
  
  # extract from input data only years in labels
  tab_color <- data[REG == region & VAR == "p" & YR %in% yr_vec$YR,]
  # for-cycle for adding right colour to each label in data.frame 
  k <- 1
  for (i in c(seq(1,length(thresh_rain)-1,1))){
    low <- thresh_rain[1:length(thresh_rain)-1][i]
    high <- thresh_rain[2:length(thresh_rain)][i]
    if (toupper(substring(char_drought, 1, 1)) == "A"){
      tab_color[RANK_AREA > low & RANK_AREA <= high ,AREA_COL:= rain_col[i]]
    } else {
      tab_color[RANK_SEV > low & RANK_SEV <= high ,SEV_COL:= rain_col[i]]
    }
  }
  # merge colour data.frame with vector of labels
  tab_color <- merge(x = tab_color, y = yr_vec, by.x = "YR", by.y = "YR")
  tab_color2 <- tab_color[order(tab_color$ORD)] # order colour data.frame by order
  # create vetor of colours for labels
  if (toupper(substring(char_drought, 1, 1)) == "A"){
    fills <- tab_color2$AREA_COL
  } else {
    fills <- tab_color2$SEV_COL
  }
  # for-cycle for assigning right colour to labels in tile graph
  k <- 1
  for (i in stript) {
    j <- which(grepl('rect', plot_gtable$grobs[[i]]$grobs[[1]]$childrenOrder))
    plot_gtable$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  #grid.newpage()
  #grid.draw(plot_gtable)
  
  ##### Point graph #####
  # if vector of literature events present and char of drought is AREA
  if (length(lit_droughts) != 0 && (toupper(substring(char_drought, 1, 1)) == "A")){
    
    # which literature events meets modeled events
    flag_droughts <- intersect(x = years, y = lit_droughts) 
    # the rest of the literature events
    paper_droughts <- lit_droughts[which(lit_droughts > 1765)][!lit_droughts[which(lit_droughts > 1765)] %in% years]
    
    # ggplot of timelie of modeled and literature droughts
    point <- ggplot(data.frame(YR = years, EVENT = rep(x = 0, times = length(years)))) +
      geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
      geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
      geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 12) +
      geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
                 aes(x = YR, y = Y), shape = 25, fill = "red") +
      geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
                 aes(x = YR, y = Y), shape = 16, color = "grey") +
      labs(title = ifelse(test = toupper(type_drought) == "S", 
                          yes = paste0(region, " ", "Soil drought", " ", "/", " ", "Area"),
                          no = paste0(region, " ", "Discharge drought", " ", "/", " ", "Area"))) +
      theme(axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            plot.title = element_text(size = 25)) +
      scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                         labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
      scale_color_manual(values = tab_color2[order(tab_color2$YR)]$AREA_COL, guide = FALSE) +
      coord_cartesian(expand = FALSE)
    
    # create grob from point ggplot 
    point_gtable <- ggplot_gtable(ggplot_build(point))
    
  }
  
  # if vector of literature events present and char of drought is "SEVERITY"
  if (length(lit_droughts) != 0 && (toupper(substring(char_drought, 1, 1)) == "S")) {
    
    # which literature events meets modeled events
    flag_droughts <- intersect(x = years, y = lit_droughts)
    # the rest of the literature events
    paper_droughts <- lit_droughts[which(lit_droughts > 1765)][!lit_droughts[which(lit_droughts > 1765)] %in% years]
    
    # ggplot of timeline of modeled and literature droughts
    point <- ggplot(data.frame(YR = years, EVENT = rep(x = 0, times = length(years)))) +
      geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
      geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
      geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 12) +
      geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
                 aes(x = YR, y = Y), shape = 25, fill = "red") +
      geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
                 aes(x = YR, y = Y), shape = 16, color = "grey") +
      labs(title = ifelse(test = toupper(type_drought) == "Q", 
                          yes = paste0(region, " ", "Discharge drought", " ", "/", " ", "Severity"),
                          no = paste0(region, " ", "Soil drought", " ", "/", " ", "Severity"))) +
      theme(axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            plot.title = element_text(size = 25)) +
      scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                         labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
      scale_color_manual(values = tab_color2[order(tab_color2$YR)]$SEV_COL, guide = FALSE) +
      coord_cartesian(expand = FALSE)
    
    # create grob from point ggplot 
    point_gtable <- ggplot_gtable(ggplot_build(point))
  }
  
  # fabricated ggplot for ggplot legend extraction
  rain_plot <- ggplot(data = data.frame(x = 1:250, y = 1:250)) +
    geom_polygon(aes(x = x, y = y, fill = cut(x, breaks = thresh_rain))) +
    theme(legend.position = "bottom") +
    labs(fill = "PREC RANK") +
    scale_fill_manual(values = rain_col)
  
  # extract legend from ggplot
  rain_legend <- get_legend(rain_plot)
  
  if (length(lit_droughts) == 0){ # if no timeline of drought events plot tile graph
    grid.newpage()
    grid.draw(arrangeGrob(grobs = list(plot_gtable, drought_legend, rain_legend),
                          layout_matrix = matrix(c(1,1,2,3), byrow = T, ncol = 2),
                          heights = c(1,0.1)))
  }
  
  if (length(lit_droughts) != 0){ # if timeline of drought events plot tile graph with timeline
    grid.newpage()
    grid.draw(arrangeGrob(grobs = list(point_gtable, plot_gtable, drought_legend, rain_legend), 
                          layout_matrix = matrix(c(1,1,2,2,3,4), byrow = T, ncol = 2), 
                          heights = c(0.25,1,0.1)))
  }
}