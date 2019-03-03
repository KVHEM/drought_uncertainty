library(data.table)
library(ggplot2)
library(viridisLite)
library(viridis)
library(grid)
library(gridExtra)
library(ggpubr)
library(cowplot)

############################## PREPARE DATA ####################################
# uncer_raw <- data.table(readRDS("D:/RGROUP/DATA/new_PAR_MET (1).rds"))
# uncer_raw$MET <- NULL
# uncer_raw$PAR <- NULL
# colnames(uncer_raw) <- c("MET", "PAR", "REG", "var", "yr", "SEVERITY", "AREA") 
# uncer_raw[, REG:=factor(REG, levels = c('CEU', 'MED', 'EUR'))]
# uncer_raw[, ID:= paste0(MET, "_", PAR)]
# uncer_raw[, RANK_SEV:= rank(SEVERITY, na.last = FALSE), by = .(REG, ID, var)]
# uncer_raw[, RANK_AREA:= rank(AREA, na.last = FALSE), by = .(REG, ID, var)]
# colnames(uncer_raw) <- c("MET", "PAR", "REG", "VAR", "YR", "SEVERITY", "AREA", "ID", "RANK_SEV", "RANK_AREA")
# ###### ID 5_2 MET_PAR###### 
# 
# dat5_2 <- uncer_raw[ID %in% c("5_1", "5_3", "6_2", "4_2"), {SEVERITY = mean(SEVERITY); AREA = mean(AREA); RANK_SEV = mean(RANK_SEV);
# RANK_AREA = mean(RANK_AREA); list(SEVERITY = SEVERITY, AREA = AREA, RANK_SEV = RANK_SEV, RANK_AREA = RANK_AREA)},
# by = .(REG,VAR, YR)]
# dat5_2[,MET:= 5]
# dat5_2[,PAR:= 2]
# dat5_2[, ID:= paste0(MET, "_", PAR)]
# setcolorder(dat5_2, neworder = colnames(uncer_raw))
# uncer_raw <- rbind(uncer_raw, dat5_2)

uncer_raw <- data.table(readRDS("D:/RGROUP/DATA/extremity_ens_EUR_FIXED.rds"))

size_axis_title <- 10; size_axis_text <- 9; size_title <- 12; size_strip <- 8; size_thick <- 2

######### COLORS  BROWN ########
heavy_rain <- "white"; strong_rain <- '#f2c4a3'; mean_rain <- '#e68d4d'; light_rain <- '#b85b19'
strong_drought_s <- '#b85b19'; mean_drought_s <- '#e68d4d'; light_drought_s <- '#f2c4a3'
strong_drought_q <- '#b85b19'; mean_drought_q <- '#e68d4d'; light_drought_q <- '#f2c4a3'

############################### TILES vs YEARS #################################

ggplot(uncer_raw[REG == "CEU" & VAR == "s" & RANK_AREA > 240,]) +
  geom_tile(aes(x = PAR, y = MET, fill = RANK_AREA), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 25, ncol = 10) +
  scale_fill_viridis(option = "D",direction = -1)
#theme(
#  strip.background = element_blank(),
#  strip.text.x = element_blank()
#)

######################### TILES AREA vs YEARS wo NOISE #########################

######################### CEU Soil drought / Area ##############################
uncer_noise_area <- uncer_raw[RANK_AREA > 225,]
uncer_noise_area[,NOISE:= .N, by = .(YR, REG, VAR)]

################ RAW #####
p <- ggplot(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 5) +
  scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  ggtitle("CEU Soil drought / Area") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "bottom") +
  panel_border(colour = "black")
########################## Literature droughts ##################################
lit_droughts_ceu_q <- c(1540, 1616, 1893, 1921, 2003, 2006, 2007, 2010, 2013, 2015, #Hanel
                        1952, 1956, 1976, 1976, 1964, #Spinoni (area)
                        1950, 1953, 1954, 1959, 1960, 1972:1974, 1983, 1992, 1996:1997, #Spinoni (genereal)
                        1999:2001, 2004:2005, 2008, 2011) #Spinoni (genereal)

lit_droughts_ceu_s <- c(1540, 1616, 1893, 1921, 2003, 2006, 2007, 2010, 2013, 2015) #Hanel

lit_droughts_med_q <- c(1540, 1616, 1893, 1921, 2003, 2006, 2007, 2010, 2013, 2015, #Hanel
                        2001, 2005, 2002, 1989, #Spinoni (area)
                        1950:1954, 1972:1974, 1985, 1989:1991, 1995, 2004:2005, 2008) #Spinoni (genereal)

lit_droughts_med_s <- c(1540, 1616, 1893, 1921, 2003, 2006, 2007, 2010, 2013, 2015) #Hanel
######################### CEU Soil drought / Area ##############################
years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > 10,]$YR)
years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > 10,]$YR)
years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > 10,]$YR)
years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > 10,]$YR)

ceu_s <- ggplot(uncer_raw[REG == "CEU" & VAR == "s" & RANK_AREA >= 125 & YR %in% years_ceu_s,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = c(125, 224, 247, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 4) +
  #scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
  ggtitle("CEU Soil moisture drought / Area") +
  theme(strip.text = element_text(size = size_strip, colour = 'black'),
        legend.position = "none", 
        axis.title = element_text(size = size_axis_title),
        axis.text = element_text(size = size_axis_text),
        plot.title = element_text(size = size_title)) +
  panel_border(colour = "black") +
  labs(x = "Model parameterization set number", y = "Meteorological forcing set number")

ceu_s_g <- ggplot_gtable(ggplot_build(ceu_s))
stript <- which(grepl('strip-t', ceu_s_g$layout$name))
yr_vec <- c()

for (i in 1:length(stript)){
  k <- ceu_s_g$grobs[which(grepl('strip-t', ceu_s_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
  if (is.null(k)){
    k <- NA
    yr_vec <- append(yr_vec, k)
  } else {
    yr_vec <- append(yr_vec, k)
  }
}

num_nas <- length(which(is.na(yr_vec)==T))
stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_ceu <- uncer_raw[REG == "CEU" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_ceu[RANK_SEV <= 125 ,SEV_COL:= heavy_rain]
tab_col_ceu[RANK_SEV > 125 & RANK_SEV <= 224 ,SEV_COL:= strong_rain]
tab_col_ceu[RANK_SEV > 224 & RANK_SEV <= 247 ,SEV_COL:= mean_rain]
tab_col_ceu[RANK_SEV > 247 & RANK_SEV <= 250 ,SEV_COL:= light_rain]
#tab_col_ceu[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
#tab_col_ceu[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_ceu[RANK_AREA <= 125, AREA_COL:= heavy_rain]
tab_col_ceu[RANK_AREA > 125 & RANK_AREA <= 224 ,AREA_COL:= strong_rain]
tab_col_ceu[RANK_AREA > 224 & RANK_AREA <= 247 ,AREA_COL:= mean_rain]
tab_col_ceu[RANK_AREA > 247 & RANK_AREA <= 250 ,AREA_COL:= light_rain]
#tab_col_ceu[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
#tab_col_ceu[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_ceu2 <- merge(x = tab_col_ceu, y = yr_vec, by.x = "YR", by.y = "YR")
common_years_ceu <- intersect(years_ceu_q, years_ceu_s)
common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
tab_col_ceu2[YR %in% common_years_ceu, COM_COL:= "black"]
tab_col_ceu2[YR %in% common_years_ceu, COM_THI:= size_thick]
#tab_col_ceu2[YR %in% common_all, COM_THI:= 4]
tab_col_ceu2[YR %in% common_years_ceu, COM_LTY:= 2]
tab_col_ceu2[YR %in% common_all, COM_LTY:= 1]
tab_col_ceu2[is.na(COM_LTY), COM_LTY:=0]
tab_col_ceu3 <- tab_col_ceu2[order(tab_col_ceu2$ORD)]

fills_ceu <- tab_col_ceu3$AREA_COL
frames_ceu <- tab_col_ceu3$COM_COL
frames_ceu_lty <- tab_col_ceu3$COM_LTY
frames_ceu_thi <- tab_col_ceu3$COM_THI

k <- 1
for (i in stript) {
  j <- which(grepl('rect', ceu_s_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  ceu_s_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_ceu[k]
  k <- k+1
}

stript <- which(grepl('panel', ceu_s_g$layout$name))
#length(stript) <- prod(dim(matrix(stript, nrow = 3)))
common_yr_matr <- matrix(stript, nrow = 4, byrow = F)
#common_yr_vec <- c(common_yr_matr[6,], common_yr_matr[5,], common_yr_matr[4,],
 #                  common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
common_yr_vec <- c(common_yr_matr[4,],
                   common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
#not <- tail(common_yr_matr[6,], num_nas)
not <- tail(common_yr_matr[4,], num_nas)

k <- 1
for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
  j <- which(grepl('border', ceu_s_g$grobs[[i]]$childrenOrder))
  ceu_s_g$grobs[[i]]$children[[j]]$gp$col <- frames_ceu[k]
  ceu_s_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_ceu_thi[k]
  ceu_s_g$grobs[[i]]$children[[j]]$gp$lty <- frames_ceu_lty[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(ceu_s_g)

############################ POINT CEU S #######################################
flag_droughts <- intersect(x = years_ceu_s, y = lit_droughts_ceu_s)
paper_droughts <- lit_droughts_ceu_s[which(lit_droughts_ceu_s > 1765)][!lit_droughts_ceu_s[which(lit_droughts_ceu_s > 1765)] %in% years_ceu_s]

point_ceu_s <- ggplot(data.frame(YR = years_ceu_s, EVENT = rep(x = 0, times = length(years_ceu_s)))) +
  geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
  geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
  geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 5) +
  #geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
  #           aes(x = YR, y = Y), shape = 25, fill = "red") +
  #geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
  #           aes(x = YR, y = Y), shape = 16, color = "grey") +
  ggtitle("CEU Soil moisture drought / Area") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7, angle = 45),
        plot.title = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                     labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
  scale_color_manual(values = tab_col_ceu2[order(tab_col_ceu2$YR)]$AREA_COL, guide = FALSE) +
  coord_cartesian(expand = FALSE)

point_ceu_s_g <- ggplot_gtable(ggplot_build(point_ceu_s))

############ MED Soil drought / Area ##########
years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > 10,]$YR)
years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > 10,]$YR)
years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > 10,]$YR)
years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > 10,]$YR)

med_s <- ggplot(uncer_raw[REG == "MED" & VAR == "s" & RANK_AREA >= 125 & YR %in% years_med_s,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = c(125, 224, 247, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 4) +
  #scale_fill_viridis("RANK", option = "D", direction = 1, discrete = T) +
  scale_fill_manual(values = c(light_drought_s, mean_drought_s, strong_drought_s)) +
  ggtitle("MED Soil moisture drought / Area") +
  theme(strip.text = element_text(size = size_strip, colour = 'black'),
        legend.position = "none", 
        axis.title = element_text(size = size_axis_title),
        axis.text = element_text(size = size_axis_text),
        plot.title = element_text(size = size_title)) +
  panel_border(colour = "black") +
  labs(x = "Model parameterization set number", y = "Meteorological forcing set number")

med_s_g <- ggplot_gtable(ggplot_build(med_s))
stript <- which(grepl('strip-t', med_s_g$layout$name))
yr_vec <- c()

for (i in 1:length(stript)){
  k <- med_s_g$grobs[which(grepl('strip-t', med_s_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
  if (is.null(k)){
    k <- NA
    yr_vec <- append(yr_vec, k)
  } else {
    yr_vec <- append(yr_vec, k)
  }
}

num_nas <- length(which(is.na(yr_vec)==T))
stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_med <- uncer_raw[REG == "MED" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_med[RANK_SEV <= 125 ,SEV_COL:= heavy_rain]
tab_col_med[RANK_SEV > 125 & RANK_SEV <= 224 ,SEV_COL:= strong_rain]
tab_col_med[RANK_SEV > 224 & RANK_SEV <= 247 ,SEV_COL:= mean_rain]
tab_col_med[RANK_SEV > 250 & RANK_SEV <= 250 ,SEV_COL:= light_rain]
#tab_col_med[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
#tab_col_med[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_med[RANK_AREA <= 125, AREA_COL:= heavy_rain]
tab_col_med[RANK_AREA > 125 & RANK_AREA <= 224 ,AREA_COL:= strong_rain]
tab_col_med[RANK_AREA > 224 & RANK_AREA <= 247 ,AREA_COL:= mean_rain]
tab_col_med[RANK_AREA > 247 & RANK_AREA <= 250 ,AREA_COL:= light_rain]
#tab_col_med[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
#tab_col_med[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_med2 <- merge(x = tab_col_med, y = yr_vec, by.x = "YR", by.y = "YR")
common_years_med <- intersect(years_med_q, years_med_s)
common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
tab_col_med2[YR %in% common_years_med, COM_COL:= "black"]
tab_col_med2[YR %in% common_years_med, COM_THI:= size_thick]
#tab_col_med2[YR %in% common_all, COM_THI:= 4]
tab_col_med2[YR %in% common_years_med, COM_LTY:= 2]
tab_col_med2[YR %in% common_all, COM_LTY:= 1]
tab_col_med2[is.na(COM_LTY), COM_LTY:=0]
tab_col_med3 <- tab_col_med2[order(tab_col_med2$ORD)]

fills_med <- tab_col_med3$AREA_COL
frames_med <- tab_col_med3$COM_COL
frames_med_lty <- tab_col_med3$COM_LTY
frames_med_thi <- tab_col_med3$COM_THI

k <- 1
for (i in stript) {
  j <- which(grepl('rect', med_s_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  med_s_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_med[k]
  k <- k+1
}

stript <- which(grepl('panel', med_s_g$layout$name))
#length(stript) <- prod(dim(matrix(stript, nrow = 3)))
common_yr_matr <- matrix(stript, nrow = 6, byrow = F)
#common_yr_vec <- c(common_yr_matr[6,], common_yr_matr[5,], common_yr_matr[4,], 
 #                  common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
common_yr_vec <- c(common_yr_matr[4,], 
                   common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
#not <- tail(common_yr_matr[6,], num_nas)
not <- tail(common_yr_matr[4,], num_nas)

k <- 1
for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
  j <- which(grepl('border', med_s_g$grobs[[i]]$childrenOrder))
  med_s_g$grobs[[i]]$children[[j]]$gp$col <- frames_med[k]
  med_s_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_med_thi[k]
  med_s_g$grobs[[i]]$children[[j]]$gp$lty <- frames_med_lty[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(med_s_g)

############################ POINT MED S #######################################
flag_droughts <- intersect(x = years_med_s, y = lit_droughts_med_s)
paper_droughts <- lit_droughts_med_s[which(lit_droughts_med_s > 1765)][!lit_droughts_med_s[which(lit_droughts_med_s > 1765)] %in% years_med_s]

point_med_s <- ggplot(data.frame(YR = years_med_s, EVENT = rep(x = 0, times = length(years_med_s)))) +
  geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
  geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
  geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 5) +
  #geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
  #           aes(x = YR, y = Y), shape = 25, fill = "red") +
  #geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
  #           aes(x = YR, y = Y), shape = 16, color = "grey") +
  ggtitle("MED Soil moisture drought / Area") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7, angle = 45),
        plot.title = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                     labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
  scale_color_manual(values = tab_col_med2[order(tab_col_med2$YR)]$AREA_COL, guide = FALSE) +
  coord_cartesian(expand = FALSE)

point_med_s_g <- ggplot_gtable(ggplot_build(point_med_s))

##################### CEU Discharge drought / Area #############################
years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > 10,]$YR)
years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > 10,]$YR)
years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > 10,]$YR)
years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > 10,]$YR)

ceu_q <- ggplot(uncer_raw[REG == "CEU" & VAR == "q" & RANK_AREA >= 125 & YR %in% years_ceu_q,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = c(125, 224, 247, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 4) +
  #scale_fill_viridis("RANK", option = "D",direction = 1, discrete = T) +
  scale_fill_manual(values = c(light_drought_q, mean_drought_q, strong_drought_q)) +
  ggtitle("CEU Runoff drought / Area") +
  theme(strip.text = element_text(size = size_strip, colour = 'black'),
        legend.position = "none", 
        axis.title = element_text(size = size_axis_title),
        axis.text = element_text(size = size_axis_text),
        plot.title = element_text(size = size_title)) +
  panel_border(colour = "black") +
  labs(x = "Model parameterization set number", y = "Meteorological forcing set number")

ceu_q_g <- ggplot_gtable(ggplot_build(ceu_q))
stript <- which(grepl('strip-t', ceu_q_g$layout$name))
yr_vec <- c()

for (i in 1:length(stript)){
  k <- ceu_q_g$grobs[which(grepl('strip-t', ceu_q_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
  if (is.null(k)){
    k <- NA
    yr_vec <- append(yr_vec, k)
  } else {
    yr_vec <- append(yr_vec, k)
  }
}

num_nas <- length(which(is.na(yr_vec)==T))
stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_ceu <- uncer_raw[REG == "CEU" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_ceu[RANK_SEV <= 125 ,SEV_COL:= heavy_rain]
tab_col_ceu[RANK_SEV > 125 & RANK_SEV <= 224 ,SEV_COL:= strong_rain]
tab_col_ceu[RANK_SEV > 224 & RANK_SEV <= 247 ,SEV_COL:= mean_rain]
tab_col_ceu[RANK_SEV > 247 & RANK_SEV <= 250 ,SEV_COL:= light_rain]
#tab_col_ceu[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
#tab_col_ceu[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_ceu[RANK_AREA <= 125, AREA_COL:= heavy_rain]
tab_col_ceu[RANK_AREA > 125 & RANK_AREA <= 224 ,AREA_COL:= strong_rain]
tab_col_ceu[RANK_AREA > 224 & RANK_AREA <= 247 ,AREA_COL:= mean_rain]
tab_col_ceu[RANK_AREA > 247 & RANK_AREA <= 250 ,AREA_COL:= light_rain]
#tab_col_ceu[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
#tab_col_ceu[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_ceu2 <- merge(x = tab_col_ceu, y = yr_vec, by.x = "YR", by.y = "YR")
common_years_ceu <- intersect(years_ceu_q, years_ceu_s)
common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
tab_col_ceu2[YR %in% common_years_ceu, COM_COL:= "black"]
tab_col_ceu2[YR %in% common_years_ceu, COM_THI:= size_thick]
#tab_col_ceu2[YR %in% common_all, COM_THI:= 4]
tab_col_ceu2[YR %in% common_years_ceu, COM_LTY:= 2]
tab_col_ceu2[YR %in% common_all, COM_LTY:= 1]
tab_col_ceu2[is.na(COM_LTY), COM_LTY:=0]
tab_col_ceu3 <- tab_col_ceu2[order(tab_col_ceu2$ORD)]

fills_ceu <- tab_col_ceu3$AREA_COL
frames_ceu <- tab_col_ceu3$COM_COL
frames_ceu_lty <- tab_col_ceu3$COM_LTY
frames_ceu_thi <- tab_col_ceu3$COM_THI

k <- 1
for (i in stript) {
  j <- which(grepl('rect', ceu_q_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  ceu_q_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_ceu[k]
  k <- k+1
}

stript <- which(grepl('panel', ceu_q_g$layout$name))
#length(stript) <- prod(dim(matrix(stript, nrow = 3)))
common_yr_matr <- matrix(stript, nrow = 4, byrow = F)
#common_yr_vec <- c(common_yr_matr[6,], common_yr_matr[5,], common_yr_matr[4,],
 #                  common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
common_yr_vec <- c(common_yr_matr[4,],
                   common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
#not <- tail(common_yr_matr[6,], num_nas)
not <- tail(common_yr_matr[4,], num_nas)

k <- 1
for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
  j <- which(grepl('border', ceu_q_g$grobs[[i]]$childrenOrder))
  ceu_q_g$grobs[[i]]$children[[j]]$gp$col <- frames_ceu[k]
  ceu_q_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_ceu_thi[k]
  ceu_q_g$grobs[[i]]$children[[j]]$gp$lty <- frames_ceu_lty[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(ceu_q_g)

############################ POINT CEU Q #######################################
flag_droughts <- intersect(x = years_ceu_q, y = lit_droughts_ceu_q)
paper_droughts <- lit_droughts_ceu_q[which(lit_droughts_ceu_q > 1765)][!lit_droughts_ceu_q[which(lit_droughts_ceu_q > 1765)] %in% years_ceu_q]

point_ceu_q <- ggplot(data.frame(YR = years_ceu_q, EVENT = rep(x = 0, times = length(years_ceu_q)))) +
  geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
  geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
  geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 5) +
  #geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
  #           aes(x = YR, y = Y), shape = 25, fill = "red") +
  #geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
  #           aes(x = YR, y = Y), shape = 16, color = "grey") +
  ggtitle("CEU Runoff drought / Area") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7, angle = 45),
        plot.title = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                     labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
  scale_color_manual(values = tab_col_ceu2[order(tab_col_ceu2$YR)]$AREA_COL, guide = FALSE) +
  coord_cartesian(expand = FALSE)

point_ceu_q_g <- ggplot_gtable(ggplot_build(point_ceu_q))

######################### MED Discharge drought / Area #########################
years_ceu_s <- unique(uncer_noise_area[REG == "CEU" & VAR == "s" & NOISE > 10,]$YR)
years_ceu_q <- unique(uncer_noise_area[REG == "CEU" & VAR == "q" & NOISE > 10,]$YR)
years_med_s <- unique(uncer_noise_area[REG == "MED" & VAR == "s" & NOISE > 10,]$YR)
years_med_q <- unique(uncer_noise_area[REG == "MED" & VAR == "q" & NOISE > 10,]$YR)

med_q <- ggplot(uncer_raw[REG == "MED" & VAR == "q" & RANK_AREA >= 125 & YR %in% years_med_q,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_AREA, breaks = c(125, 224, 247, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 4) +
  #scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  scale_fill_manual(values = c(light_drought_q, mean_drought_q, strong_drought_q)) +
  ggtitle("MED Runoff drought / Area") +
  theme(strip.text = element_text(size = size_strip, colour = 'black'),
        legend.position = "none", 
        axis.title = element_text(size = size_axis_title),
        axis.text = element_text(size = size_axis_text),
        plot.title = element_text(size = size_title)) +
  panel_border(colour = "black") +
  labs(x = "Model parameterization set number", y = "Meteorological forcing set number")

med_q_g <- ggplot_gtable(ggplot_build(med_q))
stript <- which(grepl('strip-t', med_q_g$layout$name))
yr_vec <- c()

for (i in 1:length(stript)){
  k <- med_q_g$grobs[which(grepl('strip-t', med_q_g$layout$name))][[i]][["grobs"]][[1]][["children"]][[2]][["children"]][[1]]$label
  if (is.null(k)){
    k <- NA
    yr_vec <- append(yr_vec, k)
  } else {
    yr_vec <- append(yr_vec, k)
  }
}

num_nas <- length(which(is.na(yr_vec)==T))
stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_med <- uncer_raw[REG == "MED" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_med[RANK_SEV <= 125 ,SEV_COL:= heavy_rain]
tab_col_med[RANK_SEV > 125 & RANK_SEV <= 224 ,SEV_COL:= strong_rain]
tab_col_med[RANK_SEV > 224 & RANK_SEV <= 247 ,SEV_COL:= mean_rain]
tab_col_med[RANK_SEV > 250 & RANK_SEV <= 250 ,SEV_COL:= light_rain]
#tab_col_med[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
#tab_col_med[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_med[RANK_AREA <= 125, AREA_COL:= heavy_rain]
tab_col_med[RANK_AREA > 125 & RANK_AREA <= 224 ,AREA_COL:= strong_rain]
tab_col_med[RANK_AREA > 224 & RANK_AREA <= 247 ,AREA_COL:= mean_rain]
tab_col_med[RANK_AREA > 247 & RANK_AREA <= 250 ,AREA_COL:= light_rain]
#tab_col_med[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
#tab_col_med[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_med2 <- merge(x = tab_col_med, y = yr_vec, by.x = "YR", by.y = "YR")
common_years_med <- intersect(years_med_q, years_med_s)
common_all <- Reduce(intersect, list(years_ceu_q, years_ceu_s, years_med_q, years_med_s))
tab_col_med2[YR %in% common_years_med, COM_COL:= "black"]
tab_col_med2[YR %in% common_years_med, COM_THI:= size_thick]
#tab_col_med2[YR %in% common_all, COM_THI:= 4]
tab_col_med2[YR %in% common_years_med, COM_LTY:= 2]
tab_col_med2[YR %in% common_all, COM_LTY:= 1]
tab_col_med2[is.na(COM_LTY), COM_LTY:=0]
tab_col_med3 <- tab_col_med2[order(tab_col_med2$ORD)]

fills_med <- tab_col_med3$AREA_COL
frames_med <- tab_col_med3$COM_COL
frames_med_lty <- tab_col_med3$COM_LTY
frames_med_thi <- tab_col_med3$COM_THI

k <- 1
for (i in stript) {
  j <- which(grepl('rect', med_q_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  med_q_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_med[k]
  k <- k+1
}

stript <- which(grepl('panel', med_q_g$layout$name))
#length(stript) <- prod(dim(matrix(stript, nrow = 3)))
common_yr_matr <- matrix(stript, nrow = 6, byrow = F)
#common_yr_vec <- c(common_yr_matr[6,], common_yr_matr[5,], common_yr_matr[4,], 
 #                  common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
common_yr_vec <- c(common_yr_matr[4,], 
                   common_yr_matr[3,], common_yr_matr[2,], common_yr_matr[1,])
#not <- tail(common_yr_matr[6,], num_nas)
not <- tail(common_yr_matr[4,], num_nas)

k <- 1
for (i in common_yr_vec[which(!common_yr_vec %in% not)]) {
  j <- which(grepl('border', med_q_g$grobs[[i]]$childrenOrder))
  med_q_g$grobs[[i]]$children[[j]]$gp$col <- frames_med[k]
  med_q_g$grobs[[i]]$children[[j]]$gp$lwd <- frames_med_thi[k]
  med_q_g$grobs[[i]]$children[[j]]$gp$lty <- frames_med_lty[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(med_q_g)

############################ POINT MED Q #######################################
flag_droughts <- intersect(x = years_med_q, y = lit_droughts_med_q)
paper_droughts <- lit_droughts_med_q[which(lit_droughts_med_q > 1765)][!lit_droughts_med_q[which(lit_droughts_med_q > 1765)] %in% years_med_q]

point_med_q <- ggplot(data.frame(YR = years_med_q, EVENT = rep(x = 0, times = length(years_med_q)))) +
  geom_rect(data = data.frame(xmin = 1764, xmax = 2017, ymin = -0.02, ymax = 0.05), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA) +
  geom_line(data = data.frame(YR = c(1764, 2017), Y = c(0,0)), aes(x = YR, y = Y), size = 0.3, col = "grey") +
  geom_point(aes(x = YR, y = EVENT, color = factor(YR)), shape = "|", lwd = 5) +
  #geom_point(data = data.frame(YR = flag_droughts, Y = rep(0.03, times = length(flag_droughts))), 
  #           aes(x = YR, y = Y), shape = 25, fill = "red") +
  #geom_point(data = data.frame(YR = paper_droughts, Y = rep(0.03, times = length(paper_droughts))), 
  #           aes(x = YR, y = Y), shape = 16, color = "grey") +
  ggtitle("MED Runoff drought / Area") +
  theme(axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7, angle = 45),
        plot.title = element_text(size = 12)) +
  scale_x_continuous(breaks = c(1766, 1800, 1850, 1900, 1950, 2000, 2015), 
                     labels = c('1766', '1800', '1850', '1900', '1950', '2000', '2015')) +
  scale_color_manual(values = tab_col_med2[order(tab_col_med2$YR)]$AREA_COL, guide = FALSE) +
  coord_cartesian(expand = FALSE)

point_med_q_g <- ggplot_gtable(ggplot_build(point_med_q))

############################ PRINT OUT #########################################

#mat <- matrix(list(ceu_s_g, ceu_q_g, med_s_g, med_q_g), nrow = 2)
#z <- matrix(c(3, 1, 2, 4), nrow = 2)
#grid.newpage()
#grid.draw(gtable::gtable_matrix("demo", mat, unit(c(1, 1), "null"), unit(c(1, 1), "null"), z = z))

#ggsave(filename = "area.pdf", device = "pdf", dpi = 300, units = "mm", width = 297,height = 210)

#mat <- matrix(list(point_ceu_s_g, ceu_s_g, point_ceu_q_g, ceu_q_g, point_med_s_g, med_s_g, point_med_q_g, med_q_g), nrow = 4)
#z <- matrix(c(2, 4, 1, 3, 6, 8, 5, 7), nrow = 4, byrow = T)
#grid.newpage()
#grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
#                                heights = unit(c(0.6, 2, 0.6, 2), "null"), z = z))


#mat <- matrix(list(ceu_s_g, ceu_q_g, med_s_g, med_q_g), nrow = 2)
#z <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = T)
##### GRAPH 1 #####
mat <- matrix(list(ceu_s_g, ceu_q_g), nrow = 2)
z <- matrix(c(1, 2), nrow = 2, byrow = T)

##### GRAPH 2 #####
mat <- matrix(list(med_s_g, med_q_g), nrow = 2)
z <- matrix(c(1, 2), nrow = 2, byrow = T)

grid.newpage()
#grid.draw(gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
 #                               heights = unit(c(1, 1), "null"), z = z))

#graphs <- gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
 #                               heights = unit(c(1, 1), "null"), z = z)

#fin <-  gtable::gtable_matrix(name = "demo", grobs = matrix(list(graphs, legend), nrow = 2), widths = unit(c(1), "null"), 
 #                             heights = unit(c(0.977, 0.023), "null"), z = matrix(c(1,2), nrow = 2))
#grid.draw(fin)

##### GRAPH 1 ######
graphs <- gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1), "null"), 
                                heights = unit(c(1, 1), "null"), z = z)

fin <-  gtable::gtable_matrix(name = "demo", grobs = matrix(list(graphs, legend), nrow = 2), widths = unit(c(1), "null"), 
                              heights = unit(c(0.977, 0.023), "null"), z = matrix(c(1,2), nrow = 2))
grid.draw(fin)
##### GRAPH 2 ######
graphs <- gtable::gtable_matrix(name = "demo", grobs = mat, widths = unit(c(1, 1), "null"), 
                                heights = unit(c(1, 1), "null"), z = z)

fin <-  gtable::gtable_matrix(name = "demo", grobs = matrix(list(graphs, legend), nrow = 2), widths = unit(c(1), "null"), 
                              heights = unit(c(0.977, 0.023), "null"), z = matrix(c(1,2), nrow = 2))
grid.draw(fin)
