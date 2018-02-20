library(data.table)
library(ggplot2)
library(viridisLite)
library(viridis)
library(grid)
library(gridExtra)
library(ggpubr)
library(cowplot)

############################## PREPARE DATA ####################################

uncer_raw <- data.table(readRDS("D:/RGROUP/DATA/extremity_ens_1_EUR.rds"))
uncer_raw[REG=='MED', AREA := AREA * 1672/550, by = .(REG, var)]
uncer_raw[REG=='CEU', AREA := AREA * 1672/1122, by = .(REG, var)]
uncer_raw[, AREA:=100*AREA]
uncer_raw[, REG:=factor(REG, levels = c('CEU', 'MED', 'EUR'))]
uncer_raw[, ID:= paste0(MET, "_", PAR)]
uncer_raw[, RANK_SEV:= rank(SEVERITY), by = .(REG, ID, var)]
uncer_raw[, RANK_AREA:= rank(AREA), by = .(REG, ID, var)]
colnames(uncer_raw) <- c("REG", "VAR", "YR", "SEVERITY", "AREA", "MET", "PAR", "ID", "RANK_SEV", "RANK_AREA") 

######################### TILES AREA vs YEARS wo NOISE #########################

####### CEU Soil drought / Area ######
uncer_noise_sev <- uncer_raw[RANK_SEV > 225,]
uncer_noise_sev[,NOISE:= .N, by = .(YR, REG, VAR)]

################ RAW #####
ggplot(uncer_noise_sev[REG == "CEU" & VAR == "s" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 3) +
  scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  ggtitle("CEU Soil drought / Severity") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black", size = 5))
################ RAW #####

ceu_s <- ggplot(uncer_noise_sev[REG == "CEU" & VAR == "s" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 3) +
  scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  ggtitle("CEU Soil drought / Severity") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "none", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        strip.text.x = element_text(size = 7))

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

stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_ceu <- uncer_raw[REG == "CEU" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_ceu[RANK_SEV <= 225 ,SEV_COL:= "#EBEBEB"]
tab_col_ceu[RANK_SEV > 225 & RANK_SEV <= 230 ,SEV_COL:= "#440053"]
tab_col_ceu[RANK_SEV > 230 & RANK_SEV <= 235 ,SEV_COL:= "#3C458A"]
tab_col_ceu[RANK_SEV > 235 & RANK_SEV <= 240 ,SEV_COL:= "#26908C"]
tab_col_ceu[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
tab_col_ceu[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_ceu[RANK_AREA <= 225, AREA_COL:= "#EBEBEB"]
tab_col_ceu[RANK_AREA > 225 & RANK_AREA <= 230 ,AREA_COL:= "#440053"]
tab_col_ceu[RANK_AREA > 230 & RANK_AREA <= 235 ,AREA_COL:= "#3C458A"]
tab_col_ceu[RANK_AREA > 235 & RANK_AREA <= 240 ,AREA_COL:= "#26908C"]
tab_col_ceu[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
tab_col_ceu[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_ceu2 <- merge(x = tab_col_ceu, y = yr_vec, by.x = "YR", by.y = "YR")
tab_col_ceu3 <- tab_col_ceu2[order(tab_col_ceu2$ORD)]

fills_ceu <- tab_col_ceu3$SEV_COL

k <- 1
for (i in stript) {
  j <- which(grepl('rect', ceu_s_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  ceu_s_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_ceu[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(ceu_s_g)

############ MED Soil drought / Area ##########

med_s <- ggplot(uncer_noise_sev[REG == "MED" & VAR == "s" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 3) +
  scale_fill_viridis("RANK", option = "D", direction = 1, discrete = T) +
  ggtitle("MED Soil drought / Severity") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "none", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        strip.text.x = element_text(size = 7))

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

stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_med <- uncer_raw[REG == "MED" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_med[RANK_SEV <= 225 ,SEV_COL:= "#EBEBEB"]
tab_col_med[RANK_SEV > 225 & RANK_SEV <= 230 ,SEV_COL:= "#440053"]
tab_col_med[RANK_SEV > 230 & RANK_SEV <= 235 ,SEV_COL:= "#3C458A"]
tab_col_med[RANK_SEV > 235 & RANK_SEV <= 240 ,SEV_COL:= "#26908C"]
tab_col_med[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
tab_col_med[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_med[RANK_AREA <= 225, AREA_COL:= "#EBEBEB"]
tab_col_med[RANK_AREA > 225 & RANK_AREA <= 230 ,AREA_COL:= "#440053"]
tab_col_med[RANK_AREA > 230 & RANK_AREA <= 235 ,AREA_COL:= "#3C458A"]
tab_col_med[RANK_AREA > 235 & RANK_AREA <= 240 ,AREA_COL:= "#26908C"]
tab_col_med[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
tab_col_med[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_med2 <- merge(x = tab_col_med, y = yr_vec, by.x = "YR", by.y = "YR")
tab_col_med3 <- tab_col_med2[order(tab_col_med2$ORD)]

fills_med <- tab_col_med3$SEV_COL

k <- 1
for (i in stript) {
  j <- which(grepl('rect', med_s_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  med_s_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_med[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(med_s_g)

##################### CEU Discharge drought / Area #############################

ceu_q <- ggplot(uncer_noise_sev[REG == "CEU" & VAR == "q" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 3) +
  scale_fill_viridis("RANK", option = "D",direction = 1, discrete = T) +
  ggtitle("CEU Discharge drought / Severity") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "none", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        strip.text.x = element_text(size = 7))

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

stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_ceu <- uncer_raw[REG == "CEU" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_ceu[RANK_SEV <= 225 ,SEV_COL:= "#EBEBEB"]
tab_col_ceu[RANK_SEV > 225 & RANK_SEV <= 230 ,SEV_COL:= "#440053"]
tab_col_ceu[RANK_SEV > 230 & RANK_SEV <= 235 ,SEV_COL:= "#3C458A"]
tab_col_ceu[RANK_SEV > 235 & RANK_SEV <= 240 ,SEV_COL:= "#26908C"]
tab_col_ceu[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
tab_col_ceu[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_ceu[RANK_AREA <= 225, AREA_COL:= "#EBEBEB"]
tab_col_ceu[RANK_AREA > 225 & RANK_AREA <= 230 ,AREA_COL:= "#440053"]
tab_col_ceu[RANK_AREA > 230 & RANK_AREA <= 235 ,AREA_COL:= "#3C458A"]
tab_col_ceu[RANK_AREA > 235 & RANK_AREA <= 240 ,AREA_COL:= "#26908C"]
tab_col_ceu[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
tab_col_ceu[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_ceu2 <- merge(x = tab_col_ceu, y = yr_vec, by.x = "YR", by.y = "YR")
tab_col_ceu3 <- tab_col_ceu2[order(tab_col_ceu2$ORD)]

fills_ceu <- tab_col_ceu3$SEV_COL

k <- 1
for (i in stript) {
  j <- which(grepl('rect', ceu_q_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  ceu_q_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_ceu[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(ceu_q_g)

######################### MED Discharge drought / Area #########################

med_q <- ggplot(uncer_noise_sev[REG == "MED" & VAR == "q" & NOISE > 10,]) +
  geom_tile(aes(x = PAR, y = MET, fill = cut(RANK_SEV, breaks = c(225, 230, 235, 240, 245, 250))), colour = "white") +
  scale_x_continuous(breaks = c(1,5,10)) +
  scale_y_continuous(breaks = c(1,5,10)) +
  facet_wrap(~YR, nrow = 3) +
  scale_fill_viridis("RANK",option = "D",direction = 1, discrete = T) +
  ggtitle("MED Discharge drought / Severity") +
  theme(strip.text = element_text(colour = '#ED8810'),
        legend.position = "none", 
        axis.title = element_text(size = 7),
        axis.text = element_text(size = 6),
        strip.text.x = element_text(size = 7))

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

stript <- stript[!is.na(yr_vec)]
yr_vec <- yr_vec[complete.cases(yr_vec)]
yr_vec <- data.frame(YR = as.numeric(yr_vec))
yr_vec$ORD <- 1:nrow(yr_vec)

tab_col_med <- uncer_raw[REG == "MED" & VAR == "p" & YR %in% yr_vec$YR,]
tab_col_med[RANK_SEV <= 225 ,SEV_COL:= "#EBEBEB"]
tab_col_med[RANK_SEV > 225 & RANK_SEV <= 230 ,SEV_COL:= "#440053"]
tab_col_med[RANK_SEV > 230 & RANK_SEV <= 235 ,SEV_COL:= "#3C458A"]
tab_col_med[RANK_SEV > 235 & RANK_SEV <= 240 ,SEV_COL:= "#26908C"]
tab_col_med[RANK_SEV > 240 & RANK_SEV <= 245 ,SEV_COL:= "#5FD166"]
tab_col_med[RANK_SEV > 245 & RANK_SEV <= 250 ,SEV_COL:= "#FCF534"]

tab_col_med[RANK_AREA <= 225, AREA_COL:= "#EBEBEB"]
tab_col_med[RANK_AREA > 225 & RANK_AREA <= 230 ,AREA_COL:= "#440053"]
tab_col_med[RANK_AREA > 230 & RANK_AREA <= 235 ,AREA_COL:= "#3C458A"]
tab_col_med[RANK_AREA > 235 & RANK_AREA <= 240 ,AREA_COL:= "#26908C"]
tab_col_med[RANK_AREA > 240 & RANK_AREA <= 245 ,AREA_COL:= "#5FD166"]
tab_col_med[RANK_AREA > 245 & RANK_AREA <= 250 ,AREA_COL:= "#FCF534"]

tab_col_med2 <- merge(x = tab_col_med, y = yr_vec, by.x = "YR", by.y = "YR")
tab_col_med3 <- tab_col_med2[order(tab_col_med2$ORD)]

fills_med <- tab_col_med3$SEV_COL

k <- 1
for (i in stript) {
  j <- which(grepl('rect', med_q_g$grobs[[i]]$grobs[[1]]$childrenOrder))
  med_q_g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills_med[k]
  k <- k+1
}

#grid.newpage()
#grid.draw(med_q_g)

################################################################################

mat <- matrix(list(ceu_s_g, ceu_q_g, med_s_g, med_q_g), nrow = 2)
z <- matrix(c(3, 1, 2, 4), nrow = 2)
grid.newpage()
grid.draw(gtable::gtable_matrix("demo", mat, unit(c(1, 1), "null"), unit(c(1, 1), "null"), z = z))

#ggsave(filename = "area.pdf", device = "pdf", dpi = 300, units = "mm", width = 297,height = 210)

