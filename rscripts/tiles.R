library(data.table)
library(ggplot2)
library(viridisLite)
library(viridis)

############################## PREPARE DATA ####################################

uncer_raw <- data.table(readRDS("D:/RGROUP/DATA/extremity_ens_1_EUR.rds"))
uncer_raw[var == "q" & REG == "MED" & PAR == 3 & yr == 1922, ]
uncer_raw[yr == 1921 & var == 's' & REG == 'CEU'][, range(AREA)]
uncer_raw[REG=='MED', AREA := AREA * 1672/550, by = .(REG, var)]
uncer_raw[REG=='CEU', AREA := AREA * 1672/1122, by = .(REG, var)]
uncer_raw[, AREA:=100*AREA]
uncer_raw[, REG:=factor(REG, levels = c('CEU', 'MED', 'EUR'))]
uncer_raw[, ID:= paste0(MET, "_", PAR)]
uncer_raw[, RANK_SEV:= rank(SEVERITY), by = .(REG, ID, var)]
uncer_raw[, RANK_AREA:= rank(AREA), by = .(REG, ID, var)]
colnames(uncer_raw) <- c("REG", "VAR", "YR", "SEVERITY", "AREA", "MET", "PAR", "ID", "RANK_SEV", "RANK_AREA") 

############################### PLOTS ##########################################

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

ggplot(uncer_raw[REG == "CEU" & VAR == "q" & RANK_AREA > 225,]) +
  geom_tile(aes(x = YR, y = ID, fill = RANK_AREA), colour = "white") +
  scale_fill_viridis(option = "D",direction = -1)
