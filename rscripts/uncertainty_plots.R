# Uncertainty poster
source("rscripts/Uncertainty_sd.R")

library("RColorBrewer")

# plot coeff of var against mean area
cv_area_gg <- function(data){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(any(datain=="s")){
    xlabs <- "Mean Area (soil moisture)"
    ylabs <-"CV (soil moisture)"
  }
  else
  {
    xlabs <- "Mean Area (runoff)"
    ylabs <-"CV (runoff)"
  }
  save_name <- paste(paste(datain[1:3],collapse="_"),"_cv_area.png",sep="")
  print(save_name)
  ggplot(data, aes(x = mean_area, y = sd_area/mean_area, col = aft_1900)) +
    geom_point() +
    geom_smooth(se = F, span = 1) +
    labs(x=xlabs, y=ylabs, color="") +
    ylab(ylabs) +
    theme_bw()+
    scale_color_manual(labels = c("Before 1900","After 1900"),values = c("darkorange","royalblue4"))+
    theme(legend.position="top")
  
  ggsave(save_name)
}

cv_area_gg(uncer_ceu_s_all)
cv_area_gg(uncer_ceu_q_all)
cv_area_gg(uncer_med_s_all)
cv_area_gg(uncer_med_q_all)

# plot coeff of var against mean area
cv_sev_gg <- function(data){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(any(datain=="s")){
    xlabs <- "Mean Sev (soil moisture)"
    ylabs <-"CV (soil moisture)"
  }
  else
  {
    xlabs <- "Mean Sev (runoff)"
    ylabs <-"CV (runoff)"
  }
  save_name <- paste(paste(datain[1:3],collapse="_"),"_cv_sev.png",sep="")
  print(save_name)
  ggplot(data, aes(x = mean_sev, y = sd_sev/mean_sev, col = aft_1900)) +
    geom_point() +
    geom_smooth(se = F, span = 1) +
    labs(x=xlabs, y=ylabs, color="") +
    ylab(ylabs) +
    theme_bw()+
    scale_color_manual(labels = c("Before 1900","After 1900"), values = c("darkorange","royalblue4"))+
    theme(legend.position="top")
  
  ggsave(save_name)
}

cv_sev_gg(uncer_ceu_s_all)
cv_sev_gg(uncer_ceu_q_all)
cv_sev_gg(uncer_med_s_all)
cv_sev_gg(uncer_med_q_all)


# plots sd for each parameter set over time
my_cols <- colorRampPalette(c("darkred","darkorange","goldenrod","skyblue","royalblue4"))
# cols_val <- gray(seq(0.2, 0.9, length.out = 10))

sd_parts_gg_area <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: standard deviation (area)"
  }
  else
  {
    ylabs <-"runoff: standard deviation (area)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var,reg,"area.png",collapse="_")
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = area_sd)) +
    geom_line(aes(color=my_fac)) +
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
sd_parts_gg_sev <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: standard deviation (severity)"
  }
  else{
    ylabs <-"runoff: standard deviation (severity)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"sd",var,reg,"sev.png",collapse="_")
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
         
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = severity_sd)) +
    geom_line(aes(color=my_fac))+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

sd_parts_gg_area(uncer_met, reg = "CEU", var= "q")
sd_parts_gg_area(uncer_met, reg = "CEU", var= "s")
sd_parts_gg_area(uncer_met, reg = "MED", var= "q")
sd_parts_gg_area(uncer_met, reg = "MED", var= "s")
sd_parts_gg_sev(uncer_met, reg = "CEU", var= "q")
sd_parts_gg_sev(uncer_met, reg = "CEU", var= "s")
sd_parts_gg_sev(uncer_met, reg = "MED", var= "q")
sd_parts_gg_sev(uncer_met, reg = "MED", var= "s")
sd_parts_gg_area(uncer_par, reg = "CEU", var= "q")
sd_parts_gg_area(uncer_par, reg = "CEU", var= "s")
sd_parts_gg_area(uncer_par, reg = "MED", var= "q")
sd_parts_gg_area(uncer_par, reg = "MED", var= "s")
sd_parts_gg_sev(uncer_par, reg = "CEU", var= "q")
sd_parts_gg_sev(uncer_par, reg = "CEU", var= "s")
sd_parts_gg_sev(uncer_par, reg = "MED", var= "q")
sd_parts_gg_sev(uncer_par, reg = "MED", var= "s")


cv_parts_gg_area <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: cv (area)"
  }
  else
  {
    ylabs <-"runoff: cv (area)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var,reg,"area.png",collapse="_")
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = area_sd/area)) +
    geom_line(aes(color=my_fac)) +
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}
cv_parts_gg_sev <- function(data, reg, var){
  datain <- unlist(strsplit(deparse(substitute(data)),split="_"))
  if(var=="s"){
    ylabs <-"soil moisture: cv (severity)"
  }
  else{
    ylabs <-"runoff: cv (severity)"
  }
  if(any(datain=="met")){
    leg.title <-"meteorological set id #"
    sw <-"met"
  }
  else{
    leg.title <-"parameter set id #"
    sw <-"par"
  }
  save_name <- paste(paste(datain[1:2],collapse="_"),"cv",var,reg,"sev.png",collapse="_")
  if(sw == "met"){
    my_fac =factor(data$met)
  }else{
    my_fac =factor(data$par)
  }
  
  ggplot(data[reg==reg& var==var,], aes(x = yr, y = severity_sd/severity)) +
    geom_line(aes(color=my_fac))+
    labs(x="Year", y=ylabs, color=leg.title,title=reg) +
    theme_bw()+
    scale_color_manual(labels = as.character(1:10), values = my_cols(10))+
    theme(legend.position="top")
  ggsave(save_name)
}

cv_parts_gg_area(uncer_met, reg = "CEU", var= "q")
cv_parts_gg_area(uncer_met, reg = "CEU", var= "s")
cv_parts_gg_area(uncer_met, reg = "MED", var= "q")
cv_parts_gg_area(uncer_met, reg = "MED", var= "s")
cv_parts_gg_sev(uncer_met, reg = "CEU", var= "q")
cv_parts_gg_sev(uncer_met, reg = "CEU", var= "s")
cv_parts_gg_sev(uncer_met, reg = "MED", var= "q")
cv_parts_gg_sev(uncer_met, reg = "MED", var= "s")
cv_parts_gg_area(uncer_par, reg = "CEU", var= "q")
cv_parts_gg_area(uncer_par, reg = "CEU", var= "s")
cv_parts_gg_area(uncer_par, reg = "MED", var= "q")
cv_parts_gg_area(uncer_par, reg = "MED", var= "s")
cv_parts_gg_sev(uncer_par, reg = "CEU", var= "q")
cv_parts_gg_sev(uncer_par, reg = "CEU", var= "s")
cv_parts_gg_sev(uncer_par, reg = "MED", var= "q")
cv_parts_gg_sev(uncer_par, reg = "MED", var= "s")

# elegant tiled heat map

library(reshape2) #melt(), dcast() for data reformatting
library(plyr) #ddply() for data reformatting

ggplot(uncer_met[reg == "MED",], aes(x = yr, y = met, col = rank_sev_sd)) +
  geom_tile() +
  scale_y_continuous(breaks = 1:10) +
  labs(x="Year", y="meteorological set", color="rank sd(severity) ") +
  theme_bw()+
  theme(legend.position="right",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
ggsave('ugly_tiles_med_rank_sev.png')

ggplot(uncer_met[reg == "CEU",], aes(x = yr, y = met, col = rank_sev_sd)) +
  geom_tile() +
  scale_y_continuous(breaks = 1:10) +
  labs(x="Year", y="meteorological set", color="rank sd(severity) ") +
  theme_bw()+
  theme(legend.position="right",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
ggsave('ugly_tiles_ceu_rank_sev.png')

ggplot(uncer_met[reg == "MED",], aes(x = yr, y = met, col = rank_area_sd)) +
  geom_tile() +
  scale_y_continuous(breaks = 1:10) +
  labs(x="Year", y="meteorological set", color="rank sd(severity) ") +
  theme_bw()+
  theme(legend.position="right",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
ggsave('ugly_tiles_med_rank_area.png')

ggplot(uncer_met[reg == "CEU",], aes(x = yr, y = met, col = rank_area_sd)) +
  geom_tile() +
  scale_y_continuous(breaks = 1:10) +
  labs(x="Year", y="meteorological set", color="rank sd(severity) ") +
  theme_bw()+
  theme(legend.position="right",
        #remove plot background
        plot.background=element_blank(),
        #remove plot border
        panel.border=element_blank())
ggsave('ugly_tiles_ceu_rank_area.png')
