library(data.table)
library(ggplot2)

uncer_raw <- data.table(readRDS("data/extremity_ens_1_EUR.rds")) 

#correct area
uncer_raw[REG == 'MED', AREA := AREA * 1672 / 550, by = .(REG, var)]
uncer_raw[REG == 'CEU', AREA := AREA * 1672 / 1122, by = .(REG, var)]
uncer_raw[, AREA := 100 * AREA]
uncer_raw[, REG := factor(REG, levels = c('CEU', 'MED', 'EUR'))]

#estimate some stats
uncer <- uncer_raw 

colnames(uncer) <- c("reg", "var", "yr", "severity", "area", "met", "par") #change var names
uncer[area==0, severity:=0]

uncer[, mean_area := mean(area, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_area := sd(area, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_area := sd(area), by = .(reg, var, yr)]
uncer[, sd_area_par := sd(area), by = .(reg, var, yr, met)] 
uncer[, sd_area_met := sd(area), by = .(reg, var, yr, par)]
uncer[, mean_sev := mean(severity, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_sev := sd(severity, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_sev := sd(severity), by = .(reg, var, yr)]
uncer[, sd_sev_par := sd(severity), by = .(reg, var, yr, met)]
uncer[, sd_sev_met := sd(severity), by = .(reg, var, yr, par)]
uncer[, rank_area := rank(-area), .(par, met, var, reg)]
uncer[, rank_sev := rank(-severity), .(par, met, var, reg)]
uncer[, aft_1900 := yr >= 1900]
uncer <- uncer[(var != 'p') & (reg != 'EUR'),]

uncer_met <- uncer[, .(severity_sd = sd(severity, na.rm = TRUE), 
                       area_sd = sd(area, na.rm = TRUE), severity = mean(severity, na.rm = TRUE), 
                       area = mean(area, na.rm = TRUE)), by = .(reg, par, var, yr)]

uncer_par <- uncer[, .(severity_sd = sd(severity, na.rm = TRUE), 
                       area_sd = sd(area, na.rm = TRUE),
                       severity = mean(severity, na.rm = TRUE), 
                       area = mean(area, na.rm = TRUE)
                       ), by = .(reg, met, var, yr)]
# rank of the standard deviation
uncer_met[,rank_sev_sd := rank(severity_sd),by=.(var,reg, yr)]
uncer_met[,rank_area_sd := rank(area_sd),by=.(var,reg, yr)]
uncer_par[,rank_sev_sd := rank(severity_sd),by=.(var,reg, yr)]
uncer_par[,rank_area_sd := rank(area_sd),by=.(var,reg, yr)]
uncer_met[,av_rank_sev_sd := mean(rank_sev_sd),by=.(var,reg,par)]
uncer_met[,av_rank_area_sd := mean(rank_area_sd),by=.(var,reg,par)]
uncer_par[,av_rank_sev_sd := mean(rank_sev_sd),by=.(var,reg,met)]
uncer_par[,av_rank_area_sd := mean(rank_area_sd),by=.(var,reg,met)]

# average rank before and after 1900
uncer_met[yr >= 1900,av_rank_sev_sd2 := mean(rank_sev_sd),by=.(var,reg,par)]
uncer_met[yr >= 1900,av_rank_area_sd2 := mean(rank_area_sd),by=.(var,reg,par)]
uncer_par[yr >= 1900,av_rank_sev_sd2 := mean(rank_sev_sd),by=.(var,reg,met)]
uncer_par[yr >= 1900,av_rank_area_sd2 := mean(rank_area_sd),by=.(var,reg,met)]
uncer_met[yr < 1900,av_rank_sev_sd2 := mean(rank_sev_sd),by=.(var,reg,par)]
uncer_met[yr < 1900,av_rank_area_sd2 := mean(rank_area_sd),by=.(var,reg,par)]
uncer_par[yr < 1900,av_rank_sev_sd2 := mean(rank_sev_sd),by=.(var,reg,met)]
uncer_par[yr < 1900,av_rank_area_sd2 := mean(rank_area_sd),by=.(var,reg,met)]

# standard deviation of the ranks
uncer_met[,sd_rank_sev_sd := sd(rank_sev_sd),by=.(var,reg,par)]
uncer_met[,sd_rank_area_sd := sd(rank_area_sd),by=.(var,reg,par)]
uncer_par[,sd_rank_sev_sd := sd(rank_sev_sd),by=.(var,reg,met)]
uncer_par[,sd_rank_area_sd := sd(rank_area_sd),by=.(var,reg,met)]
uncer_met[yr >= 1900,sd_rank_sev_sd2 := sd(rank_sev_sd),by=.(var,reg,par)]
uncer_met[yr >= 1900,sd_rank_area_sd2 := sd(rank_area_sd),by=.(var,reg,par)]
uncer_par[yr >= 1900,sd_rank_sev_sd2 := sd(rank_sev_sd),by=.(var,reg,met)]
uncer_par[yr >= 1900,sd_rank_area_sd2 := sd(rank_area_sd),by=.(var,reg,met)]
uncer_met[yr < 1900,sd_rank_sev_sd2 := sd(rank_sev_sd),by=.(var,reg,par)]
uncer_met[yr < 1900,sd_rank_area_sd2 := sd(rank_area_sd),by=.(var,reg,par)]
uncer_par[yr < 1900,sd_rank_sev_sd2 := sd(rank_sev_sd),by=.(var,reg,met)]
uncer_par[yr < 1900,sd_rank_area_sd2 := sd(rank_area_sd),by=.(var,reg,met)]

uncer_par[, aft_1900 := yr >= 1900]
uncer_met[, aft_1900 := yr >= 1900]
# calculating sd for each met and par set
uncer_met_sd <- uncer_met[var!='p', .(sev_sd = sd(severity), 
                                         area_sd = sd(area)), by = .(reg, var, yr)]
uncer_par_sd <- uncer_par[var!='p', .(sev_sd = sd(severity), 
                                      area_sd = sd(area)), by = .(reg, var, yr)]

# mean sd for area and severity 
uncer_met_sd[reg == "EUR", .(area_mean = mean(area_sd), sev_mean = mean(sev_sd)), var]
uncer_par_sd[reg == "EUR", .(area_mean = mean(area_sd), sev_mean = mean(sev_sd)), var]

# looksies at the data
uncer_met_sd[reg == "EUR" & var == "q", .(sd(sev_sd), sd(area_sd))]
uncer_par_sd[reg == "EUR" & var == "q", .(sd(sev_sd), sd(area_sd))]
uncer_met_sd[reg == "EUR" & var == "q", .(mean(sev_sd), mean(area_sd))]
uncer_par_sd[reg == "EUR" & var == "q", .(mean(sev_sd), mean(area_sd))]
uncer_met_sd[reg == "EUR" & var == "s", .(mean(sev_sd), mean(area_sd))]
uncer_par_sd[reg == "EUR" & var == "s", .(mean(sev_sd), mean(area_sd))]
uncer_met_sd[reg == "EUR" & var == "s", .(sd(sev_sd), sd(area_sd))]
uncer_par_sd[reg == "EUR" & var == "s", .(sd(sev_sd), sd(area_sd))]

# compare quickly
par_vs_met_area <- merge(uncer[aft_1900 == T, mean(sd_area_par), .(var, reg)],  
                         uncer[aft_1900 == T, mean(sd_area_met), .(var, reg)], by = c("var", "reg")) 
par_vs_met_area <- merge(par_vs_met_area, uncer[aft_1900 == F, mean(sd_area_par), .(var, reg)], 
                         by = c("var", "reg"))
par_vs_met_area <- merge(par_vs_met_area, uncer[aft_1900 == F, mean(sd_area_met), .(var, reg)], 
                         by = c("var", "reg"))
par_vs_met_area <- par_vs_met_area[c(7, 9, 4, 6)]

colnames(par_vs_met_area) = c("var", "reg", "par (aft 1900)", "met (aft 1900)", "par (bef 1900)", "met (bef 1900)")

par_vs_met_sev <- merge(uncer[aft_1900 == T, mean(sd_sev_par, na.rm = T), .(var, reg)],  
                        uncer[aft_1900 == T, mean(sd_sev_met, na.rm = T), .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- merge(par_vs_met_sev, uncer[aft_1900 == F, mean(sd_sev_par, na.rm = T), 
                                              .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- merge(par_vs_met_sev, uncer[aft_1900 == F, mean(sd_sev_met, na.rm = T), 
                                              .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- par_vs_met_sev[c(7, 9, 4, 6)]
colnames(par_vs_met_sev) <- c("var", "reg", "par (aft 1900)", "met (aft 1900)", "par (bef 1900)", "met (bef 1900)")

# filter to region and var
uncer_ceu_s <- uncer[reg == "CEU" & var == "s"]
uncer_ceu_q <- uncer[reg == "CEU" & var == "q"]
uncer_med_s <- uncer[reg == "MED" & var == "s"]
uncer_med_q <- uncer[reg == "MED" & var == "q"]

uncer_ceu_s_all <- uncer_ceu_s[unique(yr), .(yr, mean_area, sd_area, mean_sev, sd_sev, aft_1900)] 
uncer_ceu_q_all <- uncer_ceu_q[unique(yr), .(yr, mean_area, sd_area, mean_sev, sd_sev, aft_1900)] 
uncer_med_s_all <- uncer_med_s[unique(yr), .(yr, mean_area, sd_area, mean_sev, sd_sev, aft_1900)] 
uncer_med_q_all <- uncer_med_q[unique(yr), .(yr, mean_area, sd_area, mean_sev, sd_sev, aft_1900)] 


# # plots
# ggplot(uncer_ceu_q_all[mean_area > 0.05], aes(x = yr, y = sd_area / mean_area)) + 
#   geom_point() + 
#   xlab("Year") + 
#   ylab("Area sd/Mean Area") +
#   theme_bw()
# ggsave("uncer_ceu_q_sd_all.png")
# 
# ggplot(uncer_ceu_s_all[mean_area > 0.05], aes(x = yr, y = sd_area / mean_area)) + 
#   geom_point() + 
#   xlab("Year") + 
#   ylab("Area sd/Mean Area") +
#   theme_bw()
# ggsave("uncer_ceu_s_sd_all.png")
# 
# ggplot(uncer_med_s_all[mean_area > 0.05], aes(x = yr, y = sd_area / mean_area)) + 
#   geom_point() + 
#   xlab("Year") + 
#   ylab("Area sd/Mean Area") +
#   theme_bw()
# 
# ggplot(uncer_med_q_all[mean_area > 0.05], aes(x = yr, y = sd_area / mean_area)) + 
#   geom_point() + 
#   xlab("Year") + 
#   ylab("Area sd/Mean Area") +
#   theme_bw()
# 
# ggplot(uncer_ceu_s_all, aes(x = mean_area, y = sd_area, col = aft_1900)) + 
#   geom_point() + 
#   geom_smooth(se = F, span = 1) +
#   xlab("Mean Area (soil moisture)") + 
#   ylab("Area sd (soil moisture)") +
#   theme_bw()+
#   theme(legend.position="none") 
# ggsave("uncer_ceu_s_sd_all_area.png")
# 
# ggplot(uncer_ceu_q_all, aes(x = mean_area, y = sd_area, col = aft_1900)) +
#   geom_point() +
#   geom_smooth(se = F, span = 1) +
#   xlab("Mean Area (runoff)") +
#   ylab("Area sd (runoff)") +
#   theme_bw()+
#   theme(legend.position="none")
# 
# ggsave("uncer_ceu_q_sd_all_area.png")
#
# ggplot(uncer_ceu_s_all, aes(x = mean_sev, y = sd_sev, col = aft_1900)) + 
#   geom_point() + 
#   geom_smooth(se = F, span = 1) +
#   xlab("Mean Severity (soil moisture)") + 
#   ylab("Severity sd (soil moisture)") +
#   theme_bw()+
#   theme(legend.position="none") 
# ggsave("uncer_ceu_s_sd_all_sev.png")
# 
# ggplot(uncer_ceu_q_all, aes(x = mean_sev, y = sd_sev, col = aft_1900)) + 
#   geom_point() + 
#   geom_smooth(se = F, span = 1) +
#   xlab("Mean Severity (runoff)") + 
#   ylab("Severity sd (runoff)") +
#   theme_bw()+
#   theme(legend.position="none") 
# ggsave("uncer_ceu_q_sd_all_sev.png")
# 
