library(data.table)
library(ggplot2)

uncer_raw <- data.table(readRDS("./data/extremity_ens_1_EUR.rds")) 

#correct area
uncer_raw[REG == 'MED', AREA := AREA * 1672 / 550, by = .(REG, var)]
uncer_raw[REG == 'CEU', AREA := AREA * 1672 / 1122, by = .(REG, var)]
uncer_raw[, AREA := 100 * AREA]
uncer_raw[, REG := factor(REG, levels = c('CEU', 'MED', 'EUR'))]

#estimate some stats
uncer <- uncer_raw 
colnames(uncer) <- c("reg", "var", "yr", "severity", "area", "met", "par") #change var names
uncer[, mean_area := mean(area, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_area := sd(area, na.rm = T), by = .(reg, var, yr)]
uncer[, range_area := diff(range(area)), by = .(reg, var, yr)]
uncer[, range_area_par := diff(range(area)), by = .(reg, var, yr, met)] 
uncer[, range_area_met := diff(range(area)), by = .(reg, var, yr, par)]
uncer[, mean_sev := mean(severity, na.rm = T), by = .(reg, var, yr)]
uncer[, sd_sev := sd(severity, na.rm = T), by = .(reg, var, yr)]
uncer[, range_sev := diff(range(severity)), by = .(reg, var, yr)]
uncer[, range_sev_par := diff(range(severity)), by = .(reg, var, yr, met)]
uncer[, range_sev_met := diff(range(severity)), by = .(reg, var, yr, par)]
uncer[, rank_area := rank(-area), .(par, met, var, reg)]
uncer[, rank_sev := rank(-severity), .(par, met, var, reg)]
uncer[yr >= 1900, aft_1900 := T]

uncer_met <- uncer[, .(severity = mean(severity, na.rm = TRUE), 
                       area = mean(area, na.rm = TRUE)), by = .(reg, met, var, yr)]
uncer_met_range <- uncer_met[var!='p', .(sev_range = diff(range(severity)), 
                                         area_range = diff(range(area))), by = .(reg, var, yr)]
uncer_met_range_rel <- uncer_met[var!='p', .(sev_range = diff(range(severity)) / mean(severity), 
                                             area_range = diff(range(area)) / mean(area)), by = .(reg, var, yr)]
uncer_met_range[reg == "EUR", .(area_mean = mean(area_range), sev_mean = mean(sev_range)), var]
uncer_met_range_rel[reg == "EUR", .(area_median = median(area_range), sev_median = median(sev_range)), var]

uncer_par <- uncer[, .(severity = mean(severity, na.rm = TRUE), 
                      area = mean(area, na.rm = TRUE)), by = .(reg, par, var, yr)]
uncer_par_range <- uncer_par[var!='p', .(sev_range = diff(range(severity)), 
                                        area_range = diff(range(area))), by = .(reg, var, yr)]
uncer_par_range_rel <- uncer_par[var!='p', .(sev_range = diff(range(severity)) / mean(severity), 
                                            area_range = diff(range(area)) / mean(area)), by = .(reg, var, yr)]
uncer_par_range_rel[reg == "EUR", .(area_median = median(area_range), sev_median = median(sev_range)), var]
uncer_par_range[reg == "EUR", .(area_mean = mean(area_range), sev_mean = mean(sev_range)), var]

uncer_met_range[reg == "EUR" & var == "q", .(range(sev_range), range(area_range))]
uncer_par_range[reg == "EUR" & var == "q", .(range(sev_range), range(area_range))]
uncer_met_range[reg == "EUR" & var == "q", .(mean(sev_range), mean(area_range))]

uncer_met_range[reg == "EUR" & var == "q", .(mean(sev_range), mean(area_range))]/
  uncer_par_range[reg == "EUR" & var == "q", .(mean(sev_range), mean(area_range))]

uncer_met_range[reg == "EUR" & var == "s", .(mean(sev_range), mean(area_range))]/
  uncer_par_range[reg == "EUR" & var == "s", .(mean(sev_range), mean(area_range))]
uncer_met_range[reg == "EUR" & var == "s", .(range(sev_range), range(area_range))]/
  uncer_par_range[reg == "EUR" & var == "s", .(range(sev_range), range(area_range))]

par_vs_met_area <- merge(uncer[aft_1900 == T, mean(range_area_par), .(var, reg)],  
                         uncer[aft_1900 == T, mean(range_area_met), .(var, reg)], by = c("var", "reg")) 
par_vs_met_area <- merge(par_vs_met_area, uncer[is.na(aft_1900), mean(range_area_par), .(var, reg)], 
                         by = c("var", "reg")) 
par_vs_met_area <- merge(par_vs_met_area, uncer[is.na(aft_1900), mean(range_area_met), .(var, reg)], 
                         by = c("var", "reg")) 
par_vs_met_area <- par_vs_met_area[c(7, 9, 4, 6)]
colnames(par_vs_met_area) = c("var", "reg", "par (aft 1900)", "met (aft 1900)", "par (bef 1900)", "met (bef 1900)")

par_vs_met_sev <- merge(uncer[aft_1900 == T, mean(range_sev_par, na.rm = T), .(var, reg)],  
                        uncer[aft_1900 == T, mean(range_sev_met, na.rm = T), .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- merge(par_vs_met_sev, uncer[is.na(aft_1900), mean(range_sev_par, na.rm = T), 
                                              .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- merge(par_vs_met_sev, uncer[is.na(aft_1900), mean(range_sev_met, na.rm = T), 
                                              .(var, reg)], by = c("var", "reg")) 
par_vs_met_sev <- par_vs_met_sev[c(7, 9, 4, 6)]
colnames(par_vs_met_sev) <- c("var", "reg", "par (aft 1900)", "met (aft 1900)", "par (bef 1900)", "met (bef 1900)")

rownames(par_vs_met_sev) <- c("par (aft 1900)", "met (aft 1900)", "par (bef 1900)", "met (bef 1900)")
colnames(par_vs_met_area) <- c("q MED", "q CEU", "s MED", "s CEU")

uncer_ceu_s <- uncer[reg == "CEU" & var == "s"]
uncer_ceu_q <- uncer[reg == "CEU" & var == "q"]
uncer_med_s <- uncer[reg == "MED" & var == "s"]
uncer_med_q <- uncer[reg == "MED" & var == "q"]

uncer_ceu_s_all <- uncer_ceu_s[unique(yr), .(yr, mean_area, sd_area, range_area, mean_sev, range_sev, aft_1900)] 
uncer_ceu_q_all <- uncer_ceu_q[unique(yr), .(yr, mean_area, sd_area, range_area, mean_sev, range_sev, aft_1900)] 
uncer_med_s_all <- uncer_med_s[unique(yr), .(yr, mean_area, sd_area, range_area, mean_sev, range_sev, aft_1900)] 
uncer_med_q_all <- uncer_med_q[unique(yr), .(yr, mean_area, sd_area,  range_area, mean_sev, range_sev, aft_1900)] 

area_rnk_ceu_s <- uncer_ceu_s[, mean(rank_area), yr]
area_rnk_ceu_q <- uncer_ceu_q[, mean(rank_area), yr]
area_rnk_med_s <- uncer_med_s[, mean(rank_area), yr]
area_rnk_med_q <- uncer_med_q[, mean(rank_area), yr]
sev_rnk_ceu_s <- uncer_ceu_s[, mean(rank_sev), yr]
sev_rnk_ceu_q <- uncer_ceu_q[, mean(rank_sev), yr]
sev_rnk_med_s <- uncer_med_s[, mean(rank_sev), yr]
sev_rnk_med_q <- uncer_med_q[, mean(rank_sev), yr]

ggplot(uncer_ceu_q_all[mean_area > 0.05], aes(x = yr, y = range_area / mean_area)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("Area Range/Mean Area") +
  theme_bw()
ggsave("uncer_ceu_q_all.png")

ggplot(uncer_ceu_s_all[mean_area > 0.05], aes(x = yr, y = range_area / mean_area)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("Area Range/Mean Area") +
  theme_bw()
ggsave("uncer_ceu_s_all.png")

ggplot(uncer_med_s_all[mean_area > 0.05], aes(x = yr, y = range_area / mean_area)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("Area Range/Mean Area") +
  theme_bw()

ggplot(uncer_med_q_all[mean_area > 0.05], aes(x = yr, y = range_area / mean_area)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("Area Range/Mean Area") +
  theme_bw()

ggplot(uncer_ceu_s_all, aes(x = mean_area, y = range_area, col = aft_1900)) + 
  geom_point() + 
  geom_smooth(se = F, span = 1) +
  xlab("Mean Area (soil moisture)") + 
  ylab("Area Range (soil moisture)") +
  theme_bw()+
  theme(legend.position="none") 
ggsave("uncer_ceu_s_all_area.png")

ggplot(uncer_ceu_q_all, aes(x = mean_area, y = range_area, col = aft_1900)) + 
  geom_point() + 
  geom_smooth(se = F, span = 1) +
  xlab("Mean Area (runoff)") + 
  ylab("Area Range (runoff)") +
  theme_bw()+
  theme(legend.position="none") 
ggsave("uncer_ceu_q_all_area.png")

ggplot(uncer_ceu_s_all, aes(x = mean_sev, y = range_sev, col = aft_1900)) + 
  geom_point() + 
  geom_smooth(se = F, span = 1) +
  xlab("Mean Severity (soil moisture)") + 
  ylab("Severity Range (soil moisture)") +
  theme_bw()+
  theme(legend.position="none") 
ggsave("uncer_ceu_s_all_sev.png")

ggplot(uncer_ceu_q_all, aes(x = mean_sev, y = range_sev, col = aft_1900)) + 
  geom_point() + 
  geom_smooth(se = F, span = 1) +
  xlab("Mean Severity (runoff)") + 
  ylab("Severity Range (runoff)") +
  theme_bw()+
  theme(legend.position="none") 
ggsave("uncer_ceu_q_all_sev.png")

