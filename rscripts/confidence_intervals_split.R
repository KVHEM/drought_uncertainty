library(data.table); library(ggplot2)

outer_ribbon = c(0.05, 0.95); inner_ribbon = c(0.25, 0.75) # ribbon probs

dta <- readRDS('data/extremity_ens_1_EUR.rds') # data load
dta <- dta[, id := paste(MET, PAR, sep = '-')]
dta <- dta[(var != 'p') & (REG != 'EUR'),]

# head(dta)

q_seq <- seq(.1, .99, .01)

prbs <- sort(c(outer_ribbon, inner_ribbon))

aux1 <- dta[PAR==10, .(val = quantile(SEVERITY, q_seq, na.rm = T),
               q = q_seq), 
           by = .(REG, var, id)]

aux1 <- aux1[, .(val = quantile(val, c(.5, prbs)),
               name = c('ensamble_median', 'rib_1_min', 'rib_2_min', 'rib_2_max', 'rib_1_max')), 
           by = .(REG, var, q)]

aux2 <- dta[PAR==8, .(val = quantile(SEVERITY, q_seq, na.rm = T),
                q = q_seq), 
            by = .(REG, var, id)]

aux2 <- aux2[, .(val = quantile(val, c(.5, prbs)),
                name = c('ensamble_median', 'rib_1_min', 'rib_2_min', 'rib_2_max', 'rib_1_max')), 
            by = .(REG, var, q)]
# head(aux)
# class(aux)
res.conf.int1 <- dcast.data.table(aux1, REG + var + q ~ name, value.var = 'val')
res.conf.int2 <- dcast.data.table(aux2, REG + var + q ~ name, value.var = 'val')
res.conf.int1[,set:="10"]
res.conf.int2[,set:="8"]

res.conf.int <- rbind(res.conf.int1,res.conf.int2)

# head(res.conf.int)

ggplot(res.conf.int, aes(fill=set)) +
  geom_ribbon(aes(x = q, ymin = rib_1_min, ymax = rib_1_max), alpha = .4) +
  geom_ribbon(aes(x = q, ymin = rib_2_min, ymax = rib_2_max), alpha = .8) +
  geom_line(aes(x = q, y = ensamble_median), lwd = .5) + 
  theme_bw() +
  scale_fill_manual(values=c("darkorange","royalblue4")) +
  labs(fill="parameter set")+
  facet_wrap(REG ~ var, scales = 'free', ncol = 2) +
  labs(x = 'p', y = 'value', title = 'Confidence intervals')+
  theme(legend.position = "top")
ggsave("conf_plot_8_10_p.png")

ggplot(res.conf.int, aes(fill=set)) +
  geom_ribbon(aes(x = -log(-log(q)), ymin = rib_1_min, ymax = rib_1_max), alpha = .4) +
  geom_ribbon(aes(x = -log(-log(q)), ymin = rib_2_min, ymax = rib_2_max), alpha = .8) +
  geom_line(aes(x = -log(-log(q)), y = ensamble_median), lwd = .5) + 
  theme_bw() +
  scale_fill_manual(values=c("darkorange","royalblue4")) +
  labs(fill="parameter set")+
  facet_wrap(REG ~ var, scales = 'free', ncol = 2) +
  labs(x = 'Gumble variate', y = 'value', title = 'Confidence intervals')+
  theme(legend.position = "top")
ggsave("conf_plot_8_10_gumble.png")
