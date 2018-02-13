library(data.table); library(ggplot2)

outer_ribbon = c(0.05, 0.95); inner_ribbon = c(0.25, 0.75) # ribbon probs

dta <- readRDS('~/Desktop/uncer/extremity_ens_1_EUR.rds') # data load
dta <- dta[, id := paste(MET, PAR, sep = '-')]
dta <- dta[(var != 'p') & (REG != 'EUR'),]

# head(dta)

q_seq <- seq(.1, .99, .01)

prbs <- sort(c(outer_ribbon, inner_ribbon))

aux <- dta[, .(val = quantile(SEVERITY, q_seq, na.rm = T),
               q = q_seq), 
           by = .(REG, var, id)]

aux <- aux[, .(val = quantile(val, c(.5, prbs)),
               name = c('ensamble_median', 'rib_1_min', 'rib_2_min', 'rib_2_max', 'rib_1_max')), 
           by = .(REG, var, q)]
# head(aux)
# class(aux)

res.conf.int <- dcast.data.table(aux, REG + var + q ~ name, value.var = 'val')
# head(res.conf.int)

ggplot(res.conf.int) +
  geom_ribbon(aes(x = q, ymin = rib_1_min, ymax = rib_1_max), fill = 'steelblue4', alpha = .4) +
  geom_ribbon(aes(x = q, ymin = rib_2_min, ymax = rib_2_max), fill = 'steelblue4', alpha = .8) +
  geom_line(aes(x = q, y = ensamble_median), col = 'red4', lwd = .5) + 
  theme_bw() +
  facet_wrap(REG ~ var, scales = 'free', ncol = 2) +
  labs(x = 'p', y = 'value', title = 'Confidence intervals')
