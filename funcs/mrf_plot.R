library(gamlss.data); library(tidyverse); library(mgcv)

# prep data
rent99 = rent99 %>%
  arrange(district) %>%
  mutate(district=factor(district))

rent99.polys[!names(rent99.polys) %in% levels(rent99$district)] = NULL
rent99.polys = rent99.polys[order(as.numeric(names(rent99.polys)))]

# run mrf and re models
gam_rent_mrf_forplot = gam(rent ~ s(district, bs = 'mrf', xt = list(polys=rent99.polys)),
                   data = rent99,
                   method = 'REML')
gam_rent_re_forplot = gam(rent ~ s(district, bs = 're'),
                  data = rent99,
                  method = 'REML')

# debugonce(plot.gam)
# plot(gam_rent_mrf_forplot)

load('data/test_gam_plot_pd_plot_stage.rda')

# x2 = x
# x2$smooth[[i]]$S.scale = 1  # from 22, doesn't change anything
# x2$smooth[[i]]$S = gam_rent_re_forplot$smooth[[1]]$S  # doesn't change anything
pd2 = pd
pd2[[i]]$fit = coef(gam_rent_re_forplot)[-1]
names(pd2[[i]]$fit) = NULL  # otherwise error on mismatch
pd2[[i]]$main = 's(district, 139.77)'  # or whatever it is

par(mfrow=c(1,2))
plot(gam_rent_mrf_forplot)
mgcv:::plot.mrf.smooth(x$smooth[[i]], P = pd2[[i]], partial.resids = partial.resids,
                       rug = rug, se = se, scale = scale, n = n, n2 = n2,
                       pers = pers, theta = theta, phi = phi, jit = jit,
                       xlab = xlab, ylab = ylab, main = main, ylim = ylim,
                       xlim = xlim, too.far = too.far, shade = shade,
                       shade.col = shade.col, shift = shift, trans = trans,
                       by.resids = by.resids, scheme = scheme[i])
graphics::layout(1)


save(x,
     pd2,
     gam_rent_mrf_forplot,
     gam_rent_re_forplot,
     file='data/mrf_re_plot.RData'
     )
