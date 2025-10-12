###################################################
# make ci
###################################################

# -- setup -- #

setwd('~/Github/PokkRacz2026a/')
library(tidyverse)
library(patchwork)
library(mgcv)
library(performance)
library(broom)
library(sjPlot)
library(ggthemes)

# -- read -- #

c = read_tsv('dat/modifier_noun_bigrams_noun_level.tsv.gz')
b = read_tsv('dat/burstiness_data.gz')

# -- scale -- #

c$log_freq_scaled = as.double(scale(c$log_freq))
b$log10_burstiness_scaled = as.double(scale(b$log10_burstiness))
b$log_freq_scaled = as.double(scale(b$log_freq))

# -- scale -- #

cn = c |> 
  filter(tag == 'nom') |> 
  mutate(bigram_type = as.factor(bigram_type))

cnn = cn |> 
  filter(bigram_type == 'number')

cna = cn |> 
  filter(bigram_type == 'adjective')

ba = b |> 
  filter(bigram_type == 'adjective')

# -- corpus -- #

fit0a = bam(log_odds_modified ~ s(log_freq_scaled, by = bigram_type, k = 6) + bigram_type, data = cn)
fit0b = bam(modifier_max_entropy ~ s(log_freq_scaled, by = bigram_type, k = 6) + bigram_type, data = cn)
fit0c = bam(preceding_adjective_entropy ~ s(log_freq_scaled, by = bigram_type, k = 6) + bigram_type, data = cn)
fit0d = bam(expected_pmi ~ s(log_freq_scaled, by = bigram_type, k = 6) + bigram_type, data = cn)

fit0e = bam(log_odds_modified ~ s(log_freq_scaled, k = 6), data = cn)
fit0f = bam(modifier_max_entropy ~ s(log_freq_scaled, k = 6), data = cn)
fit0g = bam(preceding_adjective_entropy ~ s(log_freq_scaled, k = 6), data = cn)
fit0h = bam(expected_pmi ~ s(log_freq_scaled, k = 6), data = cn)

fit1a = bam(log_odds_modified ~ s(log_freq_scaled, k = 6), data = cna)
fit1b = bam(log_odds_modified ~ s(log_freq_scaled, k = 6), data = cnn)

fit2a = bam(modifier_max_entropy ~ s(log_freq_scaled, k = 6), data = cna)
fit2b = bam(modifier_max_entropy ~ s(log_freq_scaled, k = 6), data = cnn)

fit3a = bam(preceding_adjective_entropy ~ s(log_freq_scaled, k = 6), data = cna)
fit3b = bam(preceding_adjective_entropy ~ s(log_freq_scaled, k = 6), data = cnn)

fit4a = bam(expected_pmi ~ s(log_freq_scaled, k = 6), data = cna)
fit4b = bam(expected_pmi ~ s(log_freq_scaled, k = 6), data = cnn)

fit5a = bam(log_odds_modified ~ s(log10_burstiness, k = 6) + s(log_freq_scaled, k = 6), data = ba)
fit5b = bam(modifier_max_entropy ~ s(log10_burstiness, k = 6) + s(log_freq_scaled, k = 6), data = ba)
fit5c = bam(preceding_adjective_entropy ~ s(log10_burstiness, k = 6) + s(log_freq_scaled, k = 6), data = ba)
fit5d = bam(expected_pmi ~ s(log10_burstiness, k = 6) + s(log_freq_scaled, k = 6), data = ba)

compare_performance(fit0a,fit0e, metrics = 'common')
compare_performance(fit0b,fit0f, metrics = 'common')
compare_performance(fit0c,fit0g, metrics = 'common')
compare_performance(fit0d,fit0h, metrics = 'common')
# numbers and adjectives behave differently.

summary(fit1a) # .1
summary(fit1b) # .7
summary(fit2a) # .9
summary(fit2b) # .9
summary(fit3a) # .3
summary(fit3b) # .1
summary(fit4a) # .3
summary(fit4b) # .4

plot(fit1a)
plot(fit1b)
plot(fit2a)
plot(fit2b)
plot(fit3a)
plot(fit3b)
plot(fit4a)
plot(fit4b)

summary(fit5a) # 0
summary(fit5b) # 0.03 frequency effect
summary(fit5c) # 0
summary(fit5d) # 0

plot(fit5a)
plot(fit5b)
plot(fit5c)
plot(fit5d)

plot_model(fit0a, 'pred', terms = c('log_freq_scaled','bigram_type')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

plot_model(fit0b, 'pred', terms = c('log_freq_scaled','bigram_type')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

plot_model(fit0c, 'pred', terms = c('log_freq_scaled','bigram_type')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

plot_model(fit0d, 'pred', terms = c('log_freq_scaled','bigram_type')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()

p5a = plot_model(fit5a, 'pred', terms = c('log10_burstiness','log_freq_scaled')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ggtitle('') +
  ylab('log (preceding adjective/other)') +
  xlab('log10(n Wikipedia pages)') +
  labs(colour = 'log frequency (scaled)')

p5b = plot_model(fit5b, 'pred', terms = c('log10_burstiness','log_freq_scaled')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ggtitle('') +
  ylab('max adjective entropy') +
  xlab('log10(n Wikipedia pages)') +
  labs(colour = 'log frequency (scaled)')

p5c = plot_model(fit5c, 'pred', terms = c('log10_burstiness','log_freq_scaled')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ggtitle('') +
  ylab('preceding adjective entropy') +
  xlab('log10(n Wikipedia pages)') +
  labs(colour = 'log frequency (scaled)')

p5d = plot_model(fit5d, 'pred', terms = c('log10_burstiness','log_freq_scaled')) +
  theme_bw() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  ggtitle('') +
  ylab('expected pmi') +
  xlab('log10(n Wikipedia pages)') +
  labs(colour = 'log frequency (scaled)')

p5a + p5b + p5c + p5d + plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'I')

ggsave('viz/wikipedia_adjective_noun.png', dpi = 'print', width = 6.5, height = 6)
