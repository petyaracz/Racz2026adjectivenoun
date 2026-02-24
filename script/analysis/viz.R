###################################################
# make plots
###################################################

# -- setup -- #

setwd('~/Github/Racz2026adjectivenoun/')
library(tidyverse)
library(patchwork)
library(ggthemes)

# -- read -- #

c = read_tsv('dat/modifier_noun_bigrams_noun_level.tsv.gz')
b = read_tsv('dat/burstiness_data.gz')

# -- subset -- #

cn = c |> 
  filter(tag == 'nom')

cna = cn |> 
  filter(bigram_type == 'adjective')

ca = c |> 
  filter(bigram_type == 'adjective', tag %in% c('nom','acc'))

ba = b |> 
  filter(bigram_type == 'adjective')

# -- corpus -- #

## adj

p1 = cna |> 
  ggplot(aes(log_freq, log_odds_modified)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('log(preceding adjective/other)')

p2 = cna |> 
  ggplot(aes(log_freq,modifier_max_entropy)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('max adjective entropy')

p3 = cna |> 
  ggplot(aes(log_freq,preceding_adjective_entropy)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('preceding adjective entropy')

p4 = cna |> 
  ggplot(aes(log_freq,expected_pmi)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('expected bigram pointwise mutual information')

p1 + p2 + p3 + p4 + plot_annotation(tag_levels = 'I')

ggsave('viz/corpus_nom_adjective.png', dpi = 'print', width = 6, height = 6)

## adj + num

cn$bigram = ifelse(cn$bigram_type == 'adjective', 'adjective+noun','numeral+noun')

p5 = cn |> 
  ggplot(aes(log_freq, log_odds_modified)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  facet_wrap(~bigram) +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('log(preceding modifier/other)')

p6 = cn |> 
  ggplot(aes(log_freq,modifier_max_entropy)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  facet_wrap(~bigram) +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('max modifier entropy') +
  scale_colour_colorblind()

p7 = cn |> 
  ggplot(aes(log_freq,preceding_adjective_entropy,colour)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  facet_wrap(~bigram) +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('preceding modifier entropy') +
  scale_colour_colorblind()

p8 = cn |> 
  ggplot(aes(log_freq,expected_pmi)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "count") +
  geom_smooth(colour = "red") +
  guides(fill = 'none') +
  facet_wrap(~bigram) +
  theme_bw() +
  xlab('log10 noun token frequency') +
  ylab('expected bigram\npointwise mutual information') +
  scale_colour_colorblind()

wrap_plots(p5,p6,p7,p8, ncol = 1) + plot_annotation(tag_levels = 'I') + plot_layout(guides = 'collect')

ggsave('viz/corpus_nom_adjective_nominal.png', dpi = 'print', width = 5, height = 10)

## burstiness based on wikipedia page counts

ba |> 
  ggplot(aes(log10_burstiness,log_odds_modified)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  xlab('log10 noun burstiness') +
  ylab('log(preceding adjective/other)') +
  scale_colour_colorblind()

ba |> 
  ggplot(aes(log10_burstiness,modifier_max_entropy)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  xlab('log10 noun burstiness') +
  ylab('max adjective entropy') +
  scale_colour_colorblind()

ba |> 
  ggplot(aes(log10_burstiness,preceding_adjective_entropy)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  xlab('log10 noun burstiness') +
  ylab('preceding adjective entropy') +
  coord_cartesian(ylim = c(0,2)) +
  scale_colour_colorblind()

ba |> 
  ggplot(aes(log10_burstiness,expected_pmi)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  xlab('log10 noun burstiness') +
  ylab('expected bigram pointwise mutual information') +
  coord_cartesian(ylim = c(0,5)) +
  scale_colour_colorblind()

# chef's kiss

## Hungarian ##

## adj

ca |> 
  ggplot(aes(log_freq,log_odds_modified,colour = tag)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  theme_bw() +
  ylab('melléknév + főnév párok aránya\nlog(p(melléknév + főnév)/p(egyéb + főnév))') +
  scale_colour_colorblind() +
  scale_x_continuous(
    name = 'log10 főnév típus gyakoriság',
    sec.axis = sec_axis(
      trans = ~ 10^. * unique(c$corpus_size) / 10^6,
      breaks = c(1,10,100,1000,10000,100000,1000000),
      name = "nyers gyakoriság"
    )
  )

ggsave('viz/korpusz1.png', width = 5, height = 4)

ca |> 
  ggplot(aes(log_freq,modifier_max_entropy,colour = tag)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  theme_bw() +
  ylab('max melléknév entrópia\nlog2(melléknevek száma)') +
  scale_colour_colorblind() +
  scale_x_continuous(
    name = 'log10 főnév típus gyakoriság',
    sec.axis = sec_axis(
      trans = ~ 10^. * unique(c$corpus_size) / 10^6,
      breaks = c(1,10,100,1000,10000,100000,1000000),
      name = "nyers gyakoriság"
    )
  )

ggsave('viz/korpusz2.png', width = 5, height = 4)

ca |> 
  ggplot(aes(log_freq,preceding_adjective_entropy,colour = tag)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  theme_bw() +
  ylab('A főnév előtti melléknevek entrópiája\nΣ p(melléknév|főnév) × [-log2 p(melléknév|főnév)]') +
  scale_colour_colorblind() +
  scale_x_continuous(
    name = 'log10 főnév típus gyakoriság',
    sec.axis = sec_axis(
      trans = ~ 10^. * unique(c$corpus_size) / 10^6,
      breaks = c(1,10,100,1000,10000,100000,1000000),
      name = "nyers gyakoriság"
    )
  )

ggsave('viz/korpusz3.png', width = 5, height = 4)

ca |> 
  ggplot(aes(log_freq,expected_pmi,colour = tag)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  geom_smooth() +
  theme_bw() +
  ylab('átlagos kollokációs erősség\nΣ p(melléknév|főnév) ×\nlog2[p(melléknév+főnév) / (p(melléknév) × p(főnév))]') +
  scale_colour_colorblind() +
  scale_x_continuous(
    name = 'log10 főnév típus gyakoriság',
    sec.axis = sec_axis(
      trans = ~ 10^. * unique(c$corpus_size) / 10^6,
      breaks = c(1,10,100,1000,10000,100000,1000000),
      name = "nyers gyakoriság"
    )
  )

ggsave('viz/korpusz4.png', width = 5, height = 4)

