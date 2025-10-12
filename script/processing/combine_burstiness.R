###################################################
# create burstiness data
###################################################

# -- setup -- #

setwd('~/Github/PokkRacz2026a/')
library(tidyverse)
library(patchwork)
library(ggthemes)

b = read_tsv('dat/modifier_noun_bigrams_noun_level.tsv.gz')

w = jsonlite::read_json('dat/word_counts.json')

w = tibble(
  noun = names(w),
  burstiness = unlist(w) 
)

d = left_join(b,w) |> 
  filter(burstiness != 0, !is.na(burstiness)) |> 
  mutate(log10_burstiness = log10(burstiness))

write_tsv(d, 'dat/burstiness_data.gz')
