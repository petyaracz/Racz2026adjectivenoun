###################################################
# narrow it down to one slice of the freq distro
###################################################

set.seed(1337)

setwd('~/Github/PokkRacz2026a/')
library(tidyverse)
library(patchwork)

# read, combine, filter

b = read_tsv('dat/modifier_noun_bigrams_noun_level.tsv.gz')

b2 = b |>
  filter(tag == 'nom', bigram_type == 'adjective')

# sample forms

nouns = b2 |> 
  distinct(form2,log_freq2) |> 
  mutate(
    log_freq_bin = ntile(log_freq2, 10)
  ) |> 
  filter(log_freq_bin == 5) |> 
  pull(form2)
  
b3 = b2 |> 
  filter(form2 %in% nouns)

# check

b3 |>
  distinct(form2,log_odds_modified,log_freq2) |>
  ggplot(aes(log_odds_modified,log_freq2)) +
  geom_point() +
  geom_smooth()

b3 |>
  distinct(form2,log_n_distinct_form1,log_freq2) |>
  ggplot(aes(log_n_distinct_form1,log_freq2)) +
  geom_point() +
  geom_smooth()

b3 |>
  filter(!is.na(n_distinct_form1)) |> 
  ggplot(aes(surprisal1,log_freq2)) +
  geom_point() +
  geom_smooth()

b3 |>
  filter(!is.na(n_distinct_form1)) |> 
  ggplot(aes(pmi_bigram,log_freq2)) +
  geom_point() +
  geom_smooth()

# sweet

write_lines(nouns, 'dat/words.txt', sep = '", "')
