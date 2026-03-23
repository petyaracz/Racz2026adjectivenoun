###################################################
# narrow it down to one slice of the freq distro
###################################################

set.seed(1337)

setwd('~/Github/Racz2026adjectivenoun/')
library(tidyverse)
library(patchwork)

# read, combine, filter

b = read_tsv('dat/modifier_noun_bigrams_noun_level.tsv.gz')

b2 = b |>
  filter(tag == 'nom', bigram_type == 'adjective')

# sample forms

nouns = b2 |> 
  distinct(noun,log_freq) |> 
  mutate(
    log_freq_bin = ntile(log_freq, 10)
  ) |> 
  filter(log_freq_bin == 5) |> 
  pull(noun)
  
b3 = b2 |> 
  filter(noun %in% nouns)

# check

b3 |>
  distinct(noun,log_odds_modified,log_freq) |>
  ggplot(aes(log_odds_modified,log_freq)) +
  geom_point() +
  geom_smooth()

# NOTE: column no longer exists in current pipeline output
# b3 |>
#   distinct(noun,log_n_distinct_form1,log_freq) |>
#   ggplot(aes(log_n_distinct_form1,log_freq)) +
#   geom_point() +
#   geom_smooth()

# NOTE: column no longer exists in current pipeline output
# b3 |>
#   filter(!is.na(n_distinct_form1)) |> 
#   ggplot(aes(surprisal1,log_freq)) +
#   geom_point() +
#   geom_smooth()

# NOTE: column no longer exists in current pipeline output
# b3 |>
#   filter(!is.na(n_distinct_form1)) |> 
#   ggplot(aes(pmi_bigram,log_freq)) +
#   geom_point() +
#   geom_smooth()

# sweet

write_lines(nouns, 'dat/words.txt', sep = '", "')
