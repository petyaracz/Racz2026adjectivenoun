###################################################
# subset adj + noun list and shuffle it to see how patterns look
###################################################
setwd('~/Github/Racz2026adjectivenoun/')
library(tidyverse)
library(ggthemes)
library(arrow)

set.seed(1337)

# -- fun -- #

sampleData = function(){
  pairs = read_parquet('dat/modifier_noun_pairs.parquet')
  
  nouns = pairs |> 
    filter(xpostag2 == '[/N][Nom]') |> 
    distinct(form2) |> 
    sample_n(10000) |> 
    pull(form2)
  
  sample = pairs |> 
    filter(form2 %in% nouns) 
  
  write_tsv(sample, 'dat/shuffled_pairs.tsv.gz')
}

# get entropy (stripped down version of calc in build_data)
getEntropy = function(s){
  s |>
    mutate(
      log_freq2 = log10(freq2 / corpus_size * 10^6),
      log_freq_scaled = as.double(scale(log_freq2)),
      p_form2 = freq2/corpus_size,
      p_bigram = bigram_freq/corpus_size,
      p_adj_given_noun = p_bigram / p_form2,
      surprisal = -log2(p_adj_given_noun),
      weighted_surprisal = p_adj_given_noun * surprisal
    ) |>  
    summarise(
      preceding_adjective_entropy = sum(weighted_surprisal),
      .by = c(bigram_type,form2,lemma2,xpostag2,log_freq2,freq2,log_odds_modified,corpus_size,bigram_type,log_freq_scaled)
    ) 
}

# sampleData() # this won't work with the files in the repo (parquet file missing) but tells you what I did

# -- read -- #

s = read_tsv('dat/shuffled_pairs.tsv.gz')

# -- format -- #

# adjectives only sir
s = s |> 
  filter(bigram_type == 'adjective')

# baseline
s2 = getEntropy(s)

# -- shuffle -- #

# -- 1. Global adjective distribution (from full bigram data) -- #

adj_marginal <- s |>
  summarise(total_bigram_freq = sum(bigram_freq), .by = form1) |>
  mutate(prob = total_bigram_freq / sum(total_bigram_freq))

prob_vec <- adj_marginal$prob

# -- 2. Total adjective tokens per noun (for sampled nouns) -- #

noun_totals <- s |>
  summarise(
    total_adj_tokens = sum(bigram_freq),
    log_freq2 = first(log10(freq2 / corpus_size * 10^6)),
    .by = c(form2, freq2, corpus_size)
  )

# -- 3. Entropy from a count vector -- #

entropy_from_counts <- function(counts) {
  counts <- counts[counts > 0]
  p <- counts / sum(counts)
  -sum(p * log2(p))
}

# -- 4. Simulate -- #

n_sims <- 3

sim_results <- map_dfr(1:n_sims, function(sim) {
  entropies <- map_dbl(noun_totals$total_adj_tokens, function(n) {
    counts <- rmultinom(1, size = n, prob = prob_vec)[, 1]
    entropy_from_counts(counts)
  })
  noun_totals |>
    mutate(sim_entropy = entropies, sim_id = sim)
}, .progress = TRUE)

# -- 5. Summarise null distribution per noun -- #

sim_summary <- sim_results |>
  summarise(
    sim_mean = mean(sim_entropy),
    sim_lo = quantile(sim_entropy, 0.025),
    sim_hi = quantile(sim_entropy, 0.975),
    .by = c(form2, log_freq2, total_adj_tokens)
  )

# -- 6. Join with real entropy and compare -- #

comparison <- s2 |> 
  select(form2, log_freq2, preceding_adjective_entropy) |>
  inner_join(sim_summary, by = c("form2", "log_freq2"))

# -- viz -- #

comparison |> 
  ggplot(aes(sim_mean)) +
  geom_histogram()

comparison |> 
  select(form2,log_freq2,preceding_adjective_entropy,sim_mean) |> 
  pivot_longer(-c(form2,log_freq2), names_to = 'type', values_to = 'entropy') |> 
  ggplot(aes(log_freq2,entropy,colour = type)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind()

comparison |> 
  mutate(sim_real_diff = sim_mean - preceding_adjective_entropy) |> 
  ggplot(aes(log_freq2,sim_real_diff)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw()

comparison |> 
  mutate(sim_real_diff_weighted = (sim_mean - preceding_adjective_entropy) / sim_mean) |> 
  ggplot(aes(log_freq2,sim_real_diff_weighted)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw()
