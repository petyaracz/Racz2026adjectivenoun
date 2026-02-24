# flagged issue: corpus size was double it should be in previous runs

###################################################
# extract adjective + noun pairings from webcorpus bigram list
###################################################
setwd('~/Github/Racz2026adjectivenoun/')
library(tidyverse)
library(arrow)

# -- read -- #

dp = read_tsv('~/Documents/Webcorpus/bigram_freqs_processed_min10.tsv.gz', col_names = F) # doesn't contain non-a ch
c = read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet')

print('Status: loaded data')

## test run ##

ct = c |>
  filter(form %in% c('kutya','elefánt','vabank','kék','bútorlap','megy','három','sok','szép','nagy'))

dt = dp |>
  mutate(
    bigram = str_to_lower(X1),
    form1 = str_extract(bigram, '^[^\\+].*(?=\\+)'),
    form2 = str_extract(bigram, '(?<=\\+)[^\\+].*$')
  ) |>
  filter(
    form2 %in% c('kutya','elefánt','vabank'),
    form1 %in% c('egy','sok','szép','nagy')
  ) |>
  select(X1,X2)

# -- fun -- #

# filter for nom / acc / dat

buildPairs = function(corpus,pairs){
  
  # magical handwave homonym filtering: if type x repeats in corpus, we only keep the most frequent form
  
  c2 = corpus |> 
    filter(freq > 10) |> 
    arrange(-freq) |> 
    slice(1, .by = form) |>  # para
    select(form,lemma,xpostag,freq,lemma_freq,corpus_size)
  
  c3 = c2 |> 
    filter(hunspell::hunspell_check(form, dict = hunspell::dictionary(lang = 'hu-HU')))
  
  print('Status: filtered c')
  
  n = c3 |> 
    filter(str_detect(xpostag, '\\[\\/N\\]')) |> 
    rename(
      form2 = form,
      lemma2 = lemma,
      xpostag2 = xpostag
    ) |> 
    select(form2,lemma2,xpostag2) |> 
    filter(nchar(lemma2) > 1)
  
  adj = c2 |> 
    filter(xpostag == '[/Adj][Nom]') |> 
    rename(
      form1 = form,
      lemma1 = lemma,
      xpostag1 = xpostag
    ) |> 
    select(form1,lemma1,xpostag1) |> 
    filter(nchar(lemma1) > 1)
  
  num = c2 |> 
    filter(xpostag == '[/Num][Nom]') |> 
    rename(
      form1 = form,
      lemma1 = lemma,
      xpostag1 = xpostag
    ) |> 
    select(form1,lemma1,xpostag1) |> 
    filter(nchar(lemma1) > 1)
  
  print('Status: built d')
  
  d0 = pairs |> 
    rename(
      bigram = X1,
      bigram_freq = X2
    ) |> 
    mutate(
      bigram = str_to_lower(bigram),
      form1 = str_extract(bigram, '^[^\\+].*(?=\\+)'),
      form2 = str_extract(bigram, '(?<=\\+)[^\\+].*$')
    ) |> 
    distinct()
  
  d1a = d0 |> 
    inner_join(n) |> 
    inner_join(adj) 
  
  d1b = d0 |> 
    inner_join(n) |> 
    inner_join(num)
  
  d1c = d0 |> 
    summarise(
      freq1 = sum(bigram_freq),
      .by = form1
    ) |> 
    mutate(corpus_size1 = sum(freq1))
  
  d1d = d0 |> 
    summarise(
      freq2 = sum(bigram_freq),
      .by = form2
    ) |> 
    mutate(corpus_size2 = sum(freq2))
  
  d1 = d1a |> 
    bind_rows(d1b) |> 
    left_join(d1c) |> 
    left_join(d1d) |> 
    mutate(corpus_size = corpus_size1) |> # !!!
    select(-corpus_size1,-corpus_size2)
  
  # sum bigram frequencies over caps/no caps
  d1 = d1 |> 
    summarise(
      bigram_freq = sum(bigram_freq),
      .by = c(bigram,form1,form2,lemma1,lemma2,xpostag1,xpostag2,freq1,freq2,corpus_size)
    )
  
  # big one
  d1 = d1 |> 
    mutate(bigram_type = ifelse(
      str_detect(xpostag1, 'Num'),
      'number',
      'adjective'
    )
    )
  
  # .. count modified and non-modified instances
  n_counts_modified = d1 |> 
    count(form2,lemma2,xpostag2,freq2,corpus_size,bigram_type, name = 'n_preceding_modifier')
  
  n_counts_not_modified = d0 |> 
    filter(form2 %in% n$form2) |> 
    anti_join(d1 |> select(form1, form2)) |> 
    count(form2, name = 'n_preceding_other')
  
  n_counts_modified_or_not = n_counts_modified |> 
    left_join(n_counts_not_modified) |> # a consequential move
    mutate(
      # n_preceding_modifier = replace_na(n_preceding_modifier, 0),
      n_preceding_other = replace_na(n_preceding_other, 0), # horrible
      log_odds_modified = log((n_preceding_modifier+1)/(n_preceding_other+1))
    )
  
  d2 = d1 |> 
    full_join(n_counts_modified_or_not)
  
  return(d2)
}

calcInfo = function(dat){
  
  # n distinct modifiers per noun
  d2b = dat |>
    distinct(form1,form2,lemma2,xpostag2,freq2,corpus_size) |> 
    count(form2,lemma2,xpostag2,freq2,corpus_size, name = 'n_distinct_form1')
  
  d3 = dat |> 
    left_join(d2b) |> 
    mutate(
      modifier_max_entropy = log2(n_distinct_form1)
    )
  
  # adj entropy avg
  d4 = d3 |> 
    mutate(
      p_form1 = freq1/corpus_size,
      p_form2 = freq2/corpus_size,
      p_bigram = bigram_freq/corpus_size,
      log_freq2 = log10(freq2 / corpus_size * 10^6),
      pmi = log2(p_bigram / (p_form1 * p_form2)),
      p_adj_given_noun = p_bigram / p_form2,
      surprisal = -log2(p_adj_given_noun),
      weighted_surprisal = p_adj_given_noun * surprisal
    )
  
  # avgs
  d5 = d4 |> 
    summarise(
      preceding_adjective_entropy = sum(weighted_surprisal),
      expected_pmi = sum(p_adj_given_noun * pmi),
      mean_pmi = mean(pmi),
      .by = c(bigram_type,form2,lemma2,xpostag2,log_freq2,freq2,modifier_max_entropy,log_odds_modified,corpus_size)
    ) 
  
  # tidy up cols
  d6 = d5 |> 
    rename(
      noun = form2,
      lemma = lemma2,
      xpostag = xpostag2,
      freq = freq2,
      log_freq = log_freq2
    )
  
  # add info, clear up tag
  d7 = d6 |> 
    mutate(
      tag = case_when(
        xpostag == '[/N][Nom]' ~ 'nom',
        xpostag == '[/N][Acc]' ~ 'acc',
        T ~ 'other'
      )
    ) |> 
    relocate(tag, .after = xpostag)
  
  return(d7)
}

# -- remove 1-2-3 from numerals -- #

dp2 = dp |> 
  filter(str_detect(X1, '^(egy|kettő|két|három|tíz|száz|ezer|10|100|1000)\\+', negate = T))

# -- run -- #

# test
d2t = buildPairs(ct, dt)
d7t = calcInfo(d2t)

# run
d2 = buildPairs(c, dp2)
d7 = calcInfo(d2)

# -- write -- #

write_parquet(d2, 'dat/modifier_noun_pairs.parquet')
write_tsv(d7, 'dat/modifier_noun_bigrams_noun_level.tsv.gz')
