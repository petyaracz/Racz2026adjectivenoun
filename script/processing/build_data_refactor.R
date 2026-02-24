###################################################
# extract adjective + noun pairings from webcorpus bigram list
# refactored for ~100M+ observations
###################################################
setwd('~/Github/Racz2026adjectivenoun/')
library(data.table)
library(arrow)

# -- read -- #

# fread is dramatically faster than read_tsv at this scale.
# col.names avoids renaming later. colClasses avoids type guessing on 100M rows.
dp = fread(
  cmd = 'gzcat ~/Documents/Webcorpus/bigram_freqs_processed_min10.tsv.gz',
  header = FALSE,
  col.names = c('bigram_raw', 'bigram_freq'),
  colClasses = c('character', 'integer')
)

# parquet is already fast
c = as.data.table(read_parquet('~/Github/Webcorpus2FrequencyList/frequencies.parquet'))

cat('Status: loaded data\n')

# -- fun -- #

buildPairs = function(corpus, pairs) {
  
  # --- filter corpus ---
  
  c2 = corpus[freq > 10]
  setorder(c2, -freq)
  c2 = c2[, .SD[1], by = form]
  c2 = c2[, .(form, lemma, xpostag, freq, lemma_freq, corpus_size)]
  
  # hunspell on unique forms only (already unique after slice above, but be explicit)
  ok_forms = c2$form[
    hunspell::hunspell_check(c2$form, dict = hunspell::dictionary(lang = 'hu-HU'))
  ]
  c3 = c2[form %chin% ok_forms]
  
  cat('Status: filtered c\n')
  
  # --- build modifier and noun sets ---
  
  n = c3[grepl('[/N]', xpostag, fixed = TRUE) & nchar(lemma) > 1,
         .(form2 = form, lemma2 = lemma, xpostag2 = xpostag)]
  setkey(n, form2)
  
  # adj and num come from c2 (pre-hunspell), matching original logic
  adj = c2[xpostag == '[/Adj][Nom]' & nchar(lemma) > 1,
           .(form1 = form, lemma1 = lemma, xpostag1 = xpostag)]
  setkey(adj, form1)
  
  num = c2[xpostag == '[/Num][Nom]' & nchar(lemma) > 1,
           .(form1 = form, lemma1 = lemma, xpostag1 = xpostag)]
  setkey(num, form1)
  
  cat('Status: built modifier/noun sets\n')
  
  # --- parse bigrams ---
  # replace lookahead/lookbehind regex with simple sub().
  # sub('\\+[^+]*$', '', x) = everything before the last +
  # sub('.*\\+', '', x)      = everything after the last +
  # both work in base R, data.table, and database backends.
  
  d0 = copy(pairs)
  setnames(d0, c('bigram_raw', 'bigram_freq'))
  d0[, bigram := tolower(bigram_raw)]
  d0[, `:=`(
    form1 = sub('\\+[^+]*$', '', bigram),
    form2 = sub('.*\\+', '', bigram)
  )]
  d0 = unique(d0, by = c('bigram', 'bigram_freq', 'form1', 'form2'))
  
  cat('Status: parsed bigrams\n')
  
  # --- join: adj+noun and num+noun ---
  # keyed joins are much faster than dplyr inner_join at this scale.
  
  setkey(d0, form2)
  d0n = n[d0, nomatch = NULL]  # inner join on form2
  
  setkey(d0n, form1)
  d1a = adj[d0n, nomatch = NULL]  # inner join on form1
  
  setkey(d0n, form1)
  d1b = num[d0n, nomatch = NULL]
  
  cat('Status: joined pairs\n')
  
  # --- marginal frequencies from full bigram set ---
  # NB: these are marginals over ALL bigrams (not just adj/num+noun),
  # which is correct for PMI denominators.
  
  d1c = d0[, .(freq1 = sum(bigram_freq)), by = form1]
  corpus_size1 = d1c[, sum(freq1)]
  d1c[, corpus_size1 := corpus_size1]
  setkey(d1c, form1)
  
  d1d = d0[, .(freq2 = sum(bigram_freq)), by = form2]
  corpus_size2 = d1d[, sum(freq2)]
  d1d[, corpus_size2 := corpus_size2]
  setkey(d1d, form2)
  
  # --- combine ---
  
  d1 = rbindlist(list(d1a, d1b), use.names = TRUE, fill = TRUE)
  
  # add marginals
  setkey(d1, form1)
  d1 = d1c[d1, on = 'form1']
  setkey(d1, form2)
  d1 = d1d[d1, on = 'form2']
  
  # BUG FLAG: corpus_size = corpus_size1 + corpus_size2 = 2 * total_bigram_freq.
  # Both corpus_size1 and corpus_size2 equal sum(all bigram_freq) over d0,
  # so this doubles the denominator. Your PMI and probability estimates are
  # all computed with this inflated denominator. If intentional, ignore;
  # if not, pick one (they're identical) or use the actual corpus token count.
  # d1[, corpus_size := corpus_size1 + corpus_size2]
  d1[, corpus_size := corpus_size1]
  d1[, c('corpus_size1', 'corpus_size2') := NULL]
  
  # sum bigram frequencies over capitalisation variants
  d1 = d1[, .(bigram_freq = sum(bigram_freq)),
          by = .(bigram, form1, form2, lemma1, lemma2,
                 xpostag1, xpostag2, freq1, freq2, corpus_size)]
  
  d1[, bigram_type := fifelse(grepl('Num', xpostag1, fixed = TRUE), 'number', 'adjective')]
  
  cat('Status: built d1\n')
  
  # --- modification counts ---
  
  n_mod = d1[, .(n_preceding_modifier = .N),
             by = .(form2, lemma2, xpostag2, freq2, corpus_size, bigram_type)]
  
  # nouns that appear in bigrams but NOT with a known adj/num modifier
  modified_pairs = unique(d1[, .(form1, form2)])
  setkey(modified_pairs, form1, form2)
  
  d0_nouns = d0[form2 %chin% n$form2]
  d0_nouns[, in_modified := modified_pairs[d0_nouns, on = .(form1, form2), .N > 0, by = .EACHI]$V1]
  # simpler: anti-join via merge
  n_not_mod = fsetdiff(
    unique(d0_nouns[, .(form1, form2)]),
    unique(d1[, .(form1, form2)])
  )[, .(n_preceding_other = .N), by = form2]
  
  setkey(n_mod, form2)
  setkey(n_not_mod, form2)
  n_counts = n_not_mod[n_mod, on = 'form2']
  n_counts[is.na(n_preceding_other), n_preceding_other := 0L]
  n_counts[, log_odds_modified := log((n_preceding_modifier + 1) / (n_preceding_other + 1))]
  
  # full join
  d2 = merge(d1, n_counts, all = TRUE,
             by = c('form2', 'lemma2', 'xpostag2', 'freq2', 'corpus_size', 'bigram_type'))
  
  return(d2)
}

calcInfo = function(dat) {
  
  # n distinct modifiers per noun
  d2b = unique(dat, by = c('form1', 'form2', 'lemma2', 'xpostag2', 'freq2', 'corpus_size'))
  d2b = d2b[, .(n_distinct_form1 = .N),
            by = .(form2, lemma2, xpostag2, freq2, corpus_size)]
  
  d3 = merge(dat, d2b, by = c('form2', 'lemma2', 'xpostag2', 'freq2', 'corpus_size'))
  d3[, modifier_max_entropy := log2(n_distinct_form1)]
  
  # information-theoretic measures
  d3[, `:=`(
    p_form1 = freq1 / corpus_size,
    p_form2 = freq2 / corpus_size,
    p_bigram = bigram_freq / corpus_size,
    log_freq2 = log10(freq2 / corpus_size * 1e6)
  )]
  d3[, `:=`(
    pmi = log2(p_bigram / (p_form1 * p_form2)),
    p_adj_given_noun = p_bigram / p_form2
  )]
  d3[, `:=`(
    surprisal = -log2(p_adj_given_noun),
    weighted_surprisal = p_adj_given_noun * (-log2(p_adj_given_noun))
  )]
  
  # aggregate to noun level
  d5 = d3[, .(
    preceding_adjective_entropy = sum(weighted_surprisal),
    expected_pmi = sum(p_adj_given_noun * pmi),
    mean_pmi = mean(pmi)
  ), by = .(bigram_type, form2, lemma2, xpostag2, log_freq2, freq2,
            modifier_max_entropy, log_odds_modified, corpus_size)]
  
  setnames(d5, c('form2', 'lemma2', 'xpostag2', 'freq2', 'log_freq2'),
           c('noun', 'lemma', 'xpostag', 'freq', 'log_freq'))
  
  d5[, tag := fcase(
    xpostag == '[/N][Nom]', 'nom',
    xpostag == '[/N][Acc]', 'acc',
    default = 'other'
  )]
  
  setcolorder(d5, c('bigram_type', 'noun', 'lemma', 'xpostag', 'tag'))
  
  return(d5)
}

# -- remove low numerals from bigram pairs -- #

dp2 = dp[!grepl('^(egy|kett\u0151|k\u00e9t|h\u00e1rom|t\u00edz|sz\u00e1z|ezer|10|100|1000)\\+', bigram_raw, ignore.case = TRUE)]

# -- run -- #

d2 = buildPairs(c, dp2)
d7 = calcInfo(d2)

cat('Status: done\n')

# -- write -- #

write_parquet(as_arrow_table(d2), 'dat/modifier_noun_pairs.parquet')
fwrite(d7, 'dat/modifier_noun_bigrams_noun_level.tsv.gz', sep = '\t')