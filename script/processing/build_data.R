###################################################
# extract adjective + noun pairings from webcorpus bigram list
# duckdb + dbplyr backend
###################################################

library(DBI)
library(duckdb)
library(dplyr)
library(dbplyr)
library(tidyverse)

# -- paths -- #

duckdb_path   = "dat/webcorpus.duckdb"
bigram_path   = "~/Documents/Webcorpus2/dani_virag/virag_dani_web_archiv/bigram_freqs_processed_min10.tsv.gz"
freqlist_path = "~/Documents/Webcorpus2/frequency_list/data/freqlists2/webcorpus2_freqlist_hu_with_lemmafreq_hu_list_filt.tsv.gz"
output_path   = "dat/modifier_noun_bigrams_noun_level.tsv.gz"

# -- connect -- #

con = dbConnect(duckdb(), dbdir = duckdb_path)
message("Connected to DuckDB at: ", duckdb_path)

# -- helper: ingest a tsv.gz into duckdb if the table doesn't already exist -- #

ensure_table = function(con, path, table_name, header = TRUE) {
  if (dbExistsTable(con, table_name)) {
    n = tbl(con, table_name) |> count() |> collect() |> pull(n)
    message("  Table '", table_name, "' already exists (", format(n, big.mark = ","), " rows). Skipping ingest.")
  } else {
    message("  Table '", table_name, "' not found. Ingesting from:\n    ", path)
    t1 = Sys.time()
    from_clause = sprintf(
      "read_csv_auto('%s', delim='\\t', header=%s)",
      path,
      ifelse(header, "true", "false")
    )
    dbExecute(con, sprintf("CREATE TABLE %s AS SELECT * FROM %s", table_name, from_clause))
    elapsed = round(difftime(Sys.time(), t1, units = "secs"), 1)
    n = tbl(con, table_name) |> count() |> collect() |> pull(n)
    message("  Done. ", format(n, big.mark = ","), " rows ingested in ", elapsed, "s.")
  }
  tbl(con, table_name)
}

# -- ingest -- #

message("\n=== Checking / ingesting source tables ===")
bigrams_tbl  = ensure_table(con, bigram_path,   "bigrams",  header = FALSE)
freqlist_tbl = ensure_table(con, freqlist_path, "freqlist", header = TRUE)

# rename auto-generated columns in bigrams to X1, X2
# (read_csv_auto with header=false names them column0, column1)
bigrams_tbl = bigrams_tbl |>
  rename(X1 = column0, X2 = column1)

message("\n=== Source tables ready ===\n")

# -- buildPairs -- #

buildPairs = function(con, bigrams_tbl, freqlist_tbl) {

  # --- step 1: deduplicate frequency list, keep most frequent form per type ---

  message("[buildPairs] Filtering frequency list (freq > 10, dedup by form)...")

  c2 = freqlist_tbl |>
    filter(freq > 10) |>
    window_order(desc(freq)) |>
    group_by(form) |>
    filter(row_number() == 1) |>
    ungroup() |>
    select(form, lemma, xpostag, freq, lemma_freq, corpus_size)

  message("[buildPairs] Collecting for hunspell spell-check...")

  c2_local = c2 |> collect()
  message("  Collected ", format(nrow(c2_local), big.mark = ","), " rows into R.")

  # --- step 2: hunspell spell-check (R-side) ---

  message("[buildPairs] Running hunspell spell-check (this may take a moment)...")
  t1 = Sys.time()

  valid = hunspell::hunspell_check(c2_local$form, dict = hunspell::dictionary(lang = "hu_HU"))
  c3_local = c2_local |> filter(valid)

  elapsed = round(difftime(Sys.time(), t1, units = "secs"), 1)
  message("  Kept ", format(nrow(c3_local), big.mark = ","), " / ",
          format(nrow(c2_local), big.mark = ","),
          " forms that pass spell-check (", elapsed, "s).")

  # --- step 3: push spell-checked data back into duckdb ---

  message("[buildPairs] Pushing spell-checked forms back into DuckDB...")

  dbWriteTable(con, "c3_spellchecked", c3_local, overwrite = TRUE)
  c3 = tbl(con, "c3_spellchecked")

  # also push the full c2 (not spell-checked) — needed for adj and num
  dbWriteTable(con, "c2_deduped", c2_local, overwrite = TRUE)
  c2 = tbl(con, "c2_deduped")

  message("  Done.")

  # --- step 4: extract nouns (from spell-checked), adjectives and numerals (from deduped) ---

  message("[buildPairs] Identifying nouns, adjectives, numerals...")

  n = c3 |>
    filter(xpostag %like% "%/N]%") |>
    rename(form2 = form, lemma2 = lemma, xpostag2 = xpostag) |>
    select(form2, lemma2, xpostag2) |>
    filter(nchar(lemma2) > 1)

  adj = c2 |>
    filter(xpostag == "[/Adj][Nom]") |>
    rename(form1 = form, lemma1 = lemma, xpostag1 = xpostag) |>
    select(form1, lemma1, xpostag1) |>
    filter(nchar(lemma1) > 1)

  num = c2 |>
    filter(xpostag == "[/Num][Nom]") |>
    rename(form1 = form, lemma1 = lemma, xpostag1 = xpostag) |>
    select(form1, lemma1, xpostag1) |>
    filter(nchar(lemma1) > 1)

  n_nouns = n |> count() |> collect() |> pull(n)
  n_adj   = adj |> count() |> collect() |> pull(n)
  n_num   = num |> count() |> collect() |> pull(n)
  message("  Nouns: ", format(n_nouns, big.mark = ","),
          "  Adjectives: ", format(n_adj, big.mark = ","),
          "  Numerals: ", format(n_num, big.mark = ","))

  # --- step 5: parse bigrams ---

  message("[buildPairs] Parsing bigrams (lowercasing, splitting on '+')...")

  d0 = bigrams_tbl |>
    rename(bigram = X1, bigram_freq = X2) |>
    mutate(
      bigram = lower(bigram),
      form1  = regexp_extract(bigram, '^[^+]+(?=\\+)'),
      form2  = regexp_extract(bigram, '(?<=\\+)[^+]+$')
    ) |>
    distinct()

  # --- step 6: join bigrams with nouns × adjectives / numerals ---

  message("[buildPairs] Joining bigrams with noun+adjective and noun+numeral pairs...")

  d1a = d0 |>
    inner_join(n, by = "form2") |>
    inner_join(adj, by = "form1")

  d1b = d0 |>
    inner_join(n, by = "form2") |>
    inner_join(num, by = "form1")

  # --- step 7: compute marginal frequencies from bigram table ---

  d1c = d0 |>
    summarise(freq1 = sum(bigram_freq, na.rm = TRUE), .by = form1) |>
    mutate(corpus_size1 = sum(freq1, na.rm = TRUE))

  d1d = d0 |>
    summarise(freq2 = sum(bigram_freq, na.rm = TRUE), .by = form2) |>
    mutate(corpus_size2 = sum(freq2, na.rm = TRUE))

  # --- step 8: combine adj + num rows, attach marginals ---

  d1 = union_all(d1a, d1b) |>
    left_join(d1c, by = "form1") |>
    left_join(d1d, by = "form2") |>
    mutate(corpus_size = corpus_size1 + corpus_size2) |>
    select(-corpus_size1, -corpus_size2)

  # sum bigram frequencies over capitalisation variants
  d1 = d1 |>
    summarise(
      bigram_freq = sum(bigram_freq, na.rm = TRUE),
      .by = c(bigram, form1, form2, lemma1, lemma2, xpostag1, xpostag2, freq1, freq2, corpus_size)
    )

  # label bigram type
  d1 = d1 |>
    mutate(bigram_type = ifelse(xpostag1 %like% "%Num%", "number", "adjective"))

  # --- step 9: count modified vs non-modified preceding context ---

  message("[buildPairs] Computing modifier counts and log-odds...")

  n_counts_modified = d1 |>
    count(form2, lemma2, xpostag2, freq2, corpus_size, bigram_type, name = "n_preceding_modifier")

  # for "non-modified": bigrams whose form2 is a known noun but whose form1 is NOT a known adj/num
  n_counts_not_modified = d0 |>
    semi_join(n, by = "form2") |>
    anti_join(d1 |> select(form1, form2), by = c("form1", "form2")) |>
    count(form2, name = "n_preceding_other")

  n_counts_combined = n_counts_modified |>
    left_join(n_counts_not_modified, by = "form2") |>
    mutate(
      n_preceding_other = coalesce(n_preceding_other, 0L),
      log_odds_modified = log(
        (cast(n_preceding_modifier, "double") + 1) /
        (cast(n_preceding_other, "double") + 1)
      )
    )

  d2 = d1 |>
    full_join(n_counts_combined, by = c("form2", "lemma2", "xpostag2", "freq2", "corpus_size", "bigram_type"))

  # --- step 10: collect result into R ---

  message("[buildPairs] Collecting result into R...")
  t1 = Sys.time()

  result = d2 |> collect()

  elapsed = round(difftime(Sys.time(), t1, units = "secs"), 1)
  message("[buildPairs] Done. ",
          format(nrow(result), big.mark = ","), " rows, ",
          ncol(result), " columns. Collected in ", elapsed, "s.\n")

  return(result)
}

# -- calcInfo -- #
# operates on in-memory tibble (output of buildPairs)

calcInfo = function(dat) {

  message("[calcInfo] Computing information-theoretic measures...")

  # n distinct modifiers per noun
  d2b = dat |>
    distinct(form1, form2, lemma2, xpostag2, freq2, corpus_size) |>
    count(form2, lemma2, xpostag2, freq2, corpus_size, name = "n_distinct_form1")

  d3 = dat |>
    left_join(d2b, by = c("form2", "lemma2", "xpostag2", "freq2", "corpus_size")) |>
    mutate(
      modifier_max_entropy = log2(n_distinct_form1)
    )

  # probabilities and information measures
  d4 = d3 |>
    mutate(
      p_form1 = freq1 / corpus_size,
      p_form2 = freq2 / corpus_size,
      p_bigram = bigram_freq / corpus_size,
      log_freq2 = log10(freq2 / corpus_size * 10^6),
      pmi = log2(p_bigram / (p_form1 * p_form2)),
      p_adj_given_noun = p_bigram / p_form2,
      surprisal = -log2(p_adj_given_noun),
      weighted_surprisal = p_adj_given_noun * surprisal
    )

  # summarise to noun level
  d5 = d4 |>
    summarise(
      preceding_adjective_entropy = sum(weighted_surprisal),
      expected_pmi = sum(p_adj_given_noun * pmi),
      mean_pmi = mean(pmi),
      .by = c(bigram_type, form2, lemma2, xpostag2, log_freq2, freq2,
              modifier_max_entropy, log_odds_modified, corpus_size)
    )

  # tidy column names
  d6 = d5 |>
    rename(
      noun = form2,
      lemma = lemma2,
      xpostag = xpostag2,
      freq = freq2,
      log_freq = log_freq2
    )

  # readable case tag
  d7 = d6 |>
    mutate(
      tag = case_when(
        xpostag == "[/N][Nom]" ~ "nom",
        xpostag == "[/N][Acc]" ~ "acc",
        TRUE ~ "other"
      )
    ) |>
    relocate(tag, .after = xpostag)

  message("[calcInfo] Done. ",
          format(nrow(d7), big.mark = ","), " noun-level rows.\n")

  return(d7)
}

# -- run -- #

message("=== Starting build_data pipeline ===\n")

d2 = buildPairs(con, bigrams_tbl, freqlist_tbl)
d7 = calcInfo(d2)

# -- write -- #

message("Writing output to: ", output_path)
write_tsv(d7, output_path)
message("Done.\n")

# -- disconnect -- #

dbDisconnect(con, shutdown = TRUE)
message("DuckDB connection closed.")
