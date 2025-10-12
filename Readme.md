## Adjective + noun bigrams in the webcorpus

### building datasets

1. build_data (needs hungarian webcorpus 2 bigrams)
2. subset_freq, wiki_reader, combine_burstiness (needs hungarian wikipedia dump)

### datasets

- modifier_noun_bigrams_noun_level.tsv.gz: noun-level info on modifier+noun bigrams from hungarian webcorpus 2
- burstiness_data.gz: subset of above with noun burstiness calculated on Hungarian wikipedia

### sources

Hungarian webcorpus: https://hlt.bme.hu/en/resources/webcorpus2
Bigrams: https://nessie.ilab.sztaki.hu/~levai/tokenized_data/
Hungarian wikipedia dump: https://dumps.wikimedia.org/huwiki/20251001

Download files and then update path in build_data and wiki_parser. Files are big.

### analysis

- viz: visualisations
- model: simple bams to get numeric approximation of effect sizes
