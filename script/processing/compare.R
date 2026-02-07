library(tidyverse)
library(digest)

# read both outputs
old = read_tsv("dat/modifier_noun_bigrams_noun_level_OLD.tsv.gz")
new = read_tsv("dat/modifier_noun_bigrams_noun_level.tsv.gz")

# sort both the same way so row order doesn't matter
sort_cols = names(old)
old_sorted = old |> arrange(across(all_of(sort_cols)))
new_sorted = new |> arrange(across(all_of(sort_cols)))

# checksums
old_hash = digest(old_sorted, algo = "sha256")
new_hash = digest(new_sorted, algo = "sha256")

message("Old: ", old_hash)
message("New: ", new_hash)
message("Match: ", old_hash == new_hash)

# if they don't match, figure out why
if (old_hash != new_hash) {
  message("\n=== Dimensions ===")
  message("Old: ", nrow(old), " x ", ncol(old))
  message("New: ", nrow(new), " x ", ncol(new))

  message("\n=== Column names match: ", identical(sort(names(old)), sort(names(new))))

  # find rows in one but not the other
  only_old = anti_join(old, new)
  only_new = anti_join(new, old)
  message("\nRows only in old: ", nrow(only_old))
  message("Rows only in new: ", nrow(only_new))

  # check for floating point drift
  if (identical(dim(old_sorted), dim(new_sorted)) && identical(names(old_sorted), names(new_sorted))) {
    numeric_cols = names(old_sorted)[sapply(old_sorted, is.numeric)]
    message("\n=== Max absolute difference per numeric column ===")
    for (col in numeric_cols) {
      diff = max(abs(old_sorted[[col]] - new_sorted[[col]]), na.rm = TRUE)
      message("  ", col, ": ", format(diff, scientific = TRUE))
    }
  }
}