import mwxml
import bz2

# Read words from file
with open('../../dat/words.txt', 'r', encoding='utf-8') as f:
    content = f.read().strip()
    # Remove quotes and split by comma
    words = [w.strip().strip('"').strip("'") for w in content.split(',')]

print(f"Loaded {len(words)} words from words.txt")
print(f"First few words: {words[:5]}")

# Track counts only
word_counts = {w: 0 for w in words}

# Open the dump
dump = mwxml.Dump.from_file(bz2.open('/Users/pracz/Downloads/huwiki-20251001-pages-articles-multistream.xml.bz2'))

page_count = 0
for page in dump:
    if page.namespace != 0:
        continue
    
    page_count += 1
    if page_count % 10000 == 0:
        print(f"Processed {page_count} articles...")
        # Show a few counts as we go
        sample_words = list(word_counts.keys())[:3]
        for w in sample_words:
            print(f"  {w}: {word_counts[w]} pages so far")
    
    for revision in page:
        text = (revision.text or "").lower()
        for word in words:
            if word.lower() in text:
                word_counts[word] += 1
        break

print(f"\n=== FINAL RESULTS ===")
print(f"Processed {page_count} total articles\n")
for word in words:
    print(f"{word}: {word_counts[word]} pages")

# Save counts only
import json
with open('../../dat/word_counts.json', 'w', encoding='utf-8') as f:
    json.dump(word_counts, f, ensure_ascii=False, indent=2)

print("\nCounts saved to word_counts.json")