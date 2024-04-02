---
date: 2024-02-03 19:02:36.536161-07:00
description: "Capitalizing a string means modifying it so the first letter is uppercase\
  \ and the remainder of the string is lowercase. This is a common task in text\u2026"
lastmod: '2024-03-13T22:45:00.456916-06:00'
model: gpt-4-0125-preview
summary: "Capitalizing a string means modifying it so the first letter is uppercase\
  \ and the remainder of the string is lowercase. This is a common task in text\u2026"
title: Capitalizing a string
weight: 2
---

## What & Why?

Capitalizing a string means modifying it so the first letter is uppercase and the remainder of the string is lowercase. This is a common task in text processing, user input normalization, and data formatting to ensure consistency or to meet specific formatting criteria.

## How to:

In Fish Shell, strings can be manipulated directly with built-in functions, without the need for external tools or libraries. To capitalize a string, you can combine the `string` command with subcommands.

```fish
# Sample string
set sample_string "hello world"

# Capitalize first letter
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

Output:
```
Hello world
```

For scenarios requiring the capitalization of multiple words in a string (e.g., converting "hello world" to "Hello World"), you would iterate over each word, applying the capitalization logic to each:

```fish
# Sample sentence
set sentence "hello fish shell programming"

# Capitalize each word
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# Join the capitalized words
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

Output:
```
Hello Fish Shell Programming
```

Note that Fish Shell does not directly offer a single-command approach for full sentence capitalization in the same way some programming languages do with their string methods. Therefore, combining `string split`, `string sub`, `string upper`, and then rejoining represents an idiomatic approach in Fish Shell for achieving this.
