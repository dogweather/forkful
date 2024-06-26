---
date: 2024-02-03 19:02:46.465958-07:00
description: "How to: In Fish Shell, you don't have built-in commands specifically\
  \ designed for parsing dates from strings. Instead, you rely on external utilities\
  \ like\u2026"
lastmod: '2024-03-13T22:45:00.482992-06:00'
model: gpt-4-0125-preview
summary: In Fish Shell, you don't have built-in commands specifically designed for
  parsing dates from strings.
title: Parsing a date from a string
weight: 30
---

## How to:
In Fish Shell, you don't have built-in commands specifically designed for parsing dates from strings. Instead, you rely on external utilities like `date` (available in Linux and macOS) or leverage popular third-party tools such as `GNU date` for more complex parsing. Here's how to approach it:

**Using `date` with Fish:**

To parse a date string in the format "YYYY-MM-DD", you can use the `date` command with the `-d` (or `--date` for GNU date) option followed by the string. The `+` option is used to format the output.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Output: Saturday, 01 April 2023
```

For macOS (which requires a different format for the `-j` and `-f` flags):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Output: Saturday, 01 April 2023
```

**Using GNU `date` for complex parsing:** 

GNU `date` is more flexible with string formats. It can auto-detect many common date string formats without explicitly specifying the input format:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Output: 2023-04-01 14:00:00
```

However, when working with date strings that may not be automatically recognized or when precise control over the input format is needed, specifying the input format with GNU `date` isn't directly supported. In such cases, consider preprocessing the string or using another tool designed for more complex date parsing routines.
