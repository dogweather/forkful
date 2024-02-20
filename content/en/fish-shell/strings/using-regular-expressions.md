---
date: 2024-02-03 19:02:42.902269-07:00
description: "Regular expressions (regex) in Fish Shell allow you to search, match,\
  \ and manipulate strings based on specific patterns. Programmers utilize regex for\u2026"
lastmod: 2024-02-19 22:05:18.923933
model: gpt-4-0125-preview
summary: "Regular expressions (regex) in Fish Shell allow you to search, match, and\
  \ manipulate strings based on specific patterns. Programmers utilize regex for\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?

Regular expressions (regex) in Fish Shell allow you to search, match, and manipulate strings based on specific patterns. Programmers utilize regex for tasks like input validation, parsing, and text processing because it offers a compact and powerful way to specify complex text patterns.

## How to:

While Fish Shell itself does not have a built-in command for regex, it effectively uses external commands like `grep`, `sed`, and `awk` that support regex, allowing you to incorporate regex operations in your scripts.

### Basic Pattern Matching with `grep`
Search for lines in a file that match a pattern:

```fish
grep '^[0-9]+' myfile.txt
```

This command finds lines starting with one or more digits in `myfile.txt`.

### Extracting & Replacing with `sed`
Extract phone numbers from a file:

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

Replace all occurrences of "foo" with "bar" in `data.txt`:

```fish
sed 's/foo/bar/g' data.txt
```

### Using `string` for Basic Regex
Fish Shell's `string` command supports simple regex operations like match and replace:

Match a pattern in a string:

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
Output:
```
3.1.2
```

Replace digits following 'fish' with 'X.X.X':

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
Output:
```
Welcome to fish X.X.X
```

### Advanced Matching with `awk`
Print the second column of data where the first column matches a specific pattern:

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

This command looks for lines in `datafile` where the first column starts with an "a" followed by one or more digits and prints the second column.

By integrating these external commands, Fish Shell programmers can harness the full power of regular expressions for complex text manipulation tasks, enhancing the shell's native capabilities.
