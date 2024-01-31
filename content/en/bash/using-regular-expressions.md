---
title:                "Using regular expressions"
date:                  2024-01-19
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) are patterns that define search criteria for text. Programmers use them for matching, replacing, or extracting bits from strings based on these patterns—think complex find-and-replace on steroids.

## How to:
```Bash
# Matching a pattern
echo "I love to code in Bash" | grep -oP 'code'

# Output:
code

# Replacing string using regex with sed
echo "Bash 2023" | sed -E 's/[0-9]+/2024/'

# Output:
Bash 2024

# Extracting substring with regex
echo "Error: Line 42" | grep -oP '(?<=Line )\d+'

# Output:
42
```

## Deep Dive
Regular expressions have been around since the 1950s, originally conceived by mathematician Stephen Kleene. Alternatives to Bash regex include using `awk` or `perl`, which have their own regex capabilities. Implementation-wise, Bash uses grep for matching, `sed` for find-and-replace, and `=~` operator within `[[ ]]` for conditionals. Be aware that regex can vary between tools (`grep`, `egrep`, `sed`, and `awk`), so know the flavor you're working with.

## See Also
- [GNU Grep Manual](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed - An Introduction and Tutorial](https://www.grymoire.com/Unix/Sed.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex101: Online Regex Tester and Debugger](https://regex101.com/)
