---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:21.839687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Removing quotes from a string involves stripping away the quotation marks that enclose the string. Programmers often want to do this to sanitize input data, prepare data for comparison purposes, or adhere to a specific data format when interfacing with other programs or systems.

## How to:
Bash has several ways to remove quotes from strings. Here are some quick examples:

```Bash
#!/bin/bash

# Using variable substitution to remove both single and double quotes
STRING="\"Hello, World!\""
echo ${STRING//\"}

# Using `tr` to delete quotes
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# Using `sed` to delete quotes
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

Sample output:

```
Hello, World!
Hello, World!
Hello, World!
```

## Deep Dive
Way back when, Unix commands like `tr` and `sed` were the primary tools for text processing. They are still in use today for their flexibility and power in handling text transformations like removing quotes. They're a staple in any shell-scripter's toolbox.

Bash itself has since evolved and variable substitution adds another layer of simplicity for small-scale string manipulations. It saves you from piping out to external binaries, making your scripts a bit more efficient.

While `tr` is great for deleting characters, it doesn't handle more complex patterns. `sed`, on the other hand, uses regular expressions, so it's overkill sometimes and might be slower for simple operations.

Choosing between these methods depends on your specific case. If you need to strip a variety of quotes and you're already in the context of a Bash script, using variable substitution is a no-brainer for its simplicity. But if you're transforming text streams or multi-line data, `tr` and `sed` are your go-to pals.

## See Also:
- The GNU Bash manual, especially the sections on Parameter Expansion and Shell Parameter Expansion: https://www.gnu.org/software/bash/manual/
- The `tr` command manual: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- The `sed` stream editor overview: https://www.gnu.org/software/sed/manual/sed.html