---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular Expressions (RegEx) are sequences of characters that match or find other strings or sets of strings, following specific rules. Programmers use it for string searching and manipulation.

## How to:

Here's a simple example showing how to use regular expressions in Bash to find all "txt" files.

```Bash
for file in $(ls | grep -P '\.txt$')
do
    echo $file
done
```

This script lists only "txt" files in the current directory. `-P` flag enables perl-regexp for advanced functionality.

## Deep Dive:

RegEx has been around since the '50s and is extensively used in computer programming. The feature is integral in pattern matching, string parsing, data validation, and more. Despite its power, RegEx can be complex and tricky to handle for intricate patterns; hence alternatives like string functions or parsers are sometimes preferred for simplicity's sake.

Bash uses POSIX standard for regular expressions. It doesn't support perl-regexp natively but uses `grep -P`. Unfortunately, this doesn't work in all systems. In Mac, you might need to install `ggrep`.

## See Also:

1. [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
2. [Grep Man Page](https://man7.org/linux/man-pages/man1/grep.1.html)
3. [Advanced Bash-Scripting Guide: Regular Expressions](http://tldp.org/LDP/abs/html/regexp.html#REGEXREF)