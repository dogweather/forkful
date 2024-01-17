---
title:                "Extracting substrings"
html_title:           "Bash recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is a process where a specific portion of a string is retrieved. Programmers often need to extract substrings to manipulate data, format strings, or perform text processing tasks.

## How to:

To extract a substring in Bash, we can use the built-in `expr` command or the string manipulation tool, `cut`. Here are some examples:

### Extract first N characters in a string:
```Bash
echo "Hello World" | cut -c -5
```
Output: `Hello`

### Extract portion of string between two characters:
```Bash
expr match "This is a sample string" '.*\(is.*\)''
```
Output: `is a sample string`

## Deep Dive:

### Historical Context:
Extracting substrings has been a fundamental concept in computer programming since the early days of computing, dating back to the 1950s. It has been used in various programming languages, including Bash, to manipulate and process strings efficiently.

### Alternative Methods:
Apart from `cut` and `expr`, there are other ways to extract substrings in Bash, such as using `sed` and `awk` commands. However, these methods may require more complex syntax and are not as commonly used for substring extraction.

### Implementation Details:
To extract a substring using `cut`, we specify the start and end position of the desired substring using the `-c` flag. The positions can be defined using single numbers, or a range of numbers separated by a dash. The `expr` command uses regular expressions to match and extract substrings from a given string.

## See Also:

For more information on substring extraction in Bash, check out the following resources:

- [Bash Documentation on String Manipulation](https://www.gnu.org/software/bash/manual/html_node/String-Manipulation.html)
- [Unix Tutorial: Cut Command](http://www.unixtutorial.org/2008/04/cut-a-key-unix-command/)
- [Unix Tutorial: Expr Command](http://www.unixtutorial.org/2008/04/expr-command/)
- [Stack Overflow: How to Extract Substring in Bash](https://stackoverflow.com/questions/22730780/how-to-extract-substring-in-bash)