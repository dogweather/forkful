---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text within files is a common task in programming. It helps manage and manipulate data, tweak configuration settings, and upgrade code syntax - awesome stuff.

## How to:
Here's how you can search and replace text in Bash. The `sed` command gets it done. Example:

```Bash
echo "Hello World" | sed 's/World/Programmers/'
```

Output is:

```Bash
Hello Programmers
```

It's done. We replaced `World` with `Programmers`.

If you have a file to work with, do this:

```Bash
sed -i 's/foo/bar/g' filename
```

It replaces all 'foo' with 'bar' in the file named 'filename'. `-i` edits files in place and 'g' applies it globally on every line.

## Deep Dive
We used `sed` - short for stream editor - included in Unix systems since 1974. There are alternatives: like 'awk', 'perl', and 'grep'. The choice depends on your exact needs and personal preference. Simple text replacements work great with `sed`. For more complex tasks, `awk` or `perl` might suit you better.

On the implementation side, `sed` scans the file line by line, checking each line for the search term. When found, it gets replaced. This is why `sed` shines in large text files. For small files where performance doesn't matter much, any method will do.

## See Also
Explore further with these handy links:

1. [GNU sed documentation](https://www.gnu.org/software/sed/manual/sed.html)
2. [AWK - A Tutorial and Introduction](https://www.grymoire.com/Unix/Awk.html)
3. [Perl documentation](https://perldoc.perl.org/)