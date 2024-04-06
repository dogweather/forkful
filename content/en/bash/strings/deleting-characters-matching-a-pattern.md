---
date: 2024-01-20 17:41:29.677647-07:00
description: ''
lastmod: '2024-04-05T22:50:48.784072-06:00'
model: gpt-4-1106-preview
summary: ''
title: Deleting characters matching a pattern
weight: 5
---

## How to:


### Delete leading/trailing whitespace:
```Bash
text="   Hello, World!   "
trimmed=$(echo "$text" | xargs)
echo "$trimmed"
```
Output: `Hello, World!`

### Remove all digits:
```Bash
text="B4sh i5 amaz1ng!"
cleaned=${text//[^a-zA-Z ]/}
echo "$cleaned"
```
Output: `Bsh i amazng`

### Replace specific characters:
```Bash
text="Hello-World!"
cleaned=${text//-/_}
echo "$cleaned"
```
Output: `Hello_World!`

## Deep Dive
In the beginning, text processing tools like `sed` and `awk` were the go-to for string manipulation. Bash has since incorporated pattern matching and string manipulation directly into the shell itself, giving its users plenty of power without the need for external commands.

The `${parameter/pattern/string}` syntax is one approach where you replace first match of `pattern` with `string`. To remove all matches, just add another `/` as shown in the above examples.

Alternatives include using classic UNIX tools like `sed`, `awk`, `tr`, or more modern scripting languages such as Python or Perl.

Under the hood, Bash uses globbing and wildcards for pattern matching, but when you see those `${text//pattern/}` constructs, you're dealing with Bash's parameter expansion—a feature that's mighty handy for string manipulation.

## See Also
- Bash Manual on Parameter Expansion: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- An article on text processing in Linux: https://www.linuxjournal.com/content/pattern-matching-bash
- Sed & Awk 101 Hacks eBook: https://www.thegeekstuff.com/ebooks/sed_awk_101_hacks
