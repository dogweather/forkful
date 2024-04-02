---
date: 2024-01-20 17:46:35.244931-07:00
description: Finding a string's length means counting its characters. Programmers
  do this to validate input, loop through characters, or simply align output.
lastmod: '2024-03-13T22:45:00.234970-06:00'
model: gpt-4-1106-preview
summary: Finding a string's length means counting its characters. Programmers do this
  to validate input, loop through characters, or simply align output.
title: Finding the length of a string
weight: 7
---

## What & Why?
Finding a string's length means counting its characters. Programmers do this to validate input, loop through characters, or simply align output.

## How to:
The `#` symbol gets the job done in bash. Use it with parameter expansion. Here's how:

```bash
my_string="Hello, World!"
string_length=${#my_string}
echo $string_length
```

Sample output:

```
13
```

## Deep Dive
In the old days, folks used `expr` or external tools like `wc -m`, but Bash built-in features changed the game. The `${#var}` syntax is part of parameter expansion introduced in Bash. It's fast and efficient because it doesn't spawn a subshell or call an external program.

Alternatives? Sure, you got 'em:

- `expr length "$my_string"` gives you the same result, but it's kinda old-school.
- `echo -n $my_string | wc -m` uses `wc` to count, but it's an overkill for simple tasks.

Details, details... When you use `${#my_string}`, it gets you the length in bytes by default. If your text walks on the unicode side of the street, you might need to consider multi-byte characters. That's when things get more complex.

## See Also
Dive into the man pages with `man bash` to get into the weeds of parameter expansion. For those looking into handling strings beyond basic ASCII, the Advanced Bash-Scripting Guide offers some insights: https://www.tldp.org/LDP/abs/html/. And for the love of learning, keep an eye on https://www.gnu.org/software/bash/manual/ for the latest on Bash.
