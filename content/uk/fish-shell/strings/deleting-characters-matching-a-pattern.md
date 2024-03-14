---
date: 2024-01-20 17:42:13.429887-07:00
description: ''
lastmod: '2024-03-13T22:44:50.038514-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
---

{{< edit_this_page >}}

## What & Why?
## Що і чому?
Pattern deletion removes specific characters from strings—like cleaning your text from unwanted symbols. Programmers use it to format data, scrub user input, or prepare text for processing.

## How to:
## Як це зробити:
Let's delete vowels from a string. The `string` command does the heavy lifting.

```Fish Shell
set my_string "Vowels will vanish"
string replace -a -r "[aeiou]" "" -- $my_string
```

Output:

```Fish Shell
Vwls wll vnsh
```

Now, let’s strip numbers from filenames:

```Fish Shell
set filename "report_2023.txt"
string replace -a -r "[0-9]" "" -- $filename
```

Output:

```Fish Shell
report_.txt
```

## Deep Dive
## Занурення у деталі
Fish Shell debuted in 2005, focused on usability and scripting ease. Pattern deletion is built-in via `string match`, `string replace` commands, unlike POSIX shells that lean on `grep`, `sed`, or `awk`.

Alternatives? Sure. PowerShell’s `-replace`, Python’s `.replace()`, or good ol’ `sed` for a more traditional command-line approach.

As for Fish, it's all about simplicity. No need to pipe `echo` output to `sed` when `string replace` can edit in-place or work with variables directly.

## See Also
## Дивіться також
- Fish documentation on string manipulation: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- A guide to regular expressions: [regular-expressions.info](https://www.regular-expressions.info/)
- POSIX shell scripting basics: [gnu.org/software/bash/manual/bash.html#Shell-Scripts](https://www.gnu.org/software/bash/manual/bash.html#Shell-Scripts)
