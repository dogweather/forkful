---
date: 2024-01-25 20:50:17.131757-07:00
description: "Removing quotes from a string is about stripping those pesky single\
  \ (' ') or double (\" \") quotation marks off your text data. Programmers often\
  \ do this to\u2026"
lastmod: '2024-03-11T00:14:34.337475-06:00'
model: gpt-4-1106-preview
summary: "Removing quotes from a string is about stripping those pesky single (' ')\
  \ or double (\" \") quotation marks off your text data. Programmers often do this\
  \ to\u2026"
title: Removing quotes from a string
---

{{< edit_this_page >}}

## What & Why?

Removing quotes from a string is about stripping those pesky single (' ') or double (" ") quotation marks off your text data. Programmers often do this to sanitize input or prepare data for further processing without the clutter of quotes.

## How to:

Fish has built-in magic for this kind of task. Use the `string` function without breaking a sweat. Check out these spells:

```fish
# Example with single quotes
set quoted "'Hello, World!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Output: Hello, World!

# Same deal with double quotes
set double_quoted "\"Hello, Universe!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Output: Hello, Universe!
```

## Deep Dive

Back in the command-line stone age, you'd wrestle with `sed` or `awk` to strip quotes; a real tangle of backslashes and cryptic flags. Fish's `string` function is from a newer era, making code cleaner and more intuitive.

Alternatives in other shells might still rely on these old tools or might use their own built-in methods like bash's parameter expansion or zsh's modifiers.

The `string` function goes beyond trimming quotes. It's a Swiss Army knife for string operations in Fish. With `string`, you can slice, dice, split, join, or even regex-match strings right in your terminal.

## See Also

Dive deeper into `string` with the help of the official documentation:
- [Fish Shell String Documentation](https://fishshell.com/docs/current/commands.html#string)

For nostalgia or when scripting with more traditional shells, check out:
- [Sed & Awk Guide](https://www.grymoire.com/Unix/Sed.html)
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
