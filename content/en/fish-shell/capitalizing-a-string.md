---
title:                "Capitalizing a string"
html_title:           "Fish Shell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means transforming every initial letter in a string to uppercase. Programmers do this for textual uniformity, readability or to meet specific coding standards.

## How to:

Fish, surprisingly elegant, has built-in functions for string manipulations. Check how to capitalize a string by utilizing `string upper`:

```fish
set string "the quick brown fox jumps over the lazy dog"
printf "%s" $string | string split " " | string upper | string join " " 
```

Output: 

```fish
THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
```
Voila! Each word in your string is now capitalized!

## Deep Dive

Historically, string capitalization has been handled in many different ways. Some languages (like JavaScript) have a direct method (toUppercase), others (like Python) use the capitalize function. 

In Fish, `string upper` is our go-to for capitalization, but there's an alternate, more cumbersome way: manually using ASCII values of letters. Also worth noting: Fish's `string upper` capitalizes every word in a string, and not just the initial character. 

The versatility of Fish's string manipulation functions demonstrates its power. In other scripting languages, achieving the same result might require verbose code or importing additional libraries.

## See Also

- Full documentation for Fish String functions: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- An article comparing Fish's capabilities to other shells: [https://jamsek.dev/posts/fish-shell-vs-zsh/](https://jamsek.dev/posts/fish-shell-vs-zsh/)
- ASCII Table for manual capitalization: [http://www.asciitable.com/](http://www.asciitable.com/)