---
date: 2024-01-20 17:47:14.554390-07:00
description: "Finding the length of a string means counting the number of characters\
  \ in it. Programmers do it to validate input, size buffers, or loop through\u2026"
lastmod: '2024-03-11T00:14:34.341057-06:00'
model: gpt-4-1106-preview
summary: "Finding the length of a string means counting the number of characters in\
  \ it. Programmers do it to validate input, size buffers, or loop through\u2026"
title: Finding the length of a string
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means counting the number of characters in it. Programmers do it to validate input, size buffers, or loop through characters.

## How to:
Here's how to get a string's length in Fish:

```Fish Shell
set my_string "Hello, World!"
echo (string length "$my_string")
```

Output:

```
13
```

## Deep Dive
In Fish, unlike some other shells, `string length` is a built-in function making it native and efficient. Historically, other shells might have required more verbose syntax or external tools like `expr` or `wc`. Fish simplifies tasks with its robust string handling functions, where `string length` directly gives the count of Unicode characters, which isn't always equal to the byte count, especially for non-ASCII characters.

Alternatives for string length determination in shells before the `string` function in Fish could be less reliable because they didn't always account for multibyte characters. Implementation wise, `string length` counts Unicode graphemes, which is important for texts containing characters that combine with others to form a single visual unit.

## See Also
- Fish documentation on string manipulation: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Unicode Standard for understanding graphemes: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
