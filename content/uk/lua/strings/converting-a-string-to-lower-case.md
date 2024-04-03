---
date: 2024-01-20 17:38:52.480864-07:00
description: ''
lastmod: '2024-03-13T22:44:49.479794-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## What & Why?
## Що і чому?

Converting a string to lower case means changing all uppercase letters in a text to their lowercase counterparts. Programmers do this for consistency in text processing, such as case-insensitive comparisons or search.

## How to:
## Як це зробити:

```Lua
local text = "Привіт, Як Справи?"
local lowercased_text = text:lower()

print(lowercased_text)  -- outputs: "привіт, як справи?"
```

## Deep Dive
## Занурення

Lua uses the function `:lower()` to convert text. Introduced in early versions, it's been a straightforward feature without major changes. Alternatives include manually iterating over characters and converting them, but this is needlessly complex given Lua's built-in function. Underneath, Lua taps into the C language's character handling functions, ensuring speed and reliability across different systems. Note, however, Lua's string manipulation doesn't account for locale-specific rules. It works fine with basic Latin characters, but be cautious with Unicode strings, as results may vary.

## See Also
## Дивіться також

- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua string manipulation: https://www.lua.org/pil/20.html
- Unicode considerations in Lua: https://www.lua.org/manual/5.4/manual.html#6.5
