---
date: 2024-01-20 17:46:22.962303-07:00
description: "Extracting substrings means pulling out specific parts of a string based\
  \ on their position. Programmers do it to analyze, manipulate, or validate pieces\u2026"
lastmod: '2024-02-25T18:49:46.960365-07:00'
model: gpt-4-1106-preview
summary: "Extracting substrings means pulling out specific parts of a string based\
  \ on their position. Programmers do it to analyze, manipulate, or validate pieces\u2026"
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Extracting substrings means pulling out specific parts of a string based on their position. Programmers do it to analyze, manipulate, or validate pieces of data within a larger string.

## How to: (Як це зробити:)

In Lua, you can extract substrings using the `string.sub` function.

```Lua
local text = "Привіт, як справи?"
local part = string.sub(text, 1, 7)
print(part)  -- Outputs: Привіт,
```

Sample output:
```
Привіт,
```

You can also work with negative indices to count from the end of the string.

```Lua
local text = "Привіт, як справи?"
local part = string.sub(text, -6, -2)
print(part)  -- Outputs: справ
```

Sample output:
```
справ
```

## Deep Dive (Поглиблений Аналіз)

Lua supports various string manipulation functions; `string.sub` is one among them and has been a part of the language from its early versions. It's a straightforward and efficient way to interact with strings.

Alternatives include using `string.match` with patterns. This method offers more flexibility for complex substring extractions.

```Lua
local text = "Привіт, як справи?"
local part = string.match(text, "як")
print(part)  -- Outputs: як
```

Implementation-wise, Lua strings are immutable, meaning once created, they can't be changed. When you extract a substring, Lua creates a new string rather than altering the original.

## See Also (Додатково)

- Lua `string` library reference: [Programming in Lua](https://www.lua.org/pil/20.1.html)
- Lua patterns guide: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
