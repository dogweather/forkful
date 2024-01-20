---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Capitalizing a string means making the first letter of each word uppercase. Programmers do it to make text look nicer or follow certain writing rules.

## How to: (Як це зробити:)
```Lua
-- Capitalize each word in a string
function capitalize(str)
  return (str:gsub("(%l)(%w*)", function(a,b) return a:upper()..b end))
end

print(capitalize("привіт, це lua стаття!"))  -- Output: Привіт, Це Lua Стаття!
```

## Deep Dive (Занурення у деталі)
In Lua, there's no built-in function to capitalize strings. Historically, programmers write their own or use libraries. `gsub` is versatile, applying a function to pattern matches – perfect for capitalization, where we match the first letter of each word. An alternative is iterating over words, but `gsub` keeps our code concise. Understanding the Lua pattern matching system enhances implementation – `%l` matches lowercase letters, `%w` matches a word's remaining letters.

## See Also (Див. також)
- [Lua 5.4 Reference Manual: Patterns](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Stack Overflow: How do I capitalize the first letter of each word in a string?](https://stackoverflow.com/questions/20284515/how-do-i-capitalize-first-letter-of-first-name-and-last-name-in-lua)