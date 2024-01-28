---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex, are patterns used to match character combinations in text. Programmers use them for searching, editing, and manipulating strings because they're powerful and efficient.

## How to:
Lua provides basic support for patterns (its version of regex) that you can use with string-matching functions. Here's a quick spin:

```Lua
local text = "Hello Lua! 123"
-- Find digits in the text
local pattern = "%d+"
for match in string.gmatch(text, pattern) do
    print(match)
end
```
Output:
```
123
```

To replace text:

```Lua
local text = "Hello Lua! 123"
local pattern = "%d+"
local replacement = "456"
local new_text = string.gsub(text, pattern, replacement)

print(new_text)
```
Output:
```
Hello Lua! 456
```

## Deep Dive
Lua's patterns are not as feature-rich as regex found in other languages but they're fast and cover many common use cases. They were introduced as a lightweight solution to string matching, avoiding the complexity of traditional regex implementations.

Alternatives include external Lua modules like `rex_pcre` or `lpeg`, which provide more complete regex implementations or different pattern-matching paradigms, respectively.

Lua's pattern-matching functions, like `string.find`, `string.match`, `string.gmatch`, and `string.gsub`, work with predefined pattern codes like `%d` for digits, `%s` for space characters, and `%a` for letters, making implementation straightforward with less overhead than full regex engines.

## See Also
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4.1)
