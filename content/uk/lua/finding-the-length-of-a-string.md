---
title:                "Визначення довжини рядка"
date:                  2024-01-20T17:47:57.245011-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що та Навіщо?
Finding the length of a string means getting the number of characters it contains. Programmers do this to validate input, process text data, or just to manage string data efficiently.

## How to:
## Як це зробити:
```Lua
-- Basic usage
local str = "Привіт"
print(#str)  -- Output: 6

-- Storing length in a variable
local len = #str
print(len)   -- Output: 6

-- Working with strings in tables
local greetings = {"Привіт", "Hello", "Bonjour"}
for _, greeting in ipairs(greetings) do
    print(#greeting)
end
-- Output:
-- 6
-- 5
-- 7
```

## Deep Dive
## Поглиблений Аналіз
In older programming languages, strings were different and getting their length was not this straightforward. Lua uses a simple `#` operator to find a string's length. It counts the number of bytes, which, for UTF-8 encoded strings, may differ from the expected character count. It's a direct operation with no extra frills — fast and memory-efficient.

If you're dealing with multi-byte characters, as in UTF-8, you may need a different approach to count actual characters since the `#` operator may not give accurate results. In those cases, functions from libraries like 'luautf8' can help:

```Lua
local utf8 = require('lua-utf8')
print(utf8.len("Привіт"))  -- Output: 6
```

This library handles multi-byte characters correctly. Implementation-wise, finding the length of a string is a matter of iterating over the string and counting elements, which languages handle differently. In Lua, simplicity is key, keeping the language approachable and code readable.

## See Also
## Дивіться також
For more on strings in Lua:
- The official Lua documentation on strings: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua Users Wiki on strings: http://lua-users.org/wiki/StringLibraryTutorial

UTF-8 and string manipulation in Lua:
- lua-utf8 library: https://github.com/starwing/luautf8
- Understanding UTF-8 and strings in Lua: http://lua-users.org/wiki/LuaUnicode