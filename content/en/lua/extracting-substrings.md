---
title:                "Extracting substrings"
aliases:
- en/lua/extracting-substrings.md
date:                  2024-01-20T17:46:13.109202-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means pulling out a specific chunk of a string. Programmers do it to isolate, analyze, or manipulate specific data within a larger text.

## How to:
In Lua, use the `string.sub` function:

```lua
local text = "Hello, Lua!"
-- Extract 'Hello'
print(string.sub(text, 1, 5)) -- Output: Hello

-- Grab 'Lua'
print(string.sub(text, 8, 11)) -- Output: Lua
```

Or get the last characters with negative indices:

```lua
-- Snag 'Lua!' from the end
print(string.sub(text, -4)) -- Output: Lua!
```

Use patterns to find and extract:

```lua
local phrase = "The quick brown fox jumps"
-- Match and extract 'quick'
print(phrase:match("(%a+) quick")) -- Output: The
```

## Deep Dive
In early programming, string handling was manual and clunky, often needing loops and conditionals. Lua's `string.sub` is part of its richer string library, making string manipulation a breeze. Alternatives to `string.sub` include pattern matching with `string.match`, which is more powerful but can be overkill for simple tasks.

The `string.sub` and pattern matching are based on C functions due to Lua's C roots. You won't find a vast standard library in Lua for strings compared to languages like Python; it sticks to essentials, valuing simplicity and efficiency. Remember, indices in Lua start at 1, not 0.

## See Also
- Lua 5.4 Reference Manual on Strings: [www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- 'Programming in Lua' (4th edition), especially the chapter on strings: [www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
