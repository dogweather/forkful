---
title:                "Concatenating strings"
aliases:
- en/lua/concatenating-strings.md
date:                  2024-01-20T17:35:08.769721-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings means sticking them together end-to-end to make a new one. Programmers do it to build up text dynamically, like creating messages or generating code.

## How to:
In Lua, you concatenate strings with the `..` operator. Let's see it in action:

```lua
local hello = "Hello, "
local world = "world!"
local greeting = hello .. world

print(greeting)  -- Output: Hello, world!
```

You can even tack on numbers with a little coercion:

```lua
local base = "I have "
local itemCount = 3
local message = base .. itemCount .. " apples"

print(message)  -- Output: I have 3 apples
```

Remember, conversion of non-string types is manual:

```lua
local score = 9001
local displayScore = "Your score is: " .. tostring(score)

print(displayScore)  -- Output: Your score is: 9001
```

## Deep Dive
String concatenation might seem mundane, but it's vital. In Lua's early days, it was intended for embedded systems, which meant keeping things light. That's why `..` was chosen for strings - it's simple yet effective.

Alternatives to `..` include:

- `table.concat` function for arrays of strings, more efficient for concatenating many strings.
- String library functions like `string.format`, offering more control over formatting.

Lua's string concatenation performance was a concern, specifically with `..` because each use creates a new string, which can be costly in loops. To mitigate this, when concatenating in a loop, use tables:

```lua
local parts = {}
for i = 1, 10 do
    parts[i] = "Part " .. i
end
local combined = table.concat(parts, ", ")

print(combined)  -- Output: Part 1, Part 2, ... Part 10
```

Internally, Lua manages strings in a hash table to optimize memory usage, so identical strings share the same storage. But, concatenation breaks this sharing because of the new strings it creates.

## See Also
- Lua's official documentation on strings: https://www.lua.org/manual/5.4/manual.html#6.4
- Programming in Lua (Book): https://www.lua.org/pil/contents.html
- String manipulation tips: https://lua-users.org/wiki/StringLibraryTutorial
