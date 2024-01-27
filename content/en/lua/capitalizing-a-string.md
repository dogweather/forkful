---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means making the first letter of each word uppercase. Programmers do it for formatting consistency, user readability, or data normalization.

## How to:

Lua doesn't have a built-in capitalization function, so let's create one:

```lua
function capitalize(str)
  return (str:gsub("(%l)(%w*)", function(first, rest) return first:upper()..rest end))
end

print(capitalize("hello world"))  -- Output: Hello World
```

## Deep Dive

Capitalization functions are standard in many programming languages. In Lua, we make one using `string.gsub()`, a powerful pattern-matching function. Our custom `capitalize` function uses a pattern to find lowercase letters (`%l`) followed by zero or more word characters (`%w*`), and replaces them with the uppercase letter and the rest of the word.

```lua
-- Here's another way to just capitalize the first word
function capitalizeFirst(str)
  if str == "" then return str end
  return str:sub(1, 1):upper()..str:sub(2)
end
```

Lua's pattern matching capabilities are less powerful than full regular expressions but are suitable for many string manipulation tasks. Note that our `capitalize` function won't capitalize words following certain punctuation marks, so it's not foolproof. For more robust solutions, you may consider additional pattern matching or external libraries.

Historically, the need for capitalization functions arose from the desire to present text data uniformly, especially in user interfaces. However, care must be taken to understand the context: different languages and cultures have their own rules for capitalization beyond simply the first letter of a sentence or name.

## See Also

- Lua `string` library: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua Patterns: https://www.lua.org/pil/20.2.html
- Text Processing in Lua: https://www.lua.org/pil/20.html
