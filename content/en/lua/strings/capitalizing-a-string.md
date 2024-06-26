---
date: 2024-02-03 19:02:30.929223-07:00
description: "How to: Lua does not have a built-in function for capitalizing strings,\
  \ but you can easily accomplish this task using basic string manipulation functions.\u2026"
lastmod: '2024-03-13T22:45:00.190259-06:00'
model: gpt-4-0125-preview
summary: Lua does not have a built-in function for capitalizing strings, but you can
  easily accomplish this task using basic string manipulation functions.
title: Capitalizing a string
weight: 2
---

## How to:
Lua does not have a built-in function for capitalizing strings, but you can easily accomplish this task using basic string manipulation functions. Here's a simple function to capitalize the first letter of a single word:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Output: Hello
```

To capitalize each word in a sentence, you can split the sentence into words, capitalize each one, and then rejoin them:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Output: Hello World From Lua
```

If you're working on a project where performance is key and you find yourself needing more advanced string manipulation capabilities, consider using a third-party library like `Penlight`. Penlight enhances Lua with more versatile string handling functions, among other utilities:

```lua
-- Assuming Penlight is installed:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Output: Hello lua users

-- Note: Penlight's capitalized function only capitalizes the first word.
-- For capitalizing each word, you'd still implement a custom solution or explore other libraries.
```
