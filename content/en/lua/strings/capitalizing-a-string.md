---
date: 2024-02-03 19:02:30.929223-07:00
description: "Capitalizing a string involves modifying the first character of each\
  \ word in a sentence to be uppercase, while ensuring the rest are lowercase. This\u2026"
lastmod: 2024-02-19 22:05:18.652375
model: gpt-4-0125-preview
summary: "Capitalizing a string involves modifying the first character of each word\
  \ in a sentence to be uppercase, while ensuring the rest are lowercase. This\u2026"
title: Capitalizing a string
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string involves modifying the first character of each word in a sentence to be uppercase, while ensuring the rest are lowercase. This technique is commonly used to format text for more professional or readable output, such as preparing titles or user input for display.

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
