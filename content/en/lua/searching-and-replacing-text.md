---
title:                "Searching and replacing text"
html_title:           "Lua recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a fundamental task in programming. It involves finding a specific set of characters in a string and replacing it with another set of characters. Programmers do this to make changes to their code efficiently and quickly. For example, if you want to change a variable name in multiple places, you can use a search and replace function to avoid manually changing each occurrence.

## How to:

```Lua
-- To perform a simple search and replace:
local my_string = "Hello World"
local new_string = string.gsub(my_string, "World", "Universe")
print(new_string) -- Output: "Hello Universe"

-- To make a case-insensitive search and replace:
local my_string = "hello world"
local new_string = string.gsub(my_string, "WORLD", "Universe", 1, true) -- The 1 limits the replacements to one occurrence
print(new_string) -- Output: "hello Universe"

-- To use a replacement function:
local my_string = "Hello World"
local new_string = string.gsub(my_string, "%w+", function(match) return match:upper() end)
print(new_string) -- Output: "HELLO WORLD" (all words capitalized)
```

## Deep Dive:

Searching and replacing text has been a programming concept since the early days of computing. In the past, it was often done manually, taking hours or even days to make changes to a large program. However, with the advancement of technology, specialized functions like `gsub` have been developed to make this task more efficient.

Alternatives to using `gsub` include using a text editor with a find and replace function or using regular expressions. In Lua, the `re` library can be used for advanced regular expression matching and replacing.

Implementations of `gsub` vary depending on the programming language and its specific syntax and options. In Lua, `gsub` can accept a fourth and fifth argument, allowing for limiting the number of replacements and case-insensitive matching.

## See Also:

- [Lua string library documentation for `gsub`](https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub)
- [Lua Re (regular expression) library](https://github.com/rrthomas/lua-re)