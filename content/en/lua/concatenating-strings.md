---
title:                "Concatenating strings"
html_title:           "Lua recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in Lua means combining multiple strings into one. Programmers do this to create larger and more complex strings that can then be used for various purposes such as outputting information or building user interfaces.

## How to:

To concatenate strings in Lua, use the `..` operator between two or more strings. Here's an example:

```Lua
-- Define two strings
local str1 = "Hello "
local str2 = "world!"

-- Concatenate the strings
local result = str1 .. str2

-- Output the result
print(result)
```

The output of the above code will be `Hello world!`.

## Deep Dive:

- Historical Context: Concatenation comes from the Latin word "concatenare" meaning "to link together". In early programming languages, concatenating strings was a complex and tedious process, but with Lua, it has become much easier and more efficient.

- Alternatives: In Lua, there are other ways to combine strings such as using the `string.format` function or the `gsub` function. However, using the `..` operator is the most common and efficient method.

- Implementation Details: In Lua, strings are immutable, meaning they cannot be changed. So, when strings are concatenated, a new string is created instead of modifying the existing ones. To avoid creating unnecessary strings, it is recommended to use the `table.concat` function when working with large amounts of data.

## See Also:

- [Lua String Manipulation](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Learn Lua in 15 Minutes](https://learnxinyminutes.com/docs/lua/)
- [Lua Concatenation Tutorial](https://www.tutorialspoint.com/lua/lua_concatenation.htm)