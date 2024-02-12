---
title:                "Interpolating a string"
aliases: - /en/lua/interpolating-a-string.md
date:                  2024-01-20T17:51:14.356597-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you insert variables directly into strings. It's done to dynamically build strings and keep code clean.

## How to:
In Lua, use `..` for concatenation or `string.format` for interpolation. Example:
```Lua
local name = "Ada"
local greeting = "Hello, " .. name .. "!"
print(greeting) -- Output: Hello, Ada!

local age = 30
local bio = string.format("%s is %d years old.", name, age)
print(bio) -- Output: Ada is 30 years old.
```

## Deep Dive
Historically, Lua lacked built-in string interpolation, unlike some other languages (e.g., Ruby, Python). Concatenation with `..` was the go-to way. Lua 5.3 introduced `string.format` for a cleaner approach, similar to C's `printf`. **Alternatives:** Besides using the `..` operator or `string.format`, you can also write a custom interpolation function that uses gsub for pattern matching. But why complicate things? Use built-in tools for maintainability. **Implementation Details:** Be aware that frequent string concatenation can lead to performance issues. `string.format` is helpful when you need formatting control, like specifying number precision or padding.

## See Also
- Lua Manual on Strings: http://www.lua.org/manual/5.4/manual.html#6.4
- 'Programming in Lua' on Strings: https://www.lua.org/pil/20.1.html
- Lua-users Wiki on Strings: http://lua-users.org/wiki/StringLibraryTutorial
