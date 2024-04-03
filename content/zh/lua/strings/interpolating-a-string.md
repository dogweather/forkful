---
date: 2024-01-20 17:51:11.914702-07:00
description: "String interpolation is replacing placeholders within a string with\
  \ actual values, a common practice for crafting messages or assembling texts\u2026"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.896220-06:00'
model: gpt-4-1106-preview
summary: String interpolation is replacing placeholders within a string with actual
  values, a common practice for crafting messages or assembling texts dynamically.
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## How to: (如何操作：)
Lua doesn't have built-in string interpolation, but we can get the job done nicely with some creativity. Here's how:

```Lua
-- Using string.format
local name = "Xiao Ming"
local age = 25
local greeting = string.format("Hello, %s! You are %d years old.", name, age)
print(greeting)  -- Output: Hello, Xiao Ming! You are 25 years old.
```

You can also concatenate strings directly:

```Lua
-- Direct concatenation
local greeting = "Hello, " .. name .. "! You are " .. age .. " years old."
print(greeting)  -- Same output as above
```

## Deep Dive (深入了解)
Historically, Lua never aimed for string interpolation within the language itself, and that's partly why `string.format` is the go-to option—it supports a variety of formats, following the C `printf` style.

However, there are some makeshift alternatives:

1. **Using the gsub function**:
```Lua
local template = "Hello, ${name}! You are ${age} years old."
local interpolated = template:gsub('%${(%w+)}', {name="Xiao Ming", age=25})
print(interpolated)  -- Output mirrors the above
```
2. **Custom interpolation function**:
```Lua
function interpolate(s, tab)
  return (s:gsub('($%b{})', function(w) return tab[w:sub(3, -2)] or w end))
end

local message = interpolate("Hello, ${name}! You are ${age} years old.", {name="Xiao Ming", age=25})
print(message)  -- Identical to previous results
```

Each method has its pros and cons, including readability and performance implications. Choose based on your need for speed or clarity.

## See Also (另请参阅)
Here are some resources that might help you deepen your understanding and efficiency with strings in Lua:

- [Lua 5.4 Reference Manual - Strings](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Programming in Lua (4th edition) - Section 20.1, String Library](https://www.lua.org/pil/20.1.html)

Using these sources, you'll solidify the concepts and get inventive with your solutions. Happy coding!
