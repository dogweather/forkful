---
title:                "Finding the length of a string"
html_title:           "Arduino recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

In Lua programming, finding the length of a string simply means figuring out how many characters are in the string. This is handy when manipulating text — say, matching patterns, slicing, or looping through.

## How to:

Use the `string.len()` function or the `#` operator to find the length of a string.

```Lua
s = "Hello, Lua!"
print(string.len(s)) -- Output: 12
print(#s) -- Output: 12
```

Both the function and the operator spit out the same result.

## Deep Dive

Determining the length of a string has deep roots, and not just in Lua. It's available in some way in nearly all programming languages — essential for many text-based operations.

In Lua, you've got two alternatives: the `string.len()` function or the `#` operator. Under the hood, both techniques are essentially equivalent. They use the same memory and time complexity — O(1).

It's worth noting that these methods do not work for tables and functions, only for strings and tables that behave like lists.

## See Also:

1. Lua Basics: [Lua Strings](http://www.lua.org/pil/2.4.html)
2. Lua String API: [Strings Manipulation](https://www.lua.org/manual/5.3/manual.html#6.4)
3. More on Length Operator: [Uses and Limitations](https://www.lua.org/manual/5.3/manual.html#3.4.7)