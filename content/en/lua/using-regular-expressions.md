---
title:                "Using regular expressions"
html_title:           "Lua recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions are a way of describing patterns in text, allowing you to search for specific words or strings of characters. Programmers use it to efficiently extract and manipulate data, validate input, and perform string manipulation tasks with precision and ease.

## How to:
Using regular expressions in Lua is as easy as using the built-in `string.match` function. You simply pass in a string, along with a pattern or "regular expression", and it will return the first occurrence of the pattern in the string. For example:

```Lua
local string = "Hello World"
local pattern = "l"

local result = string.match(string, pattern)
print(result) -- outputs "l"
```

You can also use regular expressions to extract specific portions of a string, by using capture groups. Let's say we want to extract the year from a date string. We can do so with the following code:

```Lua
local string = "Today's date is 02/07/2021"
local pattern = "([0-9]+)/([0-9]+)/([0-9]+)"

local month, day, year = string.match(string, pattern)
print(year) -- outputs "2021"
```

Regular expressions also have special characters that allow you to match patterns such as only numbers or only letters. For example, the pattern `%d` will match any digit and the pattern `%a` will match any letter. You can use these in combination with other characters to create more complex patterns.

## Deep Dive:
Regular expressions were first introduced by computer scientist Stephen Kleene in the 1950s, and have since become a standard tool in programming languages, including Lua. While there are other ways to perform similar tasks, such as using string manipulation functions, regular expressions offer more powerful and flexible options for pattern matching.

One alternative to using regular expressions is using Lua's built-in `string.find` function, which also allows you to search for patterns in strings, but with less control and flexibility. Another alternative is using a library specifically designed for regular expressions, such as LuaPatterns or Lrexlib.

When using regular expressions in Lua, there are a few implementation details to keep in mind. For example, patterns are case-sensitive by default, but you can use the special character `i` to make them case-insensitive. Also, escape characters such as `%` need to be written twice, as in `%%`, to be used in patterns.

## See Also:
- [Official Lua documentation on `string.match`](https://www.lua.org/manual/5.4/manual.html#pdf-string.match)
- [LuaPatterns library](https://github.com/luapatterns/luapatterns)
- [Lrexlib library](https://github.com/rrthomas/lrexlib)