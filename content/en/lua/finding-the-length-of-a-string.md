---
title:                "Finding the length of a string"
html_title:           "Lua recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means determining the number of characters in a given string. This is a common task in programming, as it allows developers to manipulate and analyze text data efficiently.

## How to:
To find the length of a string in Lua, we can use the `string.len()` function. Let's say we have a string variable called `name` with the value "John." We can find its length like this:
```
Lua
local name = "John"
print(string.len(name))
```
The output would be `4`, as our string has four characters.

## Deep Dive:
The `string.len()` function has been a part of the Lua standard library since its initial release in 1993. It is available in most programming languages and follows a similar syntax. However, in some languages, it is implemented as a method instead of a function, such as `name.length()`.

If we need to find the length of a string without using `string.len()`, we can also use a for loop to iterate through the string and count the number of characters. However, using the built-in function is a more efficient and straightforward approach.

It's worth noting that the length of a string in Lua is not limited to just characters. It can also include any other type of data, such as numbers or symbols.

## See Also:
To learn more about the `string.len()` function and other string manipulation techniques in Lua, check out the Lua documentation: https://www.lua.org/manual/5.4/manual.html#6.4

For a comparison of string length implementations in other programming languages, check out this Stack Overflow thread: https://stackoverflow.com/questions/644952/increase-pure-loadstring-evaluation-speed