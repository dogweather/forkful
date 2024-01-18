---
title:                "Extracting substrings"
html_title:           "Lua recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the process of retrieving a part of a string, or a sequence of characters, from a larger string. This allows programmers to work with specific portions of a string, rather than the entire string at once. It is commonly used in text processing, parsing, and data manipulation.

## How to:
To extract substrings, the `string.sub` function can be used in Lua. It takes in three arguments: the string to extract from, the starting index, and the optional ending index. The starting index is the position of the first character to be included in the extracted substring. The optional ending index is the position of the last character to be included, and if not specified, it defaults to the end of the string. Here is an example of extracting a substring from the string "Hello World":
```Lua
local str = "Hello World"
print(string.sub(str, 7)) --> Outputs "World"
print(string.sub(str, 1, 5)) --> Outputs "Hello"
```

## Deep Dive:
The concept of extracting substrings has been around since the early days of programming, and it is commonly used in many programming languages, including Lua. In Lua, there are a few alternative ways to extract substrings, such as using a combination of `string.find` and `string.sub` or using pattern matching with `string.match`.

The `string.sub` function also has an interesting implementation detail. In Lua, strings are indexed starting from 1, unlike many other programming languages where indexing starts at 0. This means that the first character of a string has an index of 1, and the last character has an index of the string's length. So when using `string.sub`, the starting and ending indexes follow this convention as well.

## See Also:
- [Official Lua Documentation for string.sub](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Tutorial on Lua Substring Extraction](https://www.guru99.com/lua-string-library.html#11)
- [Wikipedia article on Substring](https://en.wikipedia.org/wiki/Substring)