---
title:                "Converting a string to lower case"
html_title:           "Lua recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case in Lua simply means changing all the capital letters in a string to their lowercase counterparts. This can be helpful for data validation, string comparisons, and formatting consistency. Programmers often do this to ensure data integrity and efficient coding.

## How to:

To convert a string to lower case in Lua, we can use the `string.lower()` function. Let's see how it works with an example:

```Lua
local string = "HeLLo WoRLd"
print(string.lower(string))
```

This code will output `"hello world"`, with all the letters in lowercase.

## Deep Dive

Historically, the `string.lower()` function was introduced in Lua 5.1 and has remained a standard function since then. However, there are alternative methods of converting a string to lowercase, such as using the `string.gsub()` function with a pattern, or using the `string.byte()` and `string.char()` functions.

It's worth noting that the `string.lower()` function only works with ASCII characters, so it may not produce the desired result for strings with non-English characters. In that case, using the `string.gsub()` function with a Unicode pattern might be a better choice.

## See Also

To learn more about string manipulation in Lua, check out the [official Lua documentation](https://www.lua.org/pil/20.2.html) or this [tutorial on string manipulation](https://riptutorial.com/lua/example/9680/string-manipulation) in Lua. Additionally, you can explore the different functions available for string manipulation in the [Lua strlib library](https://www.lua.org/manual/5.3/manual.html#6.4).