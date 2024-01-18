---
title:                "Capitalizing a string"
html_title:           "Lua recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string refers to converting the first letter of each word in a string to uppercase. This is commonly done in programming to improve readability and consistency. It can also be helpful when comparing strings, as capitalization can affect the outcome of the comparison.

## How to:

To capitalize a string in Lua, we can use the `string.upper()` function. This function takes a string as input and returns a new string with all characters converted to uppercase.

```Lua
-- Example 1
local str = "hello world"
print(string.upper(str)) -- Output: HELLO WORLD

-- Example 2
local name = "john smith"
local capitalizedName = string.upper(name)
print(capitalizedName) -- Output: JOHN SMITH
```

In the first example, we simply pass the string "hello world" as an argument to `string.upper()` and it returns the capitalized version. In the second example, we store the capitalized string in a variable and then print it.

## Deep Dive:

Capitalizing strings has been a common convention in programming for decades. In the early years of computing, computers were not able to distinguish between uppercase and lowercase letters, so using only uppercase letters made it easier for them to process text. Even though modern computers can handle both cases, it has become a standard practice to capitalize strings for better readability and consistency among different languages.

An alternative to using `string.upper()` is the `string.gsub()` function. This function allows us to replace specific characters in a string with new characters. We can use it to replace the first character of each word with its uppercase version. However, this method can be more tedious and not as efficient.

It is worth noting that `string.upper()` only converts the characters to uppercase according to the current locale. This means that it will not necessarily convert all characters to their uppercase equivalent, as different languages have different lowercase and uppercase letters.

## See Also:

To learn more about string manipulation in Lua, check out the official Lua documentation on strings: https://www.lua.org/manual/5.3/manual.html#6.4 

For more information on Lua's `string.upper()` function, visit: https://www.lua.org/manual/5.3/manual.html#pdf-string.upper