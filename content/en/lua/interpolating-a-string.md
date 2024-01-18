---
title:                "Interpolating a string"
html_title:           "Lua recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string in Lua refers to the process of inserting a variable's value into a string. This allows programmers to dynamically construct strings using variables, making code more flexible and readable.

## How to:

To interpolate a string in Lua, use the `string.format()` function. This function takes a format string and a list of arguments, replacing any formatting placeholders in the format string with the corresponding values from the arguments list. For example:

```Lua
local name = "John"
local age = 25

local intro = string.format("Hello, my name is %s and I am %d years old.", name, age)
print(intro)
```

This will output `"Hello, my name is John and I am 25 years old."` By using `%s` and `%d` in the format string, we tell `string.format()` where to insert the values of `name` and `age`.

You can also use numbered placeholders in the format string to specify the order in which the arguments are used. For example:

```Lua
local num1 = 10
local num2 = 5

local result = string.format("%2$d + %1$d = %d", num1, num2, num1 + num2)
print(result)
```

This will output `"5 + 10 = 15"`, with `num1 + num2` being the third argument and thus using the third placeholder in the format string.

## Deep Dive:

Interpolating strings is not a new concept and has been used in programming languages like C and Python for a long time. The `string.format()` function in Lua is based on the `sprintf()` function from the C standard library.

Alternatives to interpolating strings in Lua include using string concatenation and the `string.gsub()` function. However, using `string.format()` is generally considered the most efficient and readable method.

Internally, `string.format()` uses a formatting string similar to the one used in C, with `%` representing a placeholder and a letter indicating the data type of the argument. For example, `%s` for strings and `%d` for integers.

## See Also:

- Lua string library: https://www.lua.org/manual/5.3/manual.html#6.4
- C sprintf() function: https://www.cplusplus.com/reference/cstdio/sprintf/
- Python string formatting: https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting