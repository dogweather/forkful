---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings involves connecting two or more strings into one, akin to joining sentences. It is essential for programmers to make informative outputs, weave together sentences, format data, or build command strings.

## How to:

String concatenation in Lua uses the `..` operator. Here's a straightforward example.

```Lua
string1 = "Hello, "
string2 = "world!"
concatenated_string = string1 .. string2
print(concatenated_string)
```

The output will be:

```Lua
Hello, world!
```

Notice that we had to include a space after "Hello," in the string1 to get the right output format. Otherwise, there would be no space between "Hello," and "world!".

## Deep Dive

Historically, Lua's simplicity was geared for embedding in applications. It's still the case with its '..' operator for concatenation, providing an alternative to '+' which usually means addition in most programming languages.

Alternatives to concatenation in Lua are functions like 'string.format' and 'table.concat'. 'string.format' is similar to printf in C, and 'table.concat' deals with concatenation of table elements.

From an implementation perspective, string concatenation can be slightly costly, especially when done in large volumes. Lua creates a new string when two strings are concatenated, as Lua strings are immutable, similar to Python and Java. Efficient concatenation can be achieved through StringBuffer pattern (like in Java) or table-structured strings to avoid constantly creating new strings.

## See Also

For more in-depth knowledge, refer to the official Lua documentation:
- String usage: https://www.lua.org/pil/2.5.html
- Lua's functions: https://www.lua.org/pil/2.4.html
- Efficient string concatenation: https://www.lua.org/gems/sample.pdf