---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text involves finding a specific sequence of characters in a block of text and substitifying it with a new constellation of characters. It's a common task in any software development process - used for refactoring code, manipulating data, fixing bugs and more.

## How to?

Lua provides an in-built function `gsub` for search and replace operations. Let's learn it via examples:

```Lua
s = "Lua is great"
new_s = string.gsub(s, "great", "awesome")
print(new_s)
```

The output:

```Lua
Lua is awesome
```

The `gsub` function takes in a string, a pattern for search, and the replacement string. The function returns the new string with replacements made. 

In cases where you need to replace multiple substrings, you'll call `gsub` again:

```Lua
s = "Lua is great, Lua is powerful"
new_s = string.gsub(s, "Lua", "Python")
print(new_s)
```

Output:

```Lua
Python is great, Python is powerful
```

## Deep Dive

`gsub` stands for 'Global Substitution'. It traces back to Unix's `ed` command line editor which had a `g/re/p` command (global/regular expression/print) that influenced many text-manipulating tools today.

Alternatives to `gsub` in Lua include: manually parsing and replacing the string with loops and conditional statements (not recommended due to complexity); or using LPEG (Lua Parsing Expression Grammars), a powerful pattern-matching tool built using Parsing Expression Grammars (PEGs).

As for `gsub`, it relies on regex-like patterns to locate strings for replacement. But beware, Lua's pattern matching is not as robust as full-fledged regex libraries in languages like Python or JavaScript.

## See Also

Learn more on Lua's string operations in the official Lua documentation: https://www.lua.org/manual/5.4/manual.html#6.4

Unearth more about parsing in Lua using LPEG: http://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html

Dive further into the `gsub` function and Lua's pattern matching: https://www.lua.org/pil/20.2.html