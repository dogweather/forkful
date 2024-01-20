---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the task of grabbing specific pieces, or "substrings," from a larger piece of text or "string." Programmers do it to manipulate, parse, or otherwise examine bytes of textual data.

## How to:

Lua uses the `string.sub` function to extract a substring from a string. The function accepts three parameters: the string and the start and end positions of the substring.

Here's how to extract 'world' from 'Hello, world!':

```Lua
s = 'Hello, world!'
print(string.sub(s, 8, 12))  -- Output: world
```

It also works with negative indices, which count from the end of the string:

```Lua
s = 'Hello, world!'
print(string.sub(s, -6, -2))  -- Output: world
```

## Deep Dive

In historical context, Lua has always embraced the simplicity of string manipulation. This is reflected in the straightforward functionality of `string.sub`.

An alternative to `string.sub` is `string.match`. It extracts the substring that matches a pattern:

```Lua
s = 'Hello, world!'
print(string.match(s, 'world'))  -- Output: world
```
However, `string.match` provides flexibility to extract more complex patterns, making it a heavier tool to use.

From an implementation perspective, Lua optimises the `string.sub` function to avoid copying strings when possible. This optimisation has made Lua a preferred language for string handling tasks.

## See Also

- Official Lua 5.4 Manual: [String Manipulation](http://www.lua.org/manual/5.4/manual.html#6.4)
- An Introduction to Lua: [The String Library](https://tylerneylon.com/a/learn-lua/)
- Lua-users: [String Library Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)