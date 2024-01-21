---
title:                "Finding the length of a string"
date:                  2024-01-20T17:48:09.567762-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means figuring out how many characters it contains. Programmers do this to validate input, manipulate text, or simply count characters for various tasks.

## How to:

In Lua, you grab the length of a string with the `#` operator. Simple and snappy.

```lua
local myString = "Hello, Lua!"
print(#myString)  -- Output: 11
```

What if your string has newline characters or is empty?

```lua
local stringWithNewline = "Hello\nLua!"
local emptyString = ""
print(#stringWithNewline)  -- Output: 10
print(#emptyString)         -- Output: 0
```

Even with newlines, Lua counts each character. And yes, an empty string is 0 long.

## Deep Dive

Back in the day, strings in some languages were trickier. You might have needed functions or methods to get a string's length. Today, in Lua, it's as direct as using the `#` operator. 

Alternatives? If you're dealing with Unicode characters, the `#` operator might trip up with multi-byte characters. In that case, you'd explore libraries like `utf8`. Lua 5.3 onwards introduced this built-in library.

```lua
local unicodeString = "こんにちは" -- That's "Hello" in Japanese
print(#unicodeString)  -- Output might be surprising if you're not ready for multibyte characters!
print(utf8.len(unicodeString))  -- Output: 5 characters as expected
```

A detail worth noting: Lua keeps strings immutable and internally reused through a mechanism called string interning. This is neat because it saves memory and makes string length operations fast.

## See Also

- Lua 5.4 Reference Manual: String manipulation – https://www.lua.org/manual/5.4/manual.html#6.4
- `utf8.len` function – Dive into handling Unicode strings properly – https://www.lua.org/manual/5.4/manual.html#pdf-utf8.len
- Some Lua history and string interning info – https://www.lua.org/doc/hopl.pdf