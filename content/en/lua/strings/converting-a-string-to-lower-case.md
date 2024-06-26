---
date: 2024-01-20 17:38:44.142314-07:00
description: 'How to: In Lua, you lighten your load with `string.lower()`. Feed it
  a string, out pops the lowercase version. Observe.'
lastmod: '2024-03-13T22:45:00.193572-06:00'
model: gpt-4-1106-preview
summary: In Lua, you lighten your load with `string.lower()`.
title: Converting a string to lower case
weight: 4
---

## How to:
In Lua, you lighten your load with `string.lower()`. Feed it a string, out pops the lowercase version. Observe:

```lua
local originalString = "Hello, World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- Output: hello, world!
```

Run this snippet. The shouty capitals are now whisper-level lowercase.

## Deep Dive
Since the dawn of computing, folks have needed to smoosh text into a uniform case for all sorts of reasons, like sorting or case-insensitive logins. In Lua, `string.lower()` has been the go-to since its inception. It's neat, it's built-in, and it does its job without fuss.

But what's under the hood? `string.lower()` hustles through each character, and if it's uppercase (A through Z), it converts it. Lua relies on the ASCII values: 'A' (65) through 'Z' (90) get bumped to 'a' (97) through 'z' (122). The difference? 32. So, `lowercase = uppercase + 32`.

What if `string.lower()` feels too mainstream? You could manually trudge through characters with a loop, using ASCII values, or pattern matching with `string.gsub()`:

```lua
local s = "Make Me Lowercase, Please"
s = s:gsub("%u", function (upper) return string.char(upper:byte() + 32) end)
print(s)  -- Output: make me lowercase, please
```

But really, why row your boat when you've got an outboard motor (read: `string.lower()`)?

## See Also
Dig deeper into Lua's string manipulation with these goodies:
- [Programming in Lua (4th edition)](https://www.lua.org/pil/contents.html) for the ins, outs, and in-betweens of strings.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.4) for all the string functions when you're ready to go beyond lowercase.
