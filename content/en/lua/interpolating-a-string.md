---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation is much like a placeholder job, where variables within a string are swapped out. Coders use it to improve readability and efficiency, finding spaces filled with variable values clear-cut and concise.

## How To: 
Lua doesn't really offer native string interpolation; instead, we use the 'format' function. Let's check out some examples.

```Lua
name = "John"
print(string.format("Hi %s!", name))  --> Hi John!
```
Using multiple variables isn't an issue:
```Lua
age = 25
print(string.format("Hi %s! You are %d years old.", name, age))  --> Hi John! You are 25 years old.
```
## Deep Dive
1. Lua's lack of in-built string interpolation taps back to its history. Built in 1993 by a pair of Brazilian programmers, Lua was designed to be lightweight and efficient, skimping on many built-in functions other languages have.
2. There are workarounds if Lua's string.format isn't your cup of tea. You could use a custom function or libraries like LStringLib.
3. string.format uses the C library function 'sprintf', indicating Lua's C roots and its focus on streamlining performance.

## See Also
For more about Lua's minimalistic design and the history, hit up "Programming in Lua" by Roberto Lerusalimschy: https://www.lua.org/pil/

If you're interested in Lua string libraries, have a glance at LStringLib on GitHub: https://github.com/LuaDist/lstringlib

For more best practices with string formatting, check out: https://www.lua.org/manual/5.3/manual.html#6.4.2