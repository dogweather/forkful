---
title:                "Using an interactive shell (REPL)"
date:                  2024-01-25T03:39:46.253437-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
REPL stands for Read-Eval-Print Loop, an interactive environment where you quickly test code. Programmers use it for experimenting, debugging, and learning a language's quirks.

## How to:
To hop into Lua's REPL, just enter `lua` in your terminal. Here's an example session:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
In the session, we declare a variable, perform basic arithmetic, manipulate a table, and loop through its items.

## Deep Dive
Lua’s lightweight nature makes its REPL ideal for prototyping. It's been around since Lua’s inception in the early 1990s, inspired by earlier interactive shells for languages like Lisp. Alternatives in other languages include `irb` for Ruby and `python` for Python, each with their own set of features. Lua's REPL is minimalistic; thus, it may lack advanced features found in others, like complex debugging tools. For a beefier experience, tools like ZeroBrane Studio or LuaDist's LuaRocks offer more than the basic REPL.

## See Also
- [Lua 5.4 Reference Manual - The Standalone Lua Interpreter](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)