---
title:                "Reading command line arguments"
aliases:
- en/lua/reading-command-line-arguments.md
date:                  2024-01-20T17:56:27.955313-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments means grabbing the extra bits you type after your script’s name in the console. Programmers do this to tweak a program's behavior without changing the code, like choosing a file to open or setting the verbosity of output.

## How to:

Here's the skinny on grabbing those arguments in Lua:

```Lua
-- Save this as 'greet.lua'
local name = arg[1] -- arg[1] is the first command line argument
print("Hello, " .. (name or "stranger") .. "!")
```

Fire up the terminal and run it:

```
$ lua greet.lua LuaLearner
Hello, LuaLearner!
```

No name? No problem:

```
$ lua greet.lua
Hello, stranger!
```

## Deep Dive

Lua keeps it straightforward with the global `arg` table. Historically, folks have been reading command line arguments in programming since the dawn of time (well, since the birth of UNIX, at least). It's a staple of customization. 

In Lua, `arg` is an array with all the command-line goodies. `arg[0]` is the script name, and `arg[1]` onwards are the actual arguments. You can scoop them all up with a loop if you're feeling fancy:

```Lua
for i = 1, #arg do
  print("Argument " .. i .. ": " .. arg[i])
end
```

Alternatives? Sure, there are libraries out there for sophisticated argument parsing (like `Penlight`), but for many cases, `arg` does the trick without fuss.

As for implementation details, remember Lua's arrays are 1-based (they start counting at 1), not 0 like many other languages. That's why `arg[1]` is the first argument and not `arg[0]`.

## See Also

For those hungry for more, here's some extra chow:

- Lua 5.4 Reference Manual on the `arg` table: https://www.lua.org/manual/5.4/manual.html#6.1
- "Programming in Lua" (4th edition) for a solid grasp on Lua basics: https://www.lua.org/pil/contents.html
- Penlight, a Lua utility library with enhanced argument parsing: https://github.com/lunarmodules/Penlight
