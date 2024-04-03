---
date: 2024-01-20 17:53:05.561298-07:00
description: "Printing debug output is chucking info onto your screen to see what's\
  \ up with your code. Programmers do it to track down gremlins in the machine\u2014\
  bugs."
lastmod: '2024-03-13T22:45:00.207966-06:00'
model: gpt-4-1106-preview
summary: Printing debug output is chucking info onto your screen to see what's up
  with your code.
title: Printing debug output
weight: 33
---

## How to:
Here's the skinny on printing stuff in Lua:

```Lua
print("Hello, Debug World!")  -- Pops a string onto the console

local number = 42
print("The number is:", number)  -- Combine strings and numbers

local table = {name = "Lua", year = 1993}
print(table)  -- Prints the table reference, not super helpful
```

Sample Output:
```
Hello, Debug World!
The number is: 42
table: 0x194a330
```

To get into the table and show its guts, do this:
    
```Lua
for key, value in pairs(table) do
    print(key, "=", value)
end
```

Sample Output:
```
name = Lua
year = 1993
```

## Deep Dive
Printing debug output isn't new or fancy. It's reliable like an old hammer. See, back in the day, fancy debuggers weren't around. Coders printed to see where things went south. Lua's `print` function is straightforward. It shoves stuff to stdout—that's usually your terminal.

Alternatives? Lua has a bunch. There's the heavier `io.write()` if you need more control, like skipping new lines. Modules like `inspect` spill your tables' guts better than print can.

Implementation of `print` is basic in Lua's C source code. It uses `tostring` on each arg and shoves it to `stdout` with a newline. LuaJIT, a just-in-time compiler version of Lua, uses the same `print` approach, but belt-and-braces.

## See Also
Get the bigger picture:

- Lua's official `print` documentation: https://www.lua.org/manual/5.4/manual.html#pdf-print
- An intro to LuaJIT: http://luajit.org/intro.html
- `io` library breakdown for the down-and-dirty on `io.write`: https://www.lua.org/manual/5.4/manual.html#6.8
- The `inspect.lua` module, for when you tired of your tables playing shy: https://github.com/kikito/inspect.lua
