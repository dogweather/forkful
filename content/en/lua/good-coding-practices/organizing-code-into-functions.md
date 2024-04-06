---
date: 2024-01-25 03:00:08.015698-07:00
description: 'How to: Functions get more complex, handling various tasks.'
lastmod: '2024-04-05T21:53:35.906472-06:00'
model: gpt-4-1106-preview
summary: Functions get more complex, handling various tasks.
title: Organizing code into functions
weight: 18
---

## How to:
```Lua
-- Define a simple function to greet
function greet(name)
    return "Hello, " .. name .. "!"
end

-- Use the function
print(greet("Lua Programmer")) -- Sample Output: Hello, Lua Programmer!
```

Functions get more complex, handling various tasks:
```Lua
-- A function to calculate the area of a rectangle
function calculateArea(width, height)
    return width * height
end

-- Call the function and print the result
local area = calculateArea(5, 4)
print(area)  -- Sample Output: 20
```

## Deep Dive
Lua, since its inception in the 90s, has encouraged modular design. Organizing code with functions is not unique to Lua—it's been in practice since the dawn of programming languages like Fortran and Lisp. Alternatives like inline code and copying and pasting the same code over aren't just frowned upon; they're potential bug nests.

In Lua, functions are first-class citizens, meaning they can be stored in variables, passed as arguments, and returned from other functions. They're versatile. Lua's single-threaded nature means you've got to keep functions lean and mean for performance. Functions can be local (scoped) or global, and understanding when to use each can make or break your script's efficiency.

## See Also
- Official Lua documentation on functions: https://www.lua.org/pil/6.html
- Practical examples of function use in Lua: https://lua-users.org/wiki/SampleCode
- Clean code practices in Lua: https://github.com/Olivine-Labs/lua-style-guide
