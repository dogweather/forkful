---
title:                "Working with complex numbers"
aliases:
- en/lua/working-with-complex-numbers.md
date:                  2024-01-25T03:00:04.957882-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers extend the idea of the one-dimensional number line into the two-dimensional plane by including a perpendicular imaginary axis. Programmers work with them in fields like signal processing, fluid dynamics, and electrical engineering, where they're essential for representing oscillations and other phenomena.

## How to:
In Lua, you can represent complex numbers with tables. The basic operations involve adding, subtracting, multiplying, and dividing these tables. Here's how:

```lua
-- Define two complex numbers as tables
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Function to add two complex numbers
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Sample output
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Deep Dive
Complex numbers have been around since the 16th century, helping solve equations that couldn't be cracked with just real numbers. Lua itself doesn't have a built-in complex number type. However, this is no biggie—you can craft your own complex number manipulations using tables and functions, as shown above. Or, if your needs go deeper, snag a library like LuaComplex. This is a fine pick because it's built specifically for Lua and takes the manual work off your plate. Libraries like this also often optimize operations under the hood, so they're faster than rolling your own.

## See Also
For more detailed examples and advanced operations, check these out:

- LuaComplex library: https://github.com/davidm/lua-complex
- "Programming in Lua" book, for custom data type creation: https://www.lua.org/pil/11.1.html
- Wikipedia on complex numbers' uses in different fields: https://en.wikipedia.org/wiki/Complex_number#Applications
