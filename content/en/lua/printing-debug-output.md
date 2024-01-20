---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output helps us visualize what our code does. It's a simple way to track variables, check if code reaches a certain point, or exactly where an error occurs.

## How to:

In Lua, you can use the `print()` function:

```Lua
print("Hello") -- Will output: Hello
```

If you want to print a variable's value, use concatenation:

```Lua
local hours = 10
print("I have worked for " .. hours .. " hours.") -- Outputs: I have worked for 10 hours.
```
In case of debugging error or tracebacks, consider using `debug.traceback()`:

```Lua
local function buggyFunction()
    print(debug.traceback("Stack trace"))
    error("Oops, an error occurred!")
end

buggyFunction()
```
Running the code above, we'll see the detailed stack trace and the error message.

## Deep Dive

Printing debug traces back to the use of early "teletypewriters" where errors and statuses were printed as messages. It's a basic but powerful tool, especially when other complex debugging tools aren't available or overcomplicate things.

An alternative in Lua could be using a library like `Penlight` where we find `pl.utils.printf()` with better formatting support. But remember, external libraries need to be installed separately and they increase your project dependencies.

The `print()` function in Lua internally uses `_G.tostring()`, that converts the given parameter to its string equivalent, and `_G._io.write()`, that eventually writes this string to output.

## See Also:

- [Lua's Built In Functions](https://www.lua.org/manual/5.4/manual.html#6.1) 
- [Penlight Library](https://stevedonovan.github.io/Penlight/api/index.html)
- [Debugging in Lua](https://www.lua.org/pil/23.html)