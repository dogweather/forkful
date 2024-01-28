---
title:                "Using a debugger"
date:                  2024-01-25T20:49:59.490338-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using a debugger"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## What & Why?
A debugger is a tool that lets you inspect and control the execution of a program, making it easy to pinpoint where things go awry. Programmers use debuggers to squash bugs, understand code flow, and to ensure their code is as clean as a whistle.

## How to:
Lua doesn't come with a built-in debugger, but you can use external ones, like ZeroBrane Studio. Here's a taste of how you'd work with it:

```Lua
-- This is a simple Lua script with an intentional error
local function add(a, b)
    local result = a+ b -- Oops, let's pretend we forgot to define 'b'
    return result
end

print(add(10))
```

When you run this in a debugger, it'll halt execution where things mess up. You'll see something like this:

```
lua: example.lua:3: attempt to perform arithmetic on a nil value (local 'b')
stack traceback:
	example.lua:3: in function 'add'
	example.lua:7: in main chunk
	[C]: in ?
```

You can set breakpoints, step through your code, and peek at variable values to track down the bug without losing your marbles.

## Deep Dive
Lua's simplicity doesn't extend to debugging, sadly. No worries though, the Lua community has your back. Tools like ZeroBrane Studio, LuaDec, and others offer debugging capabilities. Historically, debuggers existed not long after the first programs turned sour, giving devs the means to fix their code without blindly fiddling around.

With Lua, you often rely on external debuggers or build them into your development environment. ZeroBrane Studio, for instance, is an IDE that fully integrates a Lua debugger. It lets you step through code, set breakpoints, and watch variables. On the implementation side, debuggers typically use hooks to insert breakpoints and other debugging facilities.

Alternatives? You bet. Good old `print` statements, affectionately known as "printf debugging," can sometimes do the trick without fancy tools.

## See Also
To continue your debugging journey, check out:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-users wiki on Debugging Lua Code: http://lua-users.org/wiki/DebuggingLuaCode
- The `debug` library reference in Lua's manual: https://www.lua.org/manual/5.4/manual.html#6.10
