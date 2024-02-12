---
title:                "Handling errors"
aliases:
- /en/lua/handling-errors/
date:                  2024-01-21T21:19:09.020887-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors in coding is about expecting the unexpected. It's the art of planning for when things go sideways so you can keep your program running smoothly.

## How to:
Lua uses two main functions for error handling: `pcall` and `xpcall`. Here’s how you use them:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("Oops! Something went wrong.")
    else
        print("All good!")
    end
end

-- Using pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("Success!")
else
    print("Caught an error:", errorMessage)
end

-- Using xpcall with an error handler
function myErrorHandler(err)
    print("Error Handler says:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("Was the call successful?", status)
```

Sample output could be:

```
Caught an error: Oops! Something went wrong.
Error Handler says: Oops! Something went wrong.
Was the call successful? false
```
Or, if no error occurs:
```
All good!
Success!
All good!
Was the call successful? true
```

## Deep Dive
Handling errors, or "exception handling," wasn't always a thing. Early programs crashed – a lot. As coding evolved, so did the need for stability. Lua's approach is simple compared to some languages. There are no `try/catch` blocks, just `pcall` and `xpcall`. The former protects a function call, returning a status and any error. The latter adds an error handling function, useful for custom clean-up or logging.

An alternative in Lua is to use `assert`, which can serve a similar purpose by throwing an error if its condition is false. But it's not as flexible as `pcall` for complex error handling scenarios.

Internally, `pcall` and `xpcall` work by setting up a "protected environment" for the function to run. If an error pops up, the environment catches it and can either handle it right away or pass it back for the program to handle.

## See Also
- The Programming in Lua book (third edition), available at https://www.lua.org/pil/ for thorough reading on error handling (Section 8.4).
- Official Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/ - for the most up-to-date info on Lua's error handling functions.
- Lua-users wiki on error handling: http://lua-users.org/wiki/ErrorHandling – for community insights and patterns.
