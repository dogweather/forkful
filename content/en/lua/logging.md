---
title:                "Logging"
date:                  2024-01-25T02:03:50.190107-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"
programming_language: "Lua"
category:             "Lua"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/logging.md"
---

{{< edit_this_page >}}

## What & Why?

Logging is essentially leaving a breadcrumb trail of what's happening in your code—kinda like a journal for your app. Programmers do it to debug, monitor performance, or catch the sneaky bugs that don't crop up until three in the morning on a Saturday.

## How to:

Let's log some stuff in Lua. We'll keep it simple—no fancy frameworks needed. I'm gonna show you a basic function to log messages with timestamps. We'll write logs to a file, but you can shoot them off to the console if that's your jam.

```Lua
local function logMessage(level, message)
    local timestamp = os.date("%Y-%m-%d %H:%M:%S")
    local logString = string.format("[%s] [%s] %s\n", timestamp, level, message)
    
    -- Print to console
    print(logString) 
    
    -- Append to log file
    local logFile = io.open("app.log", "a")
    if logFile then
        logFile:write(logString)
        logFile:close()
    end
end

-- Usage
logMessage('INFO', 'Something irrelevant happened.')
logMessage('WARNING', 'This might be bad.')
logMessage('ERROR', 'Yup, it's bad.')
```

Sample output in `app.log`:

```
[2023-04-01 14:22:31] [INFO] Something irrelevant happened.
[2023-04-01 14:22:32] [WARNING] This might be bad.
[2023-04-01 14:22:33] [ERROR] Yup, it's bad.
```

## Deep Dive

Logging's been around since the punch card days, when logs were more about which card went rogue. Now, it's about catching those glitches in the matrix—whether it's a misplaced byte or an API that's gotten a bit too cheeky.

There are alternatives to our homegrown script; serious Lua devs often lean on mature logging libraries like `lua-logging` or `Lualogging`. They offer more granularity with log levels and fancier output options.

Implementation-wise, our script is naive by design. Real-world apps need logs that are thread-safe and performant under load—something to consider before rolling your own solution in production.

## See Also

To dive deeper, or if you're ramping up on something more robust, check these out:

- The `lua-logging` library: https://keplerproject.github.io/lua-logging/
- Official Lua documentation for the `io` library: https://www.lua.org/manual/5.4/manual.html#6.8
- A quick refresher on `os.date` formatting: https://www.lua.org/manual/5.4/manual.html#pdf-os.date

Remember, great logging is like flossing—skip it, and things will eventually get messy. Happy coding!