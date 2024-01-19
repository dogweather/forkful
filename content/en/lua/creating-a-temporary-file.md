---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file is a common procedure in programming where we make a non-permanent data storage location for interim results or data. Programmers use such files to manage memory, especially in processing large amounts of data or where data consistency and recovery are important.  

## How to: 

Lua doesn't provide in-built functions to create temp files. But fear not, the standard `os` library is our savior. Here's how we create a temporary file:

```Lua
local os = require('os')
local path = os.tmpname()

-- Now we have a temp file name; let's open it
local file = assert(io.open(path, 'w'))

-- Ensure to close and remove after
file:write("Temp data")
file:close()
os.remove(path)
```

And voila! You successfully wrote and removed a temp file, with "Temp data" written inside.

## Deep Dive 

Lua, deriving much from Scheme and other languages, often uses libraries for features others include in-built. Our reliance on `os.tmpname()` for temp files is one such case. But, it's no weakness. By providing only the bare minimum, Lua ensures versatility across platforms.

Alternatives? In Lua, not so many. The `os` and `io` libraries provide this basic need quite fine. But if you're into languages, many offer direct functions—like `tmpfile()` in C.

One thing worth noting: the files created by `os.tmpname()` aren't auto-deleted. They're your responsibility to manage. A triple-play of opening, writing, and removing—as shown above—should be your routine.

## See Also 

To get familiar with other data management techniques in Lua, check these links:

1. File I/O: https://www.tutorialspoint.com/lua/lua_file_io.htm
2. Standard Libraries: https://www.lua.org/manual/5.1/manual.html#5.1
3. `os` Library: https://www.lua.org/pil/22.1.html

And that's it—no fluff, just stuff. Happy coding!