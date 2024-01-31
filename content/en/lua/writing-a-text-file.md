---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file involves saving data to a file in a readable form. Programmers do it to store configurations, save user data, or log information for debugging.

## How to:
```Lua
-- Writing to a text file in Lua
local fileName = "example.txt"
local content = "Hello, file!"

local file = io.open(fileName, "w") -- Open the file in write mode
if file then
    file:write(content)              -- Write content to the file
    file:close()                     -- Always close the file when done
else
    print("Error opening file!")
end
```
Sample output in `example.txt`:
```
Hello, file!
```

Read the text file:
```Lua
local file = io.open(fileName, "r") -- Open the file in read mode
if file then
    local fileContent = file:read("*a") -- Read the entire content
    print(fileContent)                     -- Output the content to the console
    file:close()                        -- Close the file
else
    print("Error reading file!")
end
```
Console output:
```
Hello, file!
```

## Deep Dive
Lua's file handling paradigm has roots in C's stdio library, known for its simplicity and flexibility. Unlike databases or binary formats, text files are easily edited and human-readable without special tools. When dealing with small-scale data storage or simple data formats, text files are a suitable choice due to their accessibility and cross-platform compatibility. Implementation-wise, Lua's `io` library manages file operations, which include opening (`io.open`), reading (`file:read`), writing (`file:write`), and closing files (`file:close`).

## See Also
- Lua 5.4 reference manual: https://www.lua.org/manual/5.4/
- Programming in Lua (4th edition): https://www.lua.org/pil/contents.html
- I/O model comparison: https://www.lua.org/pil/21.2.1.html
