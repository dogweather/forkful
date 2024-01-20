---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means to fetch data from a file in the form of text. Programmers do this to extract information saved in logs, settings, other forms of saved data or external sources that make application development fluid.

## How to:

Reading a file in Lua is as simple as using the `io.open` function for opening the file and `:read` for reading it. Below is an example code that reads a text file:

```Lua
local file = io.open("text_file.txt", "r") -- Open a text file in read mode

if file then
  local content = file:read("*a") -- Read the whole file
  file:close() -- Always remember to close opened files
  print(content) -- Print the content
else
  print("Couldn't open file")
end
```

The output would be the content of your 'text_file.txt'. If there is an issue in opening the file, it would print 'Couldn't open file'.

## Deep Dive:

1. _Historical Context:_ Lua, a lightweight multi-paradigm language designed for embedding, has always followed the simplistic approach for file I/O operations, saving programmers from complexity.
2. _Alternatives:_ You've alternatives for reading a text file in Lua, like using `:lines()` method which reads the file line by line, which can come handy for big files.
3. _Implementation Details:_ Lua uses underlying C libraries to perform file I/O operations. When using `io.open`, the function returns a file object, and functions like `:read` and `close` are actually methods associated with this object.

## See Also:

For further reference, check out these helpful links:
- [Lua 5.4 Reference Manual - I/O Library](http://www.lua.org/manual/5.4/manual.html#6.8)
- [Lua-users - Tutorial: File Input/Output](http://lua-users.org/wiki/FileInputOutput)
- [TutorialsPoint - Lua File I/O](https://www.tutorialspoint.com/lua/lua_file_io.htm)