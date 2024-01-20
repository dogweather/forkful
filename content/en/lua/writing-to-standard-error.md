---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) lets your program chat about errors without cluttering standard output (stdout). It’s a clear signal to users and other programs that something needs attention.

## How to:

Lua talks to stderr using `io.stderr`. Here's how to print a simple error message:

```lua
io.stderr:write("Error: Something went wrong!\n")
```

Sample output on stderr:
```
Error: Something went wrong!
```

You can get fancy and combine it with error handling:

```lua
if not file then
    io.stderr:write("Error: File not found.\n")
    os.exit(1) -- bail with a non-zero exit code
end
```

## Deep Dive

Long time back, computers got two separate streams for output—stdout for main data, stderr for the oopsies. Lua kept this Unix convention. Sometimes, folks redirect stdout (like to a file) but still want errors on screen. That's stderr’s cue.

Alternatives? Some write to a log file, use a logging library, or send it across networks. But stderr is low-hassle for simple stuff.

Implementation-wise, Lua’s `io.stderr` is a file handle. It's like `io.stdout` or `io.stdin`, ready to go without fuss. Under the hood, whether it’s a text file or a terminal, Lua doesn't sweat—`io.stderr` handles it.

## See Also

Dive deeper or get some context:

- The Lua 5.4 Reference Manual: http://www.lua.org/manual/5.4/
- Unix philosophy: https://en.wikipedia.org/wiki/Unix_philosophy
- Learn more about `os.exit`: http://www.lua.org/pil/21.3.html
- A tour of Lua's input and output facilities: http://www.lua.org/pil/21.html