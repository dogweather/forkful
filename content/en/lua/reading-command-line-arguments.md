---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

The process of reading command line arguments in Lua refers to fetching user input provided in the terminal when running a script. It allows customization of execution and passing of data without modifying code.

## How to:

Reading command line arguments in Lua is simple thanks to the global `arg` table. User input is read as strings in an array `arg` starting at index 0 (for script name) followed by parameters at increased indices.

Here's a quick illustration:

```Lua
-- argument_test.lua
print("Script name:",arg[0])
for i = 1, #arg do
    print("Argument", i .. ":", arg[i])
end
```
Test this with command:

```bash
lua argument_test.lua testArg1 testArg2 testArg3
```

And the output becomes:

```
Script name: argument_test.lua
Argument 1: testArg1
Argument 2: testArg2
Argument 3: testArg3
```

## Deep Dive:

Historical context: Lua has been around since 1993 but adopted `arg` officially in 5.1 (2006). Before, it relied on the deprecated `arg` mechanism in shell.

Alternatives: There are Lua libraries lik `Penlight` and `Lapp` that provide improved interfaces for dealing with command line arguments. They check types, set default values, and display help messages.

Implementation details: Lua's `arg` is a simple table holding all arguments. `arg[0]` is the script name and negative indices start from `arg[-1]` for the interpreter name going in reverse order.

## See Also:

1. [Lua 5.1 Reference Manual](https://www.lua.org/manual/5.1/manual.html) - The official Lua manual.
3. [Penlight library](https://stevedonovan.github.io/Penlight/api/index.html) - A popular utility library for Lua.