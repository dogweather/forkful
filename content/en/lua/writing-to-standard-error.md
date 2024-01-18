---
title:                "Writing to standard error"
html_title:           "Lua recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Lua is a way for programmers to display error messages or other important information to the user. This is useful for debugging purposes or for providing helpful feedback to the user in case of an error in the program.

## How to:

To write to standard error in Lua, we use the "io.stderr.write" function. This function takes a string as an argument and displays it to the user's standard error output. Let's take a look at an example:

```Lua
io.stderr.write("Oh no, an error has occurred!")
```

The output of this code would be:

```
Oh no, an error has occurred!
```

We can also use the "io.stderr:write()" syntax to achieve the same result. Take a look at the following example:

```Lua
io.stderr:write("This is a warning message.")
```

The output of this code would be:

```
This is a warning message.
```

## Deep Dive

In Lua, standard error is also referred to as "stderr" and is used to handle error messages separate from regular output, which is called "stdout". This allows for better organization and debugging of programs. In some cases, standard error messages may also be redirected to a log file for further analysis.

An alternative method to writing to standard error in Lua is using the "assert" function. This function checks for an error condition and, if found, displays the error message to standard error. However, the "assert" function is limited to checking for certain types of errors, whereas the "io.stderr.write" function allows for more control over which messages are displayed to the user.

For those interested in the technical implementation details, standard error in Lua is implemented using C's standard I/O functions. The "io.stderr" file handle is simply a reference to the standard error stream in C.

## See Also

To learn more about writing to standard error in Lua, check out the official Lua documentation [here](https://www.lua.org/manual/5.3/manual.html#6.9). You can also read about standard error in general on the GNU website [here](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html#Standard-Streams).