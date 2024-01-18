---
title:                "Reading command line arguments"
html_title:           "Lua recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is the process of obtaining user-provided values from the command line when running a program. This allows programmers to create more flexible and interactive programs that can be customized by the user. By reading command line arguments, programmers can easily incorporate user input, making their programs more dynamic and efficient.

## How to:
To read command line arguments in Lua, we use the global variable `arg`. This variable is an array that holds all the arguments passed to the program. To access a specific argument, we use the index of the argument in the array. Let's see an example:

```
Lua
print(arg[0]) -- prints the name of the program
print(arg[1]) -- prints the first argument entered
print(arg[2]) -- prints the second argument entered
```
If we run this program with `lua example.lua Hello World`, the output will be:
```
Lua
example.lua
Hello
World
```

We can also use loops to iterate over all the arguments passed to the program. Let's see an example of printing all the arguments entered by the user:

```
Lua
for i, v in ipairs(arg) do
  print(i, v) -- prints the index and value of each argument
end
```
If we run this program with `lua example.lua Hello World`, the output will be:
```
Lua
1 Hello
2 World
```

## Deep Dive:
Command line arguments have been widely used in programming for a long time. They provide a simple and efficient way to customize programs before they are executed. Alternative methods of obtaining user input, such as prompting the user during runtime, can be cumbersome and less efficient.

Lua also provides the `arg.n` variable, which contains the total number of arguments passed to the program. This can be useful for validation purposes or determining if the user has provided enough arguments.

To access the name of the program without using `arg[0]`, we can use `arg[-1]`. This is a common technique used to keep the code cleaner and easier to read.

## See Also:
To learn more about reading command line arguments in Lua, check out:
- [Lua official documentation](https://www.lua.org/)
- [A Beginner's Guide to Lua - Command Line Arguments](https://medium.com/beginners-guide-to-web-development/a-beginners-guide-to-lua-command-line-arguments-ec3d2a1c1b8)
- [Writing command-line apps in Lua](https://cmdchallenge.com/#/writing_cli_apps_in_lua)