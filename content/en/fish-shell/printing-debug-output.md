---
title:                "Printing debug output"
html_title:           "Fish Shell recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# What & Why?

Debugging is an important part of programming and involves finding and fixing errors in code. One way programmers do this is by printing debug output, which is when specific values or information is displayed while the code is running. This can help identify where an error is occurring and what the current state of the code is. 

# How to:

Fish Shell makes printing debug output a breeze with its built-in `fish_echo` function. Simply use the `fish_echo` command followed by the value or information you want to print. For example:

```
Fish Shell > set variable_name 3
Fish Shell > fish_echo $variable_name 
3
```

This can also be used within a script or function to print out multiple values or track the execution of the code.

# Deep Dive:

Historically, the most common way to print debug output was through the use of `printf` statements in C programming. However, with the rise of shell scripting and other high-level languages, the use of dedicated functions like `fish_echo` have become popular. 

Alternative methods for printing debug output include using the `echo` command or redirecting standard output to a file. However, these may not provide as much control and flexibility as using a dedicated function.

In terms of implementation, Fish Shell's `fish_echo` function is implemented in the C++ language and is available as a part of the core package. It also supports various formatting options, such as the `-n` flag for omitting the trailing new line.

# See Also:

For more information on Fish Shell's `fish_echo` function, check out the official documentation here: https://fishshell.com/docs/current/cmds/fish_echo.html

To learn more about debugging and other essential programming skills, consider checking out the tutorials and resources available on websites like Codecademy, Udemy, and FreeCodeCamp. Happy coding!