---
title:                "Printing debug output"
html_title:           "Lua recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the process of displaying information about the execution of a program. This can help developers track and troubleshoot any issues that may arise during the execution of their code.

## How to:

To print debug output in Lua, we use the `print()` function. This function takes in one or more arguments and displays them in the console or terminal. Let's look at some examples:

```
-- printing a simple string
print("Hello, world!")

-- printing the value of a variable
myVar = 10
print(myVar)

-- printing multiple values
myString = "Hello"
myNumber = 5
print(myString, myNumber)
```

Output:
```
Hello, world!
10
Hello 5
```

We can also print the type of a variable using the `type()` function:
```
myVar = "test"
print(type(myVar))
```

Output:
```
string
```

## Deep Dive:

Debug output has been a common practice in programming since the early days of computing. It allows developers to see the inner workings of their code and identify any potential errors or bugs.

Alternatively, some programmers use a debugger tool to step through their code and analyze its behavior. However, printing debug output is a quick and easy way to get a glimpse into the execution of a program.

In Lua, the `print()` function calls the `tostring()` function on each argument, which converts it into a string representation. This is why we can use this function with different data types without any issues.

## See Also:

- Lua official documentation on `print()` function: https://www.lua.org/manual/5.4/manual.html#pdf-print
- Lua debugging guide: https://www.lua.org/manual/5.4/manual.html#25