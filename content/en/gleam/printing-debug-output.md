---
title:                "Printing debug output"
html_title:           "Gleam recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Debugging output, also known as printing debug output, is a method in which programmers insert lines of code into their program that display certain values or messages while the program is running. This allows programmers to monitor their program's behavior and track down any errors or bugs that may occur. It is a crucial tool for troubleshooting and ensuring that a program functions correctly.

## How to:

To add debug output to your Gleam program, you can use the `IO.print` function. This function takes in a string as its argument and displays it in the terminal. Let's see an example:

```Gleam
let debug_message = "Debug output: "
IO.print(debug_message)
```

When you run this code, you will see the message "Debug output: " printed in the terminal. This simple example shows how you can use debug output to check the value of a variable or to display a message.

You can also use the `IO.println` function to print a message and a new line at once, like this:

```Gleam
IO.println("This is a debug message!")
```

This will result in the following output:

```
This is a debug message!
```

## Deep Dive:

Debug output has been a standard practice in programming since the early days of computer programming. Back then, programmers would use hardware devices such as lights or switches to display the program's output. Nowadays, with the advancement of technology, we can simply use the `IO` module provided by Gleam to print our debug output.

While debug output is a simple and effective method, there are other alternatives such as logging or using a debugger. However, these methods often require additional setup and can slow down the program's execution, whereas debug output can be easily implemented and does not affect the program's performance.

In terms of implementation, the `IO` module relies on the standard output stream to display the debug output. This means that the output will be displayed in the terminal, but it can also be redirected to a file if needed.

## See Also:

To learn more about Gleam and its features, check out the official documentation at https://gleam.run/.

You can also join the Gleam community on Discord (https://discord.gg/rDqPste) to connect with other programmers and discuss anything related to Gleam. Happy coding!