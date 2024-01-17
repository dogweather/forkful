---
title:                "Виведення відлагоджувального виводу"
html_title:           "Gleam: Виведення відлагоджувального виводу"
simple_title:         "Виведення відлагоджувального виводу"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

Whaddup, Ukrainian coders! Are you tired of spending hours trying to debug your code? Well, guess what - you don't have to anymore! Say hello to printing debug output in Gleam.

## What & Why?
Okay, so what is printing debug output? It's basically a fancy way of saying that you can print information from your code while it's running, in order to track down any errors or bugs. Why do we do it? Because let's be real, no one wants to spend hours trying to figure out why their code isn't working when they could just print out some helpful info to guide them in the right direction.

## How to:
So, how can you print debug output in Gleam? It's super simple, check it out:
```Gleam
import gleam/io

// Printing a string
let greeting = "Hello World!"
io.println(greeting)

// Printing a number
let num = 42
io.println(num)
```
And here's the output you'll see when you run this code:
```
Hello World!
42
```
Easy peasy, right?

## Deep Dive:
Printing debug output is nothing new in the programming world. Historically, developers have used it as a way to debug their code and track down any issues. However, there are alternatives such as using a debugger or writing unit tests. So why choose printing debug output? Well, it's a quick and easy way to get some insights into your code without having to set up a whole debugging environment or write extensive tests. Plus, it can come in handy when you're working on a new project or trying to troubleshoot a problem on the fly.

In terms of implementation, Gleam uses the `io.println()` function to print debug output. You can pass in any type of data and it will print it out for you. Keep in mind, you'll want to remove any debug output before deploying your code to production, as it can slow down your program.

## See Also:
Wanna learn more about printing debug output? Check out the official Gleam documentation [here](https://gleam.run/book/tour/printing-debug-output.html) and [here](https://gleam.run/book/core-functions.html#io-module). And if you're interested in other debugging techniques, look into using a debugger or writing unit tests. Happy coding!