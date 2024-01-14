---
title:                "Gleam recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
As developers, we often encounter situations where we need to debug our code and understand what is going on behind the scenes. Printing debug output is a simple yet effective way to gain insights into the inner workings of our code and find and fix any issues that may arise. 

## How To
To print debug output in Gleam, we can use the `IO` module's `formatln` function. Let's take a look at an example:

```
Gleam
import gleam/io

fn main() {
  let name = "Jane"
  gleam/io.formatln("Hello, {}!", [name])
}
```

In the above code, we have imported the `gleam/io` module and used the `formatln` function to print the message "Hello, Jane!" to the console. The curly braces in the string act as placeholders for the variables that we want to print, in this case, the `name` variable. We also pass in the `name` variable as an argument to the `formatln` function.

The output of running this code would be:

```
Hello, Jane!
```

We can also format the output to include more information, such as the current date and time. Let's see how we can do that:

```
Gleam
import time
import gleam/io

fn main() {
  let name = "Jane"
  let time = time.now()
  gleam/io.formatln("Hello, {}! The current time is {}.", [name, time])
}
```

This time, we have imported the `time` module and used its `now` function to get the current date and time. We have also added the `time` variable as an argument to the `formatln` function to print it along with the message. The output of running this code would be something like this:

```
Hello, Jane! The current time is 2021-07-22T12:34:56.789Z.
```

## Deep Dive
Printing debug output not only helps us to understand what is happening in our code but also allows us to monitor its performance and identify any bottlenecks. With the `formatln` function, we can also print the value of a specific variable at a particular point in our code. This can be useful in cases where we want to check the value of a variable after a specific operation has been performed.

For example, let's say we have a function that adds two numbers together and we want to print the result of the addition:

```
Gleam
import gleam/io

fn add(a, b) {
  let result = a + b
  gleam/io.formatln("The result of the addition is {}.", [result])
  result
}
```

In this example, we have added the `result` variable as an argument to the `formatln` function, which will print its value to the console. The `formatln` function also returns the `result` variable, which we can use in our code later on. 

## See Also
- [Gleam documentation on printing output](https://gleam.run/book/tutorials/debugging-output)
- [Debugging in Gleam](https://gleam.run/book/tutorials/debugging)