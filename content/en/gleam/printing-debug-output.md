---
title:                "Gleam recipe: Printing debug output"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debug output is an important tool for programmers to understand how their code is running. It allows them to track the flow of the program and identify any potential errors or bugs. In Gleam, printing debug output can be a useful technique for developers to troubleshoot their code and improve its performance.

## How To
To print debug output in Gleam, you can use the `io.println` function. This function takes in a string argument and prints it to the standard output. Let's say we have a function that adds two numbers and we want to print the result for debugging purposes. We can do so with the following code:

```
Gleam> let add = fn(x, y) {
  x + y
}
Gleam> let result = add(3, 5)
Gleam> io.println("The result is: " ++ result)
```
The output of this code would be: `The result is: 8`

Now, let's say we want to print out the individual steps of our function. We can do that by adding multiple `io.println` statements within our function code block, like this:

```
let add = fn(x, y) {
  io.println("Adding " ++ x ++ " and " ++ y)
  x + y
}
```
This will print out the steps of the function as it is executed, making it easier to identify any bugs or issues in the code.

## Deep Dive
In Gleam, you can also use the `io.inspect` function for more detailed output. This function takes in any type of argument and prints it out in a structured format. Let's use the same add function from above and add an inspect statement to print out the arguments and result:

```
let add = fn(x, y) {
  io.inspect("x is: " ++ x)
  io.inspect("y is: " ++ y)
  x + y
}
let result = add(3, 5)
io.inspect("The result is: " ++ result)
```

The output of this code would be:
```
x is: 3
y is: 5
The result is: 8
```

This can be especially helpful when working with complex data structures such as lists or records, as it allows you to see the contents of each element or field.

## See Also
- [Gleam documentation on IO](https://gleam.run/documentation/docs-builtin-io)
- [Blog post on debugging in Gleam](https://dennmart.me/posts/debugging-in-gleam/)