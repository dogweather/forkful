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

## Why

Debugging is an essential part of any coding process, and printing debug output can greatly aid in identifying and fixing errors in a program. In Gleam, this is done using the `gleam_io:print` function. But why should you bother with it? Let's find out!

## How To

Printing debug output in Gleam is a straightforward process. Simply use the `gleam_io:print` function in your code, passing in a string as the argument. For example:

```Gleam
gleam_io:print("Hello, debug!");
```

This will print the string "Hello, debug!" to the console. You can also use string interpolation to add variables or other data to your debug output. For instance:

```Gleam
let name = "John";
gleam_io:print("Hello, " <> name <> "!"); // Prints "Hello, John!"
```

In addition to `gleam_io:print`, there is also the `gleam_io:format` function, which allows for more complex string formatting. You can use this to print multiple variables or data types in a single line of output. For example:

```Gleam
let age = 27;
gleam_io:format("My name is ~s and I am ~d years old.", [name, age]); // Prints "My name is John and I am 27 years old."
```

Printing debug output is not limited to just strings. You can also use the `gleam_io:print` function to print lists, tuples, and other data structures. For example:

```Gleam
let fruits = ["apple", "banana", "orange"];
gleam_io:print(fruits); // Prints ["apple", "banana", "orange"]

let person = {name: "Jane", age: 32};
gleam_io:print(person); // Prints {name: "Jane", age: 32}
```

## Deep Dive

While printing debug output may seem like a simple concept, it can be a powerful tool in debugging and understanding your code. By using `gleam_io:print` or `gleam_io:format`, you can get a better understanding of the values and data flow in your program, making it easier to identify and fix any errors that may arise.

One important thing to note is that printing debug output can slow down your program, so it's best to only use it when necessary. You can also use conditional statements to control when debug output is printed, allowing you to quickly turn it on or off as needed.

Another useful feature is the ability to print to a file instead of the console. This can be done using the `gleam_io:write_file` function, which takes in a file path and string as arguments. This can be especially helpful when dealing with large amounts of data.

## See Also

- [The official Gleam documentation on `gleam_io:print`](https://gleam.run/docs/stdlib/gleam_io#print)
- [A tutorial on debugging in Gleam](https://dev.to/mrbeefy/debugging-on-mars-with-gleam-297b)