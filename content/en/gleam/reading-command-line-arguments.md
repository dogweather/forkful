---
title:                "Gleam recipe: Reading command line arguments"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why

Command line arguments are a crucial aspect of programming in any language, including Gleam. They allow for flexibility and customization in how a program is executed. In this blog post, we will explore how to effectively read and utilize command line arguments in Gleam.

##How To

To read command line arguments in Gleam, we will use the `args` module from the `sys` standard library. First, we must import the module by adding the following line to our code: 

```Gleam
import sys/args
```

Next, we can access the command line arguments in the `execute` function by using the `sys.args` function. It returns a list of strings, with the first element being the program name and subsequent elements being any arguments passed in. For example, if we run our program with the command `gleam program.gleam arg1 arg2`, we can access `arg1` and `arg2` using `sys.args` as follows:

```Gleam
execute = fn() {
  let args = sys.args()
  let arg1 = List.get_or_else(args, 1, "") // "arg1"
  let arg2 = List.get_or_else(args, 2, "") // "arg2"
  // rest of the code
}
```

We can also use pattern matching to extract specific arguments or use the `List.contains` function to check if a certain argument was passed in. 

##Deep Dive

While reading command line arguments may seem straightforward, there are a few things to keep in mind. Firstly, the arguments are always returned as strings, so conversions may be necessary depending on the type of data being passed in. Secondly, make sure to handle any potential errors that may occur, such as invalid arguments or missing arguments.

Another thing to consider is the order in which arguments are passed in. By default, the arguments are passed in the order they were entered on the command line. However, in some cases, the order may not matter and it may be useful to allow the arguments to be passed in any order. This can be achieved by using flags, where a specific flag indicates which argument follows. The `flags` module from the `sys` library can be used to handle this scenario.

##See Also

For more information on the `args` and `flags` modules, please refer to the official Gleam documentation: 

- `sys` standard library: https://gleam.run/documentation/stdlib/sys
- `args` module: https://gleam.run/documentation/stdlib/sys/args
- `flags` module: https://gleam.run/documentation/stdlib/sys/flags

Happy coding with command line arguments in Gleam!