---
title:                "Reading command line arguments"
html_title:           "Gleam recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is the process of retrieving information entered by a user in the command line interface. Programmers use this to allow users to provide specific instructions or input data to their programs at runtime. This helps make their programs more dynamic and customizable.

## How to:
To read command line arguments in Gleam, we can use the `os.args` function. This will return a list of strings, with each element representing a command line argument entered by the user. Let's see an example:

``` gleam
// Retrieving command line arguments
let args = os.args
```

If a user enters `gleam run program.gleam arg1 arg2`, the `args` list will contain `["arg1", "arg2"]`. We can then use these arguments in our program as needed.

## Deep Dive:
Command line arguments have been a crucial aspect of programming since the early days of computing. They allow for more flexible and user-friendly programs, as users can provide input without having to modify the source code. Alternatives to reading command line arguments in Gleam include using environment variables or configuration files, but these may not be as convenient for users.

In Gleam, the `os.args` function is implemented by the `gleam_stdlib.os` module, which utilizes the Erlang `init:get_plain_arguments()` function. This ensures proper handling of command line arguments, including quoting and escaping characters.

## See Also:
- [Official Gleam Documentation](https://gleam.run/book/embedding-the-vm.html#parsing-command-line-arguments)
- [Erlang init:get_plain_arguments() documentation](https://erlang.org/doc/man/init.html#get_plain_arguments-0)