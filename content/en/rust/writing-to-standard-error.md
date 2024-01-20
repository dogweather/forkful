---
title:                "Writing to standard error"
html_title:           "Rust recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error is a way for programmers to print error messages or other important information to the command line. It can be helpful for debugging and providing feedback to the user. This is different from writing to standard output, which is typically used for regular program output.

## How to:

To write to standard error in Rust, you can use the ```eprintln!()``` macro. This function takes in a formatting string and any arguments to be inserted into the string. Here's an example of how it would look like when used:

```Rust
eprintln!("An error has occurred: {}", err);
```

This will print the error message provided by the ```err``` variable to the command line.

You can also use the ```eprint!()``` macro if you don't want to end the output with a new line. This is useful for printing multiple lines of output to standard error without a new line in between each line.

```Rust
eprint!("First line of output");
eprint!("Second line of output");
```

The output for this would be:

```
First line of outputSecond line of output
```

## Deep Dive

In contrast to standard output, standard error is meant for error messages and other important information that the user should see. Standard output is typically for regular program output and can be redirected to a file or another program.

Before standard error was introduced, programmers would often print error messages to standard output, which would get mixed in with the program's regular output. This made it difficult to differentiate between error messages and regular output. Standard error was created to solve this issue and provide a dedicated channel for important messages.

In addition to printing to standard error, some programmers also choose to log errors to a file for future reference. This allows them to go back and analyze any recurring errors or issues with their program.

There are also other alternatives to writing to standard error, such as using a logging library like ```log``` or ```env_logger```. These libraries provide more robust logging features and can be useful in larger projects.

## See Also

- [Difference between standard error and standard output](https://askubuntu.com/questions/350208/command-piping-mkdir-error-message-to-text-file)
- [Logging in Rust with the log and env_logger crates](https://www.reddit.com/r/rust/comments/4asrtr/logging_in_rust_with_the_log_and_env_logger/)