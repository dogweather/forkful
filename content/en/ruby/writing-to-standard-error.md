---
title:                "Writing to standard error"
html_title:           "Ruby recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is a useful technique for debugging and error handling in Ruby programming. It allows developers to output informational messages, warnings, and errors to the standard error stream, which can then be viewed separately from standard output.

## How To

To write to standard error in Ruby, we use the `warn` method, passing in the message we want to output as a string. This will print the message to the standard error stream, separate from any standard output that may also be present.

```Ruby
# Output a warning to standard error
warn "Oops, something went wrong!"
```

This will produce the following output:

```sh
Oops, something went wrong!
```

We can also use the `puts` method with the special global variable `$stderr` to write to standard error. This will have the same effect as using `warn`.

```Ruby
# Output an error message to standard error
$stderr.puts "Error: Invalid input"
```

This will produce the following output:

```sh
Error: Invalid input
```

## Deep Dive

Writing to standard error differs from writing to standard output in that it is intended for error and warning messages, rather than regular program output. By default, standard error messages are displayed on the user's screen whereas standard output is often redirected to a file.

It is important to note that standard error messages are not the same as exceptions in Ruby. Exceptions are used for handling errors that occur during execution, while writing to standard error is more for informational messages and debugging purposes.

Additionally, it is good practice to provide a descriptive message when writing to standard error to aid in debugging and troubleshooting. This can include information such as the name of the method or function being executed, the input parameters, and any relevant data or context.

## See Also

- [Ruby documentation on `warn` method](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-warn)
- [Ruby documentation on standard streams](https://ruby-doc.org/core-2.7.1/IO.html#class-IO-label-Streaming)
- [Difference between standard error and standard output streams](https://unix.stackexchange.com/questions/4689/what-does-it-mean-to-write-to-stderr)