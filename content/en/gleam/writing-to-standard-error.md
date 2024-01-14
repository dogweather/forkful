---
title:                "Gleam recipe: Writing to standard error"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to standard error can be a useful tool for programmers in many situations. It allows for error messages and other important information to be displayed separately from the regular output, making debugging and troubleshooting much easier.

## How To
To write to standard error in Gleam, you can use the `io.error` function. This function takes a string as its argument, which will be displayed on the standard error output.

```
Gleam ...
let error_message = "Something went wrong"
io.error(error_message)
...
```

Running this code will produce the following output:

```
Something went wrong
```

As you can see, the error message is displayed separately from the regular output.

## Deep Dive
One of the key benefits of writing to standard error is that it allows for error messages to be easily distinguished from regular output. This is especially useful when dealing with large amounts of data or complex programs, as it can be difficult to locate error messages in a sea of output.

Additionally, writing to standard error can be helpful in cases where you need to differentiate between types of messages, such as warnings, errors, or informational messages. This can aid in quickly identifying and addressing issues within your code.

Another important aspect to note is that writing to standard error does not halt the execution of the program, unlike writing to standard output. This means that even if an error occurs, the program will continue to run and display output, making it easier to identify and troubleshoot issues.

## See Also
Here are some helpful links for further reading on writing to standard error in Gleam:

- [Gleam Documentation on Writing to Standard Error](https://gleam.run/articles/writing-to-standard-error)
- [Gleam Language Reference on Standard I/O](https://gleam.run/reference/std/#standard-io)
- [Gleam Blog on Debugging with Standard Error](https://gleam.run/articles/debugging-with-standard-error)