---
title:    "Gleam recipe: Writing to standard error"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Writing to standard error is an essential skill for any programmer as it allows for better debugging and error handling in your code. Standard error, also known as stderr, is a stream in your program that captures all error messages. By writing to stderr, you can easily identify and troubleshoot any issues that may arise during runtime.

## How To

To write to standard error in Gleam, you can use the `io.fprintf` function. This function takes two arguments, the first being the stream you want to write to (in this case, stderr) and the second being the message you want to print. Let's look at an example:

```
import io
io.fprintf(stderr, "Oh no, an error occurred!")
```

This code will print the message "Oh no, an error occurred!" to standard error. You can also include variables in the message:

```
import io
let name = "John"
io.fprintf(stderr, "Hello %s, there was an error.", name)
```

This will output "Hello John, there was an error." to stderr.

## Deep Dive

It's important to note that writing to standard error will not cause your program to stop or crash. It simply prints the message to stderr and the program continues to run. This allows for better error handling as you can still see the output of your program even if an error occurs.

Another important aspect to understand is that you can redirect stderr to a file or another stream. This can be useful if you want to save your error messages for later analysis or send them to a different location. To do this, you can use the `io.stderr` function to get the stderr stream. For example:

```
import io
let error_stream = io.stderr()
io.fprintf(error_stream, "This error message will be written to a file.")
```

Now, let's say you want to redirect stderr to a file called "errors.log." You can do so by using shell redirection:

```
$ gleam run my_program > errors.log
```

This will send all stderr output to the "errors.log" file instead of printing it to the terminal.

## See Also

- [Write to Standard Error in Gleam](https://gleam.run/documentation/std.html#write-to-stderr)
- [Gleam IO Module](https://gleam.run/documentation/std.html#io-functions)