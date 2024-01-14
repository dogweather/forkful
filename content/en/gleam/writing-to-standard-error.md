---
title:                "Gleam recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why Writing to Standard Error in Gleam is Useful

Writing to standard error in Gleam can be useful for debugging purposes. It allows you to print out specific information or error messages that can help you troubleshoot any issues in your code. This can save you time and effort in the troubleshooting process.

## How To Write to Standard Error in Gleam

To write to standard error in Gleam, you can use the ```gleam/io:stderr``` module. Here's an example code snippet:

```Gleam
import gleam/io

fn main() {
  io.stderr.print("Oops, an error occurred!")
}
```

In this example, we imported the ```gleam/io``` module and used the ```stderr.print()``` function to print out a custom error message to the standard error stream. 

The output of this code would be:

```
Oops, an error occurred!
```

## Deep Dive into Writing to Standard Error

In Gleam, everything printed to ```stdout``` goes to the standard output stream, which is typically displayed in the console when you run your program. The ```stderr``` stream, on the other hand, is used for error messages and is typically printed in red or highlighted in some way to differentiate it from regular output.

By using the ```stderr.print()``` function, you can add more context to your error messages and make them stand out for easier troubleshooting. You can also use the ```stderr.println()``` function to add a new line after your error message.

## See Also
- [Gleam.io module documentation](https://gleam.run/documentation/stdlib/io)
- [Gleam error handling documentation](https://gleam.run/documentation/error_handling)

By writing to standard error in Gleam, you can improve your debugging process and make it easier to identify and fix any issues in your code. Give it a try in your next project and see how it can benefit you!