---
title:    "Gleam recipe: Writing to standard error"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is an essential part of programming, as it allows us to handle and display error messages. This is especially important in debugging and troubleshooting our code, allowing us to identify and fix issues in our programs.

## How To

To write to standard error in Gleam, we can use the `gleam_io.console.err()` function. Let's take a look at an example:

```
Gleam import gleam_io
import gleam_result

fn write_error() {
  case gleam_io.console.err("Oops, something went wrong.") {
    Ok(_) -> gleam_result.Ok("Error message successfully written to standard error.")
    Error(err) -> gleam_result.Error(err)
  }
}
```

In this code, we are using the `gleam_io.console.err()` function to write the message "Oops, something went wrong." to the standard error output. We then use a case statement to handle the result of this function. If it returns an `Ok` value, we can display a success message. Otherwise, if it returns an `Error` value, we can handle the error by returning the `Error` value.

Now, let's run this code and see the output:

```
$ gleam run write_error.gleam
Oops, something went wrong.
```

As we can see, our error message was successfully written to standard error.

## Deep Dive

When writing to standard error, it is important to keep in mind the purpose of our error messages. These messages should provide useful information to help us identify and fix issues in our programs. They should be clear, concise, and specific, avoiding jargon and technical terms that may be difficult for non-technical users to understand.

Additionally, it is also important to handle errors properly and gracefully. This means using appropriate error handling techniques, such as `case` statements or `try` and `catch` blocks, to handle any potential errors that may occur.

In Gleam, we can also use the `gleam_io.console.errn()` function to write an error message to standard error with a unique identifier. This can be helpful in situations where we need to track multiple error messages and identify them based on their unique identifiers.

## See Also

- [Gleam Documentation on Writing to Standard Error](https://gleam.run/book/tour/errors.html#standard-error)
- [Gleam Console Module](https://gleam.run/modules/gleam_io#gleam_io.console)