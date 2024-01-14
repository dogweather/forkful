---
title:                "Rust recipe: Writing to standard error"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is a common task in programming, especially in Rust. By outputting to standard error, instead of standard output, developers can quickly and easily identify and debug errors in their code.

## How To

To write to standard error in Rust, we can use the `eprintln!` macro. This macro works similar to the `println!` macro, but instead outputs to standard error. Let's look at an example:

```Rust
fn main() {
    let error_message = "Oops, something went wrong.";
    eprintln!("Error: {}", error_message);
}
```

This will output the following to standard error:

```
Error: Oops, something went wrong.
```

Notice how the `eprintln!` macro works the same as `println!`, but the output is directed to standard error instead of standard output.

We can also use the `format!` macro to format our error message before passing it to `eprintln!`. For example:

```Rust
fn main() {
    let error_code = 404;
    let error_message = format!("Page not found. Error code: {}", error_code);
    eprintln!("{}", error_message);
}
```

This will output the following to standard error:

```
Page not found. Error code: 404
```

## Deep Dive

In Rust, standard error is represented by the `std::io::stderr` stream. This stream is automatically created and available for use in our code. The `eprintln!` macro uses this stream to output our error messages.

It's also worth noting that writing to standard error is not only limited to error messages. We can use it to output any kind of information that we want to be displayed to the user, even if it's not an error.

## See Also

- [Rust Documentation on `eprintln!`](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Rust Documentation on standard IO streams](https://doc.rust-lang.org/std/io/index.html)