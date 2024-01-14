---
title:    "Rust recipe: Writing to standard error"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

When writing code, it is important to have robust error handling in order to ensure that your program runs smoothly and any potential issues are caught. Writing to standard error allows you to display error messages to the user, making it easier to understand and troubleshoot any errors that may occur.

## How To

To write to standard error in Rust, you can use the `eprintln!` macro. This macro works similarly to `println!`, but it prints to standard error instead of standard output. Let's take a look at an example:

```rust
fn main() {
  let x = 10;
  let y = 0;

  if y == 0 {
    eprintln!("Error: Cannot divide by 0");
  } else {
    let result = x / y;
    println!("Result: {}", result);
  }
}
```

In this code, we are attempting to divide `x` by `y`. However, since `y` is 0, it will result in a division by 0 error. Instead of using `println!` to display the error message, we use `eprintln!` to print it to standard error. The output of this code will be:

```
Error: Cannot divide by 0
```

## Deep Dive

While using `eprintln!` is a simple and effective way to print to standard error, there are other ways to accomplish this as well. For more advanced error handling, you can use the `std::io::Write` trait to write directly to standard error. You can also use the `std::io::stderr` function to get a handle to standard error and then use methods such as `write` or `writeln` to write to it.

It is also worth noting that writing to standard error will result in a line break at the end of the message, while using `eprint!` will not include a line break. This can be useful if you want to display multiple error messages on the same line.

## See Also

For more information on writing to standard error in Rust, check out the following resources:

- [The Rust Standard Library](https://doc.rust-lang.org/std/io/struct.Stderr.html)
- [Rust By Example: Error Handling](https://doc.rust-lang.org/rust-by-example/error/multiple_error_types/boxing_errors.html)
- [The Rust Book: Standard Input and Output](https://doc.rust-lang.org/book/ch09-03-slices.html#using-an-io-project-for-more-than-just-development)

Happy coding!