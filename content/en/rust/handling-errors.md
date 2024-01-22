---
title:                "Handling errors"
date:                  2024-01-21T21:19:37.063922-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Rust"
category:             "Rust"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?

Error handling is about dealing with things when they go sideways. Programmers do it to handle the unexpected, ensuring their Rust programs are robust and don’t just crash when faced with a hiccup.

## How to:

Rust handles errors in two major ways: recoverable and unrecoverable errors. Let's check out both.

Recoverable errors use `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("File opened successfully."),
        Err(_e) => println!("Failed to open file."),
    }
}
```

Output could be either "File opened successfully." or "Failed to open file." depending on your `hello.txt`.

For unrecoverable errors, we use `panic!`:

```Rust
fn main() {
    // This will cause the program to panic because the file probably doesn't exist.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Run it, and you'll see a panic message. Your program stops dead in its tracks.

## Deep Dive

Historically, error handling in programming has been a mess. Rust gets it right with a clear distinction between recoverable and unrecoverable errors.

The `Result` enum is for recoverable errors. It's explicit – you handle the `Ok` or `Err` variant. You've got methods like `unwrap()` and `expect()` too, but they're quick and dirty shortcuts that can lead to a `panic!`.

`panic!` is Rust’s way of shouting that something really bad happened, and it can't deal. It's like an unrecoverable error that stops execution immediately. A panic in Rust is often felt with bugs you don't expect to handle, like indexing out of array bounds.

Error handling by returning `Result` is preferred when you expect to deal with errors. It's idiomatic Rust, which means it's the way Rust developers agreed to do things. There's `Option<T>` as well, for cases when an error is just something being `None` instead of `Some(T)`. It's all about expecting the unexpected without fear.

Alternatives? Sure, you could use other error handling crates for more features or ergonomic use. Like `anyhow` for simple error handling, or `thiserror` for errors in library code.

## See Also

Interested in diving deeper? Here's where to go:

- [Rust Book on Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - A great place to understand Rust's error handling philosophy.
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - Interactive examples to get your hands dirty.
- [`anyhow` crate](https://crates.io/crates/anyhow) and [`thiserror` crate](https://crates.io/crates/thiserror) - For when you want to explore beyond the standard library.

Remember, good error handling isn't just coding; it's caring for your code's users. Happy coding!