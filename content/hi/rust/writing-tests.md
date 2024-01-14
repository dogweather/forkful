---
title:                "Rust: परीक्षाओं को लिखना"
simple_title:         "परीक्षाओं को लिखना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/writing-tests.md"
---

{{< edit_this_page >}}

# Kyon

Unit testing is an essential aspect of software development, ensuring that the code is functioning as intended and catching bugs before they become bigger issues. In Rust, writing tests is particularly important due to its focus on stability and reliability. By writing tests, you can have confidence in your code and prevent potential errors.

# Kaise Karein

Tests in Rust are written using the built-in testing framework called [`assert!`](https://doc.rust-lang.org/beta/std/macro.assert.html). This macro checks if the provided expression evaluates to `true` and returns an error if it doesn't. Let's take a look at an example of writing a simple test for a function that adds two numbers:

```
Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[test]   //this attribute marks the function as a test
fn test_add() {
    assert!(add(2, 3) == 5);   //this test will pass
    assert!(add(-1, 2) == 5);   //this test will fail
}
```

In the above code, we define a function `add` that adds two numbers and then use the `assert!` macro to test its output. We use the `#[test]` attribute above the function to mark it as a test. Running this test will give us the output:

```
failures:
---- test_add failures ----
thread 'main' panicked at 'assertion failed: `(left == right)`
  left: `-1`,
 right: `5`', src\main.rs:10:5
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

This shows us that our second assertion has failed, and the test needs to be fixed.

# Gehri Jaankari

Apart from the basic `assert!` macro, there are other useful macros provided by Rust's testing framework, such as `assert_eq!` and `assert_ne!` for comparing equality and inequality, `assert!(block)` for checking if the provided code block returns no errors, and more. Additionally, Rust also supports integration tests, which allows for testing the interaction between multiple modules in your code.

# Dekhein Bhi

[The Rust Book](https://doc.rust-lang.org/stable/book/) is an excellent resource for learning more about writing tests in Rust. Additionally, [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/testing/unit_testing.html) also has a chapter on unit testing that provides further examples and explanations.