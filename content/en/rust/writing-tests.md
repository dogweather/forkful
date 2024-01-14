---
title:                "Rust recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Why Writing Tests in Rust is Important

When it comes to writing code, testing is an essential step in ensuring its reliability and functionality. Rust, being a systems programming language, allows for low-level and efficient code, but that also means more room for errors. Therefore, writing tests in Rust is crucial for ensuring code quality and reducing potential bugs.

## How To Write Tests in Rust

Writing tests in Rust is simple and intuitive, thanks to its built-in unit testing framework called `test`. Here's an example of a basic test for a function that calculates the square of a number:

```Rust
fn square(x: i32) -> i32 {
    x * x
}

#[test]
fn test_square() {
    assert_eq!(square(5), 25);
}
```

In the above code, we first define a function called `square` that takes in an integer and returns its square. Then, we use the `test` attribute to define a unit test function called `test_square`. Within this function, we use the `assert_eq` macro to check if the output of `square(5)` is equal to 25, as expected. If the assertion fails, the test will fail, indicating a potential bug.

To run the test, we use the `cargo test` command. This will compile our code and run all the tests defined in our project. If there are any failures, the output will show which assertion failed, helping us pinpoint the issue in our code.

## Deep Dive into Writing Tests in Rust

There are several other features and techniques that can be used when writing tests in Rust. These include testing multiple scenarios, using the `#[should_panic]` attribute to test for expected panics, and using the `TestResult` type to handle error messages. There are also crates available, such as `quickcheck` and `proptest`, for property-based testing.

Additionally, one can use the `integration_test` attribute to write tests that involve multiple files or binaries. And for more complex projects, Rust's `mock` and `spy` features can be used to mock external dependencies, making testing even more efficient.

## See Also

- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Official Rust Testing Documentation](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Property-based testing in Rust with proptest](https://fasterthanli.me/articles/quickcheck-for-rust)
- [Integration testing in Rust](https://doc.rust-lang.org/book/ch11-02-running-tests.html#the-tests-directory-and-integration-tests)