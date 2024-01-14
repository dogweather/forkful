---
title:    "Rust recipe: Writing tests"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
Many developers often overlook the importance of writing tests, especially when working with a language like Rust. However, writing tests is an essential part of the development process that can greatly improve code quality and prevent potential bugs from sneaking into production.

## How To
To get started with writing tests in Rust, you'll need to use the built-in `test` module. This module provides a `#[test]` attribute that can be used to mark functions as tests. Let's take a look at a simple example:

```Rust
// Define a function to be tested
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Write a test for the add function
#[test]
fn test_add() {
    assert_eq!(add(2, 3), 5);
}
```

In the example above, we define a simple `add` function and mark it as a test using the `#[test]` attribute. Within the test function, we use the `assert_eq!` macro to ensure that the result of calling `add` with the arguments `2` and `3` is equal to `5`. Now when we run our tests, we should see the following output:

```
running 1 test
test test_add ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

We can also use the `#[should_panic]` attribute to test for expected panics in our code. For example:

```Rust
// Write a test for the add function
#[test]
#[should_panic]
fn test_add_panic() {
    assert_eq!(add(2, 3), 6);
}
```

The above test should fail since we're expecting the `assert_eq!` macro to panic when comparing `5` to `6`. This ensures that our code handles errors and panics gracefully.

## Deep Dive
There are many other useful testing features in Rust, such as the `#[ignore]` attribute to skip certain tests, `assert_ne!` macro for asserting inequality, and `assert_matches!` macro for asserting pattern matching. You can also use the `cargo test` command to run all tests in a project or specific tests by name.

It's also worth mentioning that Rust tests are run in parallel by default, which can greatly speed up the test execution process. However, this also means that tests should be written in a way that they don't rely on global state or resources.

## See Also
- [The Rust Book: Writing Automated Tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust by Example: Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Rust Test Attributes](https://doc.rust-lang.org/rustdoc/attributes/testing.html)