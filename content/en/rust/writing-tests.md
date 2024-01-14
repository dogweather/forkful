---
title:    "Rust recipe: Writing tests"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential part of the development process in any programming language, including Rust. It ensures that our code is functioning correctly and helps us catch any potential bugs or errors. By taking the time to write tests, we can save ourselves from encountering unexpected issues in our code down the line.

## How To

To write tests in Rust, we will be using the built-in Rust testing framework called `cargo test`. This framework allows us to run and manage our tests easily. Let's create a simple test for a function that adds two numbers together. 

```
Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[test]
fn test_add() {
    let result = add(3, 4);
    assert_eq!(result, 7);
}
```

In this example, we have created a function called `add` that takes in two `i32` integers and returns their sum. The `#[test]` annotation indicates that this is a test, and the `assert_eq!` macro checks if the `result` is equal to the expected output `7`. 

To run this test, we can use the command `cargo test` in our terminal. If all goes well, we should see a message saying `test test_add ... ok`.

## Deep Dive

Now that we have a basic understanding of writing tests in Rust, let's dive deeper into some useful features and tips.

### Structuring Tests

In our previous example, we used the `#[test]` annotation to indicate that this is a test function. We can also use the `#[cfg(test)]` attribute on our test module to let the compiler know that the modules only contain test code. This way, we prevent our test code from building in our final executable.

### Testing Error Cases

We can also use the `assert!` macro to test our functions that are expected to return an error. For example, if we have a function that divides two numbers, and we want to test if it returns an error when the second number is zero.

```
Rust
fn divide(x: f32, y: f32) -> Result<f32, String> {
    if y == 0.0 {
        Err("Cannot divide by zero!".to_string())
    } else {
        Ok(x / y)
    }
}

#[test]
fn test_divide() {
    let result = divide(5.0, 0.0);
    assert!(result.is_err());
}
```

### Integration Testing

In addition to unit testing, we can also write integration tests that test the interaction between different modules and components in our code. We can do this by creating a `tests` directory and placing our integration test files inside it. These tests will be run separately from our unit tests by using the command `cargo test --tests`.

## See Also

- [The Rust Book: Chapter 11 - Automated Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust By Example: Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [Rust Testing Guide](https://rust-lang-nursery.github.io/rust-cookbook/testing.html)