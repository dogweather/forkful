---
title:                "Rust recipe: Writing tests"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Why

Writing tests is an essential aspect of programming that ensures the reliability and functionality of our code. It allows us to catch bugs early on and make sure our code works as intended. In the long run, it saves time and effort and improves the overall quality of our projects.

## How To

To write tests in Rust, we will be using the `assert!` macro from the standard library. This macro takes in a boolean expression and prints an error message if the expression evaluates to false. Let's see an example:

```Rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// simple test to check if the function correctly adds two numbers
#[test]
fn test_add() {
    let result = add(5, 10);
    assert!(result == 15, "5 + 10 should equal 15, but got {}", result);
}
```

In this `test_add` function, we first call our `add` function with two numbers and store the result in a variable. Then, we use the `assert!` macro to check if the result is equal to 15. If not, the error message will be printed along with the actual value of the result. The `#[test]` attribute tells the compiler to treat this function as a test.

We can also use the `assert_eq!` macro to check for equality between two values, and the `assert_ne!` macro to check for inequality. Additionally, we can use the `should_panic` attribute to test for expected panics in our code. These are just a few examples, but there are many more testing tools available in Rust.

## Deep Dive

In Rust, tests are usually placed in a different directory `tests` than the rest of the codebase. This is to keep the tests separate from the actual code and to prevent them from being included in the compiled code.

Tests in Rust follow the same naming convention as functions, starting with `test_` and being snake case. Each test function should only test one specific aspect of our code, making it easier to identify and fix bugs.

Apart from unit tests, we can also write integration tests in Rust. These tests are placed in a separate `tests` directory and interact with our code as an external user would. This allows us to test the entire functionality of our code from a user's perspective.

## See Also

For more information on writing tests in Rust, check out these resources:

- [Rust Book - Writing Automated Tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/rust-by-example/testing.html)
- [Testing in Rust with `assert!`, `assert_eq!`, and `assert_ne!`](https://www.snoyman.com/blog/2014/12/rust-testing)

Now that you have an understanding of how to write tests in Rust, go ahead and start implementing them in your projects to improve your code's reliability and quality. Happy testing!