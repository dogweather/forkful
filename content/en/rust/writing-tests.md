---
title:                "Writing tests"
html_title:           "Rust recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Rust refers to the practice of creating code that is specifically designed to test the functionality of other code. This is commonly done by programmers to ensure that their code is working as expected and to catch any potential bugs or errors that may arise.

## How to:

In Rust, writing tests is a simple process that involves using the built-in ```#[test]``` attribute to mark a function as a test function. This function can then contain assertions that evaluate the expected output against the actual output of the code being tested.

```Rust
#[test]
fn test_addition() {
    let result = add(2, 2);
    assert_eq!(result, 4);
}
```

Running these tests can be done through the ```cargo test``` command, which will execute all the functions marked as tests and display the results in the terminal.

```
running 1 test
test test_addition ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out ...
```

## Deep Dive:

The concept of writing tests has been around since the early days of programming, with various methodologies and approaches being developed over time. In Rust, the most popular testing framework is called "cargo test", which utilizes the built-in ```#[test]``` attribute to create test functions.

There are also other methods of testing in Rust, such as property-based testing, which involves generating test data automatically and checking the properties of the code being tested. This can be achieved through the use of external libraries like proptest or quickcheck.

When writing tests in Rust, it is important to ensure that the tests are as concise and focused as possible. This helps to avoid cluttering the codebase with unnecessary tests and allows for easier maintenance in the future.

## See Also:

- [Rust Book - Writing automated tests](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [cargo test Reference](https://doc.rust-lang.org/cargo/commands/cargo-test.html)
- [Property-based testing in Rust with proptest](https://github.com/AltSysrq/proptest)