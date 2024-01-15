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

## Why
Writing tests for your Rust code may seem like an extra step that takes up time, but it's an important practice that can save you from potential headaches in the future. Tests act as a safety net, ensuring that your code functions as intended and catches any errors before they make it into production.

## How To
Writing tests in Rust is easy and can be done using the built-in testing framework, `cargo test`. Let's take a look at an example of testing a basic function that adds two numbers:

```Rust
// Define our function to be tested
fn add_numbers(a: i32, b: i32) -> i32 {
    a + b
}

// Create our test function using the `#[test]` attribute
#[test]
fn test_add_numbers() {
    // Define our expected output
    let expected = 5;

    // Call our function and save the result
    let result = add_numbers(2, 3);

    // Assert that the result equals the expected output
    assert_eq!(result, expected);
}
```

Running `cargo test` in the terminal will run all the tests in our code, including our newly created `test_add_numbers()` function. If all goes well, we should see the following output:

```
running 1 test
test test_add_numbers ... ok
```

If there are any errors, the test will fail and provide information on where the error occurred. This is useful for debugging and fixing any issues with our code.

## Deep Dive
Writing tests not only ensures that your code is working as intended, but it also promotes good coding practices. When writing tests, you are forced to think about potential edge cases and handle them in your code. This leads to more robust and reliable code.

Additionally, having a suite of tests for your code makes it easier to make changes or add new features without worrying about breaking existing functionality. You can simply run your tests and ensure that everything is working as expected before deploying any changes.

## See Also
- [Rust Documentation on Testing](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [The `assert` Macro in Rust](https://doc.rust-lang.org/std/macro.assert.html)
- [Writing Automated Tests in Rust](https://medium.com/@saschagrunert/unit-testing-rust-bb7acc97a12f)