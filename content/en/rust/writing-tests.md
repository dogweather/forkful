---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code snippets that check if other bits of code work right. Programmers do it to catch bugs early, ensure functionality, and future-proof against new changes messing stuff up.

## How to:

Rust makes testing straightforward. Let's write a function and a test for it.

The function:

```Rust
fn add_two(a: i32) -> i32 {
    a + 2
}
```

The test:

```Rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(4, add_two(2));
    }
}
```

Run tests with `cargo test`. Expected output:

```plaintext
   Compiling my_crate v0.1.0 (/path/to/my_crate)
    Finished test [unoptimized + debuginfo] target(s) in 0.31 secs
     Running unittests (target/debug/deps/my_crate-abc123)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

## Deep Dive

Historically, tests are written after the code (post-hoc testing). Rust encourages writing tests alongside or before your code (test-driven development, TDD). There are other forms of testing - integration tests, doc tests, etc. - each with unique implementation details.

Tests in Rust are typically written in the same file or a `tests/` directory. They can be unit tests (like the `it_adds_two` example), integration tests (in separate files), or documentation tests (embedded in doc comments). The Rust compiler knows to treat functions with `#[test]` as tests to run with `cargo test`.

## See Also

- The Rust Book on testing: https://doc.rust-lang.org/book/ch11-00-testing.html
- Rust by Example's testing section: https://doc.rust-lang.org/stable/rust-by-example/testing.html
- API guidelines on testing: https://rust-lang.github.io/api-guidelines/documentation.html#crate-provides-docs-including-rustdoc-and-tests-c-dox
