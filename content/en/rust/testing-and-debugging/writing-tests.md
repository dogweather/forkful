---
title:                "Writing tests"
aliases:
- /en/rust/writing-tests/
date:                  2024-02-03T19:03:39.537072-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Rust involves creating automated checks to ensure your code performs as expected. Programmers do this to catch bugs early, facilitate refactoring, and maintain code quality over time.

## How to:

Rust's built-in test framework supports unit, integration, and documentation tests without the need for external libraries. Tests are annotated with `#[test]`, and any function annotated as such is compiled as a test.

### Writing a Unit Test:

Place unit tests in the module they're testing using a `tests` sub-module marked with `#[cfg(test)]` to ensure they're only compiled when testing.

```rust
// lib.rs or main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Running tests:
```shell
$ cargo test
```

Output:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Writing Integration Tests:

Integration tests go in a tests directory at the top level of your project, next to `src`. Each `.rs` file in `tests` is compiled as its own separate crate.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testing with Popular Third-party Libraries:

For more extensive testing capabilities, the `proptest` library can generate a wide range of inputs to test functions.

Add `proptest` as a dev dependency in `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Use `proptest` to run the same test with many automatically generated inputs:

```rust
// inside tests/integration_test.rs or a module's #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

This checks that `add` doesn't panic for a wide range of `i32` inputs.
