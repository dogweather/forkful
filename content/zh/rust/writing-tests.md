---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

category:             "Rust"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
测试是检查代码功能是否按预期工作的方法。通过编写测试，开发者可以验证代码质量，预防未来的错误，确保软件的健壮性和可靠性。

## How to: 如何进行
```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn it_panics() {
        assert!(false, "This should panic!");
    }
}

fn main() {
    println!("If you see this, tests are not being run!");
}
```
运行 `cargo test`，你会看到：
```
running 2 tests
test tests::it_works ... ok
test tests::it_panics ... ok

test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Deep Dive 深入探讨
Rust的测试起源于软件工程的早期实践，它体现了极限编程(XP)中的测试驱动开发(TDD)原则。与其他语言相比，Rust内置的测试框架简单易用，不需要额外库。除了单元测试，还有集成测试和文档测试。在复杂项目中，可以使用外部测试框架如`proptest`或`quickcheck`进行属性测试。

## See Also 参考链接
- [Rust Book's Testing Chapter](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Rust by Example - Testing](https://doc.rust-lang.org/stable/rust-by-example/testing.html)
- [API documentation for `assert!` macro](https://doc.rust-lang.org/std/macro.assert.html)
