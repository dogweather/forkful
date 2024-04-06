---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.687623-07:00
description: "\u5982\u4F55\uFF1A Rust \u7684\u5185\u7F6E\u6D4B\u8BD5\u6846\u67B6\u652F\
  \u6301\u5355\u5143\u6D4B\u8BD5\u3001\u96C6\u6210\u6D4B\u8BD5\u548C\u6587\u6863\u6D4B\
  \u8BD5\uFF0C\u65E0\u9700\u5916\u90E8\u5E93\u3002\u6D4B\u8BD5\u4F7F\u7528 `#[test]`\
  \ \u6CE8\u91CA\uFF0C\u4EFB\u4F55\u8FD9\u6837\u6CE8\u91CA\u7684\u51FD\u6570\u90FD\
  \u4F1A\u88AB\u7F16\u8BD1\u4E3A\u6D4B\u8BD5\u3002"
lastmod: '2024-04-05T21:53:47.845345-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何：
Rust 的内置测试框架支持单元测试、集成测试和文档测试，无需外部库。测试使用 `#[test]` 注释，任何这样注释的函数都会被编译为测试。

### 编写单元测试：
使用 `#[cfg(test)]` 标记的 `tests` 子模块将单元测试放在它们正在测试的模块中，以确保它们只在测试时编译。

```rust
// lib.rs 或 main.rs
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

运行测试：
```shell
$ cargo test
```

输出：
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (或 src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### 编写集成测试：
集成测试位于项目顶层的 tests 目录中，与 `src` 并列。`tests` 中的每个 `.rs` 文件都被编译为它自己的单独的箱子（crate）。

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### 使用流行的第三方库进行测试：
为了获得更广泛的测试能力，`proptest` 库可以生成广泛的输入来测试函数。

在 `Cargo.toml` 中将 `proptest` 添加为开发依赖：

```toml
[dev-dependencies]
proptest = "1.0"
```

使用 `proptest` 对许多自动生成的输入运行相同的测试：

```rust
// 在 tests/integration_test.rs 或一个模块的 #[cfg(test)] 内部

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

这检查了 `add` 对于广泛的 `i32` 输入不会导致恐慌。
