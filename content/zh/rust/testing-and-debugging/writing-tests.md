---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:22.687623-07:00
description: "\u7528 Rust \u7F16\u5199\u6D4B\u8BD5\u662F\u6307\u521B\u5EFA\u81EA\u52A8\
  \u68C0\u67E5\uFF0C\u4EE5\u786E\u4FDD\u4F60\u7684\u4EE3\u7801\u6309\u9884\u671F\u6267\
  \u884C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u6355\
  \u6349\u5230 bug\uFF0C\u4FC3\u8FDB\u91CD\u6784\uFF0C\u5E76\u968F\u7740\u65F6\u95F4\
  \u7684\u63A8\u79FB\u4FDD\u6301\u4EE3\u7801\u8D28\u91CF\u3002"
lastmod: '2024-03-11T00:14:21.289654-06:00'
model: gpt-4-0125-preview
summary: "\u7528 Rust \u7F16\u5199\u6D4B\u8BD5\u662F\u6307\u521B\u5EFA\u81EA\u52A8\
  \u68C0\u67E5\uFF0C\u4EE5\u786E\u4FDD\u4F60\u7684\u4EE3\u7801\u6309\u9884\u671F\u6267\
  \u884C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u6355\
  \u6349\u5230 bug\uFF0C\u4FC3\u8FDB\u91CD\u6784\uFF0C\u5E76\u968F\u7740\u65F6\u95F4\
  \u7684\u63A8\u79FB\u4FDD\u6301\u4EE3\u7801\u8D28\u91CF\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么与为什么？

用 Rust 编写测试是指创建自动检查，以确保你的代码按预期执行。程序员这样做是为了尽早捕捉到 bug，促进重构，并随着时间的推移保持代码质量。

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
