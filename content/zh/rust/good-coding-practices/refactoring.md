---
date: 2024-01-26 03:36:41.069584-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u91CD\u6784\u4E00\u6BB5\
  \u7B80\u5355\u7684Rust\u4EE3\u7801\uFF0C\u4F7F\u5176\u66F4\u52A0\u7B26\u5408\u4E60\
  \u60EF\u7528\u6CD5\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002\u6211\u4EEC\u4ECE\u4E00\u4E2A\
  \u8BA1\u7B97\u6574\u6570\u5411\u91CF\u548C\u7684\u51FD\u6570\u5F00\u59CB\uFF1A."
lastmod: '2024-04-05T22:38:46.691022-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u91CD\u6784\u4E00\u6BB5\
  \u7B80\u5355\u7684Rust\u4EE3\u7801\uFF0C\u4F7F\u5176\u66F4\u52A0\u7B26\u5408\u4E60\
  \u60EF\u7528\u6CD5\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002\u6211\u4EEC\u4ECE\u4E00\u4E2A\
  \u8BA1\u7B97\u6574\u6570\u5411\u91CF\u548C\u7684\u51FD\u6570\u5F00\u59CB\uFF1A."
title: "\u91CD\u6784"
weight: 19
---

## 如何操作：
让我们重构一段简单的Rust代码，使其更加符合习惯用法且易于维护。我们从一个计算整数向量和的函数开始：

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("总和为 {}", sum(&numbers));
}
```

输出：
```
总和为 15
```

现在，让我们通过利用迭代器和 `fold` 方法来重构它，使用更符合Rust习惯的方式：

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("总和为 {}", sum(&numbers));
}
```

输出没有变化——仍然是 `15`——但重构后的版本更加简洁且利用了Rust的优势，如借用和迭代器方法。

## 深入探讨
重构起源于Smalltalk社区，并通过Martin Fowler的书《重构：改善既有代码的设计》在Java世界中普及。其原则是普遍适用的，并且也适用于Rust，其中安全性和并发性至关重要。Rust鼓励通过在编译时捕获问题来编写健壮的代码，因此在重构期间，Rust编译器充当了一个安全网。

替代手动重构的方法包括使用自动化工具，如`rustfmt`用于代码格式化，`clippy`用于代码审查，它们可以建议更符合习惯的编码方式。然而，深入的重构往往需要对代码设计有深思熟虑的理解，这些工具不能完全自动化。

在Rust中，重构可能围绕改进类型使用、有效利用生命周期、减少不必要的分配，或使用并发模式，如在必要时使用`Arc<Mutex<T>>`。也常见于从`unwrap()`过渡到使用`Result<T, E>`进行更具表现力的错误处理。

## 另请参阅
要进一步深入探讨Rust中的重构：

- Rust官方书籍：https://doc.rust-lang.org/book/
- Rust通过例子学：https://doc.rust-lang.org/rust-by-example/
- Clippy，一个Rust语言工具：https://github.com/rust-lang/rust-clippy
- 《重构：改善既有代码的设计》Martin Fowler著：https://martinfowler.com/books/refactoring.html
