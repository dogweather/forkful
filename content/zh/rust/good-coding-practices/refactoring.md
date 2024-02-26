---
date: 2024-01-26 03:36:41.069584-07:00
description: "\u91CD\u6784\u662F\u5BF9\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\
  \u91CD\u65B0\u67B6\u6784\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u7F16\u7801\u7ED3\u6784\
  \u2014\u2014\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u6539\u5584\u8F6F\u4EF6\u7684\u975E\
  \u529F\u80FD\u5C5E\u6027\uFF0C\u6BD4\u5982\u53EF\u8BFB\u6027\u3001\u964D\u4F4E\u590D\
  \u6742\u6027\u3001\u63D0\u9AD8\u53EF\u7EF4\u62A4\u6027\uFF0C\u5E76\u521B\u5EFA\u4E00\
  \u4E2A\u66F4\u5177\u8868\u8FBE\u6027\u7684\u5185\u90E8\u67B6\u6784\u6216\u5BF9\u8C61\
  \u6A21\u578B\u4EE5\u63D0\u9AD8\u53EF\u6269\u5C55\u6027\u3002"
lastmod: '2024-02-25T18:49:45.092658-07:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5BF9\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\u7684\
  \u91CD\u65B0\u67B6\u6784\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u7F16\u7801\u7ED3\u6784\
  \u2014\u2014\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u6539\u5584\u8F6F\u4EF6\u7684\u975E\
  \u529F\u80FD\u5C5E\u6027\uFF0C\u6BD4\u5982\u53EF\u8BFB\u6027\u3001\u964D\u4F4E\u590D\
  \u6742\u6027\u3001\u63D0\u9AD8\u53EF\u7EF4\u62A4\u6027\uFF0C\u5E76\u521B\u5EFA\u4E00\
  \u4E2A\u66F4\u5177\u8868\u8FBE\u6027\u7684\u5185\u90E8\u67B6\u6784\u6216\u5BF9\u8C61\
  \u6A21\u578B\u4EE5\u63D0\u9AD8\u53EF\u6269\u5C55\u6027\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么与为什么？

重构是对现有计算机代码的重新架构过程——改变编码结构——而不改变其外部行为。程序员进行重构是为了改善软件的非功能属性，比如可读性、降低复杂性、提高可维护性，并创建一个更具表达性的内部架构或对象模型以提高可扩展性。

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
