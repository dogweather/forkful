---
date: 2024-01-26 01:16:06.820195-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5047\u8BBE\u4F60\u6709\u4E00\u6BB5\u8BA1\u7B97\
  \u5706\u9762\u79EF\u591A\u6B21\u7684\u4EE3\u7801\u3002\u4F60\u4E0D\u662F\u91CD\u590D\
  \u8FD9\u4E2A\u516C\u5F0F\uFF0C\u800C\u662F\u5C06\u5B83\u5C01\u88C5\u8FDB\u4E00\u4E2A\
  \u51FD\u6570\u3002"
lastmod: '2024-04-05T22:38:46.687969-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A \u5047\u8BBE\u4F60\u6709\u4E00\u6BB5\u8BA1\u7B97\
  \u5706\u9762\u79EF\u591A\u6B21\u7684\u4EE3\u7801\u3002\u4F60\u4E0D\u662F\u91CD\u590D\
  \u8FD9\u4E2A\u516C\u5F0F\uFF0C\u800C\u662F\u5C06\u5B83\u5C01\u88C5\u8FDB\u4E00\u4E2A\
  \u51FD\u6570\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何做：
假设你有一段计算圆面积多次的代码。你不是重复这个公式，而是将它封装进一个函数。

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("圆的面积是：{}", area);
}
```

输出：

```
圆的面积是：78.53981633974483
```

## 深入探讨
历史上，函数源自数学，它将输入映射到输出。在编程中，自汇编语言时代以来，它们就已经存在了，尽管我们称它们为“子程序”。Rust 函数可以返回值甚至是其他函数，这得益于一等函数和闭包。

代替方案？内联代码或宏，但它们对复杂逻辑来说很混乱。用带方法的对象来组织功能是另一种方式，与独立函数不同的风格。

在 Rust 中的实现相当直接。函数声明它们的参数类型和返回类型。按照惯例，它们使用“蛇形命名法”。你有公共函数（`pub fn`）供模块外部使用，也有私有函数供内部使用。Rust 还有一个很酷的特性，即在函数的最后一个表达式中你不需要 `return` 关键字。

## 另请参阅
查看以下更多信息：
- Rust 编程语言书籍：[函数](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust 通过示例关于[函数](https://doc.rust-lang.org/rust-by-example/fn.html)
