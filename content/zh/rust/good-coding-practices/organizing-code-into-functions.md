---
date: 2024-01-26 01:16:06.820195-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u6307\u5C06\u7A0B\
  \u5E8F\u62C6\u5206\u6210\u53EF\u91CD\u7528\u7684\u3001\u6A21\u5757\u5316\u7684\u4EE3\
  \u7801\u5757\uFF0C\u5E76\u901A\u8FC7\u540D\u79F0\u6765\u6807\u8BC6\u5B83\u4EEC\u3002\
  \u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u6211\u4EEC\u7684\u4EE3\u7801\
  \u66F4\u5E72\u51C0\u3001\u66F4\u6613\u8BFB\uFF0C\u5E76\u66F4\u6613\u4E8E\u8C03\u8BD5\
  \u3002\u5B83\u7684\u76EE\u7684\u662F\u907F\u514D\u91CD\u590D\u81EA\u5DF1\u5E76\u7B80\
  \u5316\u66F4\u65B0\u3002"
lastmod: '2024-03-11T00:14:21.292239-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u6307\u5C06\u7A0B\
  \u5E8F\u62C6\u5206\u6210\u53EF\u91CD\u7528\u7684\u3001\u6A21\u5757\u5316\u7684\u4EE3\
  \u7801\u5757\uFF0C\u5E76\u901A\u8FC7\u540D\u79F0\u6765\u6807\u8BC6\u5B83\u4EEC\u3002\
  \u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u6211\u4EEC\u7684\u4EE3\u7801\
  \u66F4\u5E72\u51C0\u3001\u66F4\u6613\u8BFB\uFF0C\u5E76\u66F4\u6613\u4E8E\u8C03\u8BD5\
  \u3002\u5B83\u7684\u76EE\u7684\u662F\u907F\u514D\u91CD\u590D\u81EA\u5DF1\u5E76\u7B80\
  \u5316\u66F4\u65B0\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么?
将代码组织成函数是指将程序拆分成可重用的、模块化的代码块，并通过名称来标识它们。我们这样做是为了使我们的代码更干净、更易读，并更易于调试。它的目的是避免重复自己并简化更新。

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
