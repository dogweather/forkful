---
title:                "将代码组织成函数"
date:                  2024-01-26T01:16:06.820195-07:00
model:                 gpt-4-0125-preview
simple_title:         "将代码组织成函数"

category:             "Rust"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/organizing-code-into-functions.md"
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
