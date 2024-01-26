---
title:                "代码重构"
date:                  2024-01-26T01:18:50.509036-07:00
model:                 gpt-4-0125-preview
simple_title:         "代码重构"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/refactoring.md"
---

{{< edit_this_page >}}

## 什么是重构？为什么要重构？
重构是指对代码进行修改，使其更加清晰、可维护，同时不改变其外部行为的过程。程序员进行重构以提高代码的可读性、减少复杂性，并使代码库更容易适应未来的更新或功能添加。

## 如何进行重构：
假设你有一段代码，在多个函数中进行了一些重复的计算或字符串操作。这正是重构的主要目标。下面使用 Gleam 语言进行了重构前后的对比，Gleam 强调类型安全和不变性：

```gleam
// 重构前
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("面积是 \(area)")
}

// 重构后
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("面积是 \(area)")
}

// 在你的代码的另一部分，你会这样调用 print_area：
print_area(calculate_area(10, 20))
```

样例输出：
```
面积是 200
```

通过重构，我们让 `print_area` 函数更专注于打印，而计算则在其他地方处理，使代码更加模块化，更容易重用或测试。

## 深入探讨
重构作为一个概念，自编程诞生以来就一直存在——回顾和清理代码是良好的家庭作业的一部分。重构的现代形式化，以及今天使用的许多技术和模式，可以追溯到1999年马丁·福勒（Martin Fowler）发表的开创性著作《重构：改善既有代码的设计》。

在 Gleam 生态中，重构有特定的考虑因素。最重要的是在编译时进行强类型检查，这可以在你移动东西时提早捕捉到错误。Gleam的模式匹配和不变性特性也可以引导你编写更清晰、更简洁的代码——这是重构的主要目标之一。

重构的替代方案可能包括从头开始重写代码或用快速修复来修补代码。然而，重构通常是改进现有代码的最安全、最高效的方法，因为它涉及到增量的、明确的、保留行为的转换。

## 另请参阅
- 马丁·福勒的《重构》书籍：https://martinfowler.com/books/refactoring.html
- Gleam 语言官网，包含更多文档和示例：https://gleam.run/
- 由马丁·福勒著作的《重构：改善既有代码的设计》（适用于跨语言的基本原则）：https://martinfowler.com/books/refactoring.html