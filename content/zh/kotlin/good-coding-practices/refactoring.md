---
date: 2024-01-26 01:46:11.247506-07:00
description: "\u91CD\u6784\u662F\u4E00\u79CD\u8C03\u6574\u73B0\u6709\u4EE3\u7801\u7684\
  \u8FC7\u7A0B\uFF0C\u76EE\u7684\u662F\u6539\u5584\u5176\u7ED3\u6784\u3001\u53EF\u8BFB\
  \u6027\u548C\u6027\u80FD\uFF0C\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\
  \u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\
  \u66F4\u6613\u4E8E\u7EF4\u62A4\uFF0C\u7B80\u5316\u65B0\u529F\u80FD\u7684\u6DFB\u52A0\
  \uFF0C\u5E76\u66F4\u5BB9\u6613\u5730\u53D1\u73B0\u548C\u4FEE\u590D\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:47.728964-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u4E00\u79CD\u8C03\u6574\u73B0\u6709\u4EE3\u7801\u7684\
  \u8FC7\u7A0B\uFF0C\u76EE\u7684\u662F\u6539\u5584\u5176\u7ED3\u6784\u3001\u53EF\u8BFB\
  \u6027\u548C\u6027\u80FD\uFF0C\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\
  \u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u4F7F\u4EE3\u7801\
  \u66F4\u6613\u4E8E\u7EF4\u62A4\uFF0C\u7B80\u5316\u65B0\u529F\u80FD\u7684\u6DFB\u52A0\
  \uFF0C\u5E76\u66F4\u5BB9\u6613\u5730\u53D1\u73B0\u548C\u4FEE\u590D\u9519\u8BEF\u3002\
  ."
title: "\u91CD\u6784\u4EE3\u7801"
weight: 19
---

## 什么 & 为什么？
重构是一种调整现有代码的过程，目的是改善其结构、可读性和性能，而不改变其外部行为。程序员进行重构是为了使代码更易于维护，简化新功能的添加，并更容易地发现和修复错误。

## 如何操作：
这里有一个Kotlin代码片段，展示了一个常见的代码异味及其重构后的版本。我们从一大块做了太多事情的代码开始：

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("订单ID: ${order.id}")
        // 计算订单总额
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // 应用折扣
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("总计: $total")
        // 更多处理...
    }
}
```

为了更好的可读性和关注点分离而重构：

```kotlin
fun printOrderSummary(order: Order) {
    print("订单ID: ${order.id}")
    val total = calculateTotal(order)
    print("总计: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

这里没有示例输出，因为我们没有改变功能，但代码的可读性和可维护性得到了巨大的提升！

## 深入了解
重构作为一个概念自编程开始以来就存在，但直到1990年代，尤其是在Martin Fowler于1999年出版《重构：改善既有代码的设计》之后，才真正作为一门学科蓬勃发展。这本书为这种实践命名，并定义了一种有组织的应用方法，包括重构技术的目录。

与重构的替代方案相比：你可以从头开始重写代码（风险高且耗时），或者简单地进行附加更改（导致软件膨胀和潜在的技术债务）。重构找到了甜点——它在保持低风险的同时，现代化并清理代码。

在实施方面，开始重构之前拥有一套健全的测试是至关重要的，以确保你不会意外改变程序的行为。许多现代IDE（包括Kotlin的IntelliJ）都有自动化重构工具来重命名变量、提取方法等，这可以加速过程并减少错误。

## 另见
- 《重构：改善既有代码的设计》Martin Fowler著（关于这个主题的基础工作）
- Kotlin代码约定文档：[https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html)（了解Kotlin代码清洁的“Kotlin方式”）
- JetBrains在IntelliJ IDEA中对重构的支持：[https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html)（关于实际重构工具使用的实用指南）
- Google关于大规模重构的指南：[https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html)（关于应对更大重构挑战的见解）
