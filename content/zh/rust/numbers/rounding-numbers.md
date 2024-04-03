---
date: 2024-01-26 03:46:56.049671-07:00
description: "\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u5B83\u4EEC\
  \u8C03\u6574\u5230\u6700\u63A5\u8FD1\u7684\u6574\u6570\u6216\u5177\u6709\u7279\u5B9A\
  \u7CBE\u5EA6\u7684\u5206\u6570\u3002\u7A0B\u5E8F\u5458\u56DB\u820D\u4E94\u5165\u6570\
  \u5B57\u662F\u4E3A\u4E86\u7B80\u5316\u503C\u4EE5\u65B9\u4FBF\u4EBA\u7C7B\u9605\u8BFB\
  \uFF0C\u6EE1\u8DB3\u89C4\u8303\u8981\u6C42\uFF0C\u6216\u51CF\u5C11\u6D6E\u70B9\u64CD\
  \u4F5C\u4E2D\u7684\u8BA1\u7B97\u5F00\u9500\u3002"
lastmod: '2024-03-13T22:44:47.514631-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5B57\u56DB\u820D\u4E94\u5165\u610F\u5473\u7740\u5C06\u5B83\u4EEC\
  \u8C03\u6574\u5230\u6700\u63A5\u8FD1\u7684\u6574\u6570\u6216\u5177\u6709\u7279\u5B9A\
  \u7CBE\u5EA6\u7684\u5206\u6570\u3002\u7A0B\u5E8F\u5458\u56DB\u820D\u4E94\u5165\u6570\
  \u5B57\u662F\u4E3A\u4E86\u7B80\u5316\u503C\u4EE5\u65B9\u4FBF\u4EBA\u7C7B\u9605\u8BFB\
  \uFF0C\u6EE1\u8DB3\u89C4\u8303\u8981\u6C42\uFF0C\u6216\u51CF\u5C11\u6D6E\u70B9\u64CD\
  \u4F5C\u4E2D\u7684\u8BA1\u7B97\u5F00\u9500\u3002."
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 什么 & 为什么？
数字四舍五入意味着将它们调整到最接近的整数或具有特定精度的分数。程序员四舍五入数字是为了简化值以方便人类阅读，满足规范要求，或减少浮点操作中的计算开销。

## 如何操作：
Rust 让四舍五入变得易如反掌。看看这些适用于 `f32` 或 `f64` 类型的方法：

```rust
fn main() {
    let num = 2.34567;

    // 四舍五入到最近的整数
    let round = num.round();
    println!("四舍五入: {}", round); // 四舍五入: 2

    // 向下取整 - 小于或等于数字的最大整数
    let floor = num.floor();
    println!("向下取整: {}", floor); // 向下取整: 2

    // 向上取整 - 大于或等于数字的最小整数
    let ceil = num.ceil();
    println!("向上取整: {}", ceil); // 向上取整: 3

    // 截断 - 没有小数部分的整数部分
    let trunc = num.trunc();
    println!("截断: {}", trunc); // 截断: 2

    // 四舍五入到最接近的十的幂的倍数
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("四舍五入到2位小数: {}", multiple_of_ten); // 四舍五入到2位小数: 2.35
}
```

## 深入了解
从历史上看，四舍五入对于在有限的数字空间内适应无限小数或无理数至关重要——对于内存匮乏的古代计算机来说是必须的。想想算盘，但少了一些巧妙，多了一些数学。

除了Rust原生方法外的备选方案包括：
1. `format!` 宏用于字符串格式化，默认进行四舍五入。
2. 用于特殊数学任务的外部 crates，比如具有更细粒度控制的 `round` crate。

在底层，Rust 的四舍五入操作符合 IEEE 标准——技术行话意味着“它像你的数学老师希望的那样进行四舍五入”。另外，由于二进制表示法，一些数字（如0.1）由于在二进制中的无限表示，不能采用传统方式四舍五入。

## 参见
- Rust 文档关于原始类型方法的介绍：https://doc.rust-lang.org/std/primitive.f64.html
- IEEE 浮点算术标准 (IEEE 754)：https://ieeexplore.ieee.org/document/4610935
- 更复杂的四舍五入 "round" crate：https://crates.io/crates/round
