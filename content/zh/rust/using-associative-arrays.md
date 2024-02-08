---
title:                "使用关联数组"
aliases:
- zh/rust/using-associative-arrays.md
date:                  2024-01-30T19:12:51.410016-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

关联数组，或者 Rust 爱好者称之为“哈希映射”，是以键值对形式存储数据的集合。程序员使用它们可以快速查找数据，允许基于唯一键高效地操作数据。

## 如何操作：

在 Rust 中，`std::collections` 模块提供的 `HashMap` 类型提供了关联数组的功能。以下是您如何使用它们的方法：

```Rust
use std::collections::HashMap;

fn main() {
    // 创建一个新的 HashMap
    let mut scores = HashMap::new();

    // 插入值
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // 访问值
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Blue 队的分数：{}", score); // 输出：Blue 队的分数：10
    }

    // 更新值
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // 遍历键值对
    for (key, value) in &scores {
        println!("{}：{}", key, value); // 输出：Blue: 15, Yellow: 50
    }
}
```

## 深入了解

Rust 中的 `HashMap` 使用哈希函数将键映射到值，这使得数据检索非常快速。然而，这种效率是有成本的：哈希映射不会保持其元素的顺序。这与其他关联数组实现不同，比如在最近版本中的 Python（`dict`）或 Ruby，它们作为一个特性保持了插入顺序。对于键值对的顺序很重要的用例，Rust 开发人员可能会考虑使用 `std::collections` 模块中的 `BTreeMap`，它保持顺序，但可能提供比 `HashMap` 更慢的插入和检索速度。最终，选择 `HashMap` 还是 `BTreeMap` 取决于对顺序和性能的具体需求。
