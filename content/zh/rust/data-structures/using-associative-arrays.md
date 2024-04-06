---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:51.410016-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Rust \u4E2D\uFF0C`std::collections`\
  \ \u6A21\u5757\u63D0\u4F9B\u7684 `HashMap` \u7C7B\u578B\u63D0\u4F9B\u4E86\u5173\u8054\
  \u6570\u7EC4\u7684\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u60A8\u5982\u4F55\u4F7F\u7528\
  \u5B83\u4EEC\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:47.834141-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
