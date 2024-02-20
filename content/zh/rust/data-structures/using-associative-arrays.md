---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:51.410016-07:00
description: "\u5173\u8054\u6570\u7EC4\uFF0C\u6216\u8005 Rust \u7231\u597D\u8005\u79F0\
  \u4E4B\u4E3A\u201C\u54C8\u5E0C\u6620\u5C04\u201D\uFF0C\u662F\u4EE5\u952E\u503C\u5BF9\
  \u5F62\u5F0F\u5B58\u50A8\u6570\u636E\u7684\u96C6\u5408\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528\u5B83\u4EEC\u53EF\u4EE5\u5FEB\u901F\u67E5\u627E\u6570\u636E\uFF0C\u5141\u8BB8\
  \u57FA\u4E8E\u552F\u4E00\u952E\u9AD8\u6548\u5730\u64CD\u4F5C\u6570\u636E\u3002"
lastmod: 2024-02-19 22:05:06.529897
model: gpt-4-0125-preview
summary: "\u5173\u8054\u6570\u7EC4\uFF0C\u6216\u8005 Rust \u7231\u597D\u8005\u79F0\
  \u4E4B\u4E3A\u201C\u54C8\u5E0C\u6620\u5C04\u201D\uFF0C\u662F\u4EE5\u952E\u503C\u5BF9\
  \u5F62\u5F0F\u5B58\u50A8\u6570\u636E\u7684\u96C6\u5408\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528\u5B83\u4EEC\u53EF\u4EE5\u5FEB\u901F\u67E5\u627E\u6570\u636E\uFF0C\u5141\u8BB8\
  \u57FA\u4E8E\u552F\u4E00\u952E\u9AD8\u6548\u5730\u64CD\u4F5C\u6570\u636E\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
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
