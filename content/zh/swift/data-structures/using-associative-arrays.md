---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:16.325627-07:00
description: "\u5728 Swift \u4E2D\uFF0C\u88AB\u79F0\u4E3A\u5B57\u5178\u7684\u5173\u8054\
  \u6570\u7EC4\uFF0C\u8BA9\u60A8\u80FD\u591F\u4EE5\u952E\u503C\u5BF9\u7684\u5F62\u5F0F\
  \u5B58\u50A8\u548C\u7BA1\u7406\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\
  \u4EEC\u6765\u9AD8\u6548\u5730\u7EC4\u7EC7\u6570\u636E\uFF0C\u4F7F\u57FA\u4E8E\u5B83\
  \u4EEC\u72EC\u7279\u952E\u7684\u503C\u6613\u4E8E\u8BBF\u95EE\u548C\u64CD\u4F5C\u3002"
lastmod: '2024-03-13T22:44:48.149781-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Swift \u4E2D\uFF0C\u88AB\u79F0\u4E3A\u5B57\u5178\u7684\u5173\u8054\
  \u6570\u7EC4\uFF0C\u8BA9\u60A8\u80FD\u591F\u4EE5\u952E\u503C\u5BF9\u7684\u5F62\u5F0F\
  \u5B58\u50A8\u548C\u7BA1\u7406\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\
  \u4EEC\u6765\u9AD8\u6548\u5730\u7EC4\u7EC7\u6570\u636E\uFF0C\u4F7F\u57FA\u4E8E\u5B83\
  \u4EEC\u72EC\u7279\u952E\u7684\u503C\u6613\u4E8E\u8BBF\u95EE\u548C\u64CD\u4F5C\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 什么 & 为什么？

在 Swift 中，被称为字典的关联数组，让您能够以键值对的形式存储和管理数据。程序员使用它们来高效地组织数据，使基于它们独特键的值易于访问和操作。

## 如何操作：

Swift 使得操作关联数组变得直截了当。以下是你如何在 Swift 字典中声明、添加、移除和访问项目的方法：

```Swift
// 声明一个字典
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// 添加一个新项目
fruitColors["Grape"] = "Purple"

// 使用其键访问一个值
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // 输出：Apple is Red.
} else {
    print("Color not found.")
}

// 移除一个项目
fruitColors["Banana"] = nil  // 这将从字典中移除“Banana”

// 迭代项目
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // 输出：
    // Apple is Red.
    // Grape is Purple.
}
```

字典极其灵活，允许您动态地操纵和访问数据。它们的无序性不影响数据检索的速度，这在处理大数据集时是一个重大优势。

## 深入探讨

Swift 将字典作为关联数组的实现源自它们将唯一键映射到值的强大能力。从历史上看，编程语言已经以各种名称（如哈希表或映射）实现了这一概念，暗示它们创建键与值之间“映射”的功能。

在 Swift 中，字典针对性能进行了优化，利用可哈希键进行高效数据检索。这意味着在 `[Key: Value]` 字典中的 `Key` 类型必须遵循 `Hashable` 协议，这适用于大多数 Swift 标准类型，如 `Int`、`String` 和 `Double`。

需要考虑的一点是，尽管字典非常适合关联数据对，但它们缺乏顺序。如果您需要维护元素的顺序，可能会探索像 `Array` 这样的替代品，用于有序元素序列，或者自定义数据结构，这些数据结构结合了数组和字典的特性。

同样值得注意的是，Swift 在不断发展，它对字典的处理和优化也在不断进步。因此，持续更新最新的 Swift 文档对于充分利用字典至关重要，确保您使用最高效、最新的实践。
