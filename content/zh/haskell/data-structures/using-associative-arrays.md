---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:34.148054-07:00
description: "Haskell \u4E2D\u7684\u5173\u8054\u6570\u7EC4\u6216\u5B57\u5178\uFF0C\
  \u5176\u6838\u5FC3\u529F\u80FD\u662F\u4E3A\u4E86\u5B9E\u73B0\u952E\u503C\u5BF9\u7684\
  \u5FEB\u901F\u67E5\u627E\u548C\u9AD8\u6548\u6570\u636E\u7BA1\u7406\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u5B83\u4EEC\u6765\u5904\u7406\u6210\u5BF9\u5143\u7D20\u7684\u96C6\
  \u5408\uFF0C\u4E0E\u5217\u8868\u76F8\u6BD4\uFF0C\u67E5\u627E\u5143\u7D20\u8F7B\u800C\
  \u6613\u4E3E\u3002"
lastmod: '2024-03-13T22:44:47.807454-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u4E2D\u7684\u5173\u8054\u6570\u7EC4\u6216\u5B57\u5178\uFF0C\u5176\
  \u6838\u5FC3\u529F\u80FD\u662F\u4E3A\u4E86\u5B9E\u73B0\u952E\u503C\u5BF9\u7684\u5FEB\
  \u901F\u67E5\u627E\u548C\u9AD8\u6548\u6570\u636E\u7BA1\u7406\u3002\u7A0B\u5E8F\u5458\
  \u4F7F\u7528\u5B83\u4EEC\u6765\u5904\u7406\u6210\u5BF9\u5143\u7D20\u7684\u96C6\u5408\
  \uFF0C\u4E0E\u5217\u8868\u76F8\u6BD4\uFF0C\u67E5\u627E\u5143\u7D20\u8F7B\u800C\u6613\
  \u4E3E\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么和为什么？

Haskell 中的关联数组或字典，其核心功能是为了实现键值对的快速查找和高效数据管理。程序员使用它们来处理成对元素的集合，与列表相比，查找元素轻而易举。

## 如何操作：

Haskell 并没有像某些其他语言那样直接提供关联数组，但它提供了一个强大的标准库 `Data.Map` 来处理键值对。让我们动手看看如何使用它们！

首先，确保导入它：
```Haskell
import qualified Data.Map as Map
```

创建一个映射非常简单。让我们创建一个包含一些编程语言及其范式的映射：
```Haskell
let languages = Map.fromList [("Haskell", "Functional"), ("Python", "Imperative"), ("Prolog", "Logical")]
```

现在，如何获取 Haskell 的范式呢？
```Haskell
Map.lookup "Haskell" languages
-- 输出：Just "Functional"
```

添加新语言很简单：
```Haskell
let languagesUpdated = Map.insert "Rust" "Systems" languages
```

如果我们想列出所有语言怎么办？使用 `Map.keys`：
```Haskell
Map.keys languagesUpdated
-- 输出：["Haskell","Python","Prolog","Rust"]
```

要列出范式，使用 `Map.elems`：
```Haskell
Map.elems languagesUpdated
-- 输出：["Functional","Imperative","Logical","Systems"]
```

这些基本操作应该覆盖了大多数用例，但 `Data.Map` 中还有更多内容值得探索！

## 深入了解

Haskell 标准库中的 `Data.Map` 模块建立在平衡二叉树之上，特别是 AVL 树。这种选择确保了对映射的大多数操作，例如插入、删除和查找，可以在 O(log n) 时间内完成，其中 n 是映射中元素的数量。这是许多用例的高效选择，尽管并非对所有场景都是绝对最快的。

还有一个历史细节：在 `Data.Map` 成为首选之前，Haskell 程序员经常使用成对列表来模拟关联数组。然而，对这种结构的操作在查找时是 O(n) 的，使得 `Data.Map` 在性能方面是一大改进。

现在，尽管 `Data.Map` 的效率和实用性很高，它并不总是每项工作的最佳工具。对于那些高度关注性能的任务，即使是 O(log n) 的查找时间也太慢，或者当键始终是整数值时，数组或哈希表（通过 `Data.HashMap`）可能以 O(1) 的访问时间提供更好的性能。

Haskell 生态系统允许使用各种数据结构来满足不同的需求，而 `Data.Map` 是关联数组的一个出色的通用选择，它平衡了易用性、灵活性和性能。
