---
title:                "使用关联数组"
aliases:
- /zh/elm/using-associative-arrays/
date:                  2024-01-30T19:10:54.463958-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

关联数组，在 Elm 中被称为字典(Dictionary)，是以一种能够使查找、插入和删除操作非常迅速的方式将键映射到值的数据结构。当你需要跟踪一些没有严格顺序的东西时，如用户偏好设置或库存列表，字典是你的首选。

## 如何操作：

在 Elm 中，你通过 `Dict` 模块来操作字典，让我们来看一个简单的例子：

```Elm
import Dict exposing (Dict)

-- 使用 String 作为键，Int 作为值初始化一个字典
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- 添加或更新一个值
updatedDict = Dict.insert "grape" 10 exampleDict

-- 检索一个值（注意 Maybe 类型，因为键可能不存在）
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- 删除一个键值对
finalDict = Dict.remove "banana" updatedDict

-- 将字典转换回列表
dictToList = Dict.toList finalDict
```

当展示 `dictToList` 时的样例输出：

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

这展示了基本操作：创建、更新、访问和迭代一个字典。

## 深入探索

Elm 中的字典内部使用了一种称为 AVL 树的结构 - 一种自平衡的二叉查找树。这种选择在确保像插入、获取和移除这样的操作有良好性能（对数时间复杂度）与保持处理数据的简单性之间取得了平衡。

尽管 Elm 的 `Dict` 具有强大的功能，但它并不是万能的解决方案。对于有序的或需要按顺序迭代的集合，列表(List)或数组(Array)可能更合适。此外，当处理一组已知的固定键集时，使用自定义类型（Elm 的枚举版本）可以提供更多的类型安全性和代码意图的清晰性。

在 Elm 的生态系统中，`Dict` 提供了一种管理键值对集合的可靠方式，其中键是唯一的且顺序不重要。虽然可能会出现更新颖或更复杂的结构，但 `Dict` 模块作为 Elm 程序员工具箱中的基本工具，因其处理关联数组的简单性和效率而保持其根本地位。
