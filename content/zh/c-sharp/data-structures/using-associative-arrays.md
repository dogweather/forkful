---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:10.148902-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728 C# \u4E2D\uFF0C\u60A8\u53EF\u4EE5\u901A\
  \u8FC7\u4F7F\u7528 `Dictionary<TKey, TValue>` \u7C7B\u6765\u64CD\u4F5C\u5173\u8054\
  \u6570\u7EC4\u3002\u8FD9\u91CC\u662F\u4E00\u4E2A\u5FEB\u901F\u5165\u95E8\u793A\u4F8B\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.759647-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\uFF0C\u60A8\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528 `Dictionary<TKey,\
  \ TValue>` \u7C7B\u6765\u64CD\u4F5C\u5173\u8054\u6570\u7EC4\u3002\u8FD9\u91CC\u662F\
  \u4E00\u4E2A\u5FEB\u901F\u5165\u95E8\u793A\u4F8B\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作:
在 C# 中，您可以通过使用 `Dictionary<TKey, TValue>` 类来操作关联数组。这里是一个快速入门示例：

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // 创建一个词典
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // 添加键值对
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // 使用键访问值
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // 更新值
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // 删除键值对
        fruitBasket.Remove("Oranges");

        // 遍历词典
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
示例输出：
```
Apples: 5
Updated Apples: 7
Apples: 7
```

这个示例展示了创建词典、添加、访问、更新和删除元素，以及遍历它的过程。

## 深入探索
关联数组的概念可以追溯到它们在像 Perl 和 PHP 这样的脚本语言中的使用，其中它们在管理数据集合时提供了灵活性。在 C# 中，`Dictionary<TKey, TValue>` 是事实上的实现，自 .NET Framework 2.0 引入。它将数据存储在哈希表中，确保了高效的查找、添加和删除操作。

然而，值得注意的是，尽管词典非常灵活，它们可能并不总是您的最佳选择。对于维护有序集合，您可能会看一下 `SortedDictionary<TKey, TValue>` 或 `SortedList<TKey, TValue>`，它们在保持排序顺序的同时以较慢的插入和删除操作为代价。对于需要线程安全的场景，`ConcurrentDictionary<TKey, TValue>` 增加了开销，但确保了多线程之间的安全访问而无需手动锁定。

最终，C# 中关联数组实现的选择取决于您对顺序、性能和线程安全的具体需求。
