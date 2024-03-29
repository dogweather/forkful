---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:43.216852-07:00
description: "\u5728Elixir\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u6620\
  \u5C04\uFF08Maps\uFF09\uFF0C\u5B83\u4EEC\u662F\u7531\u552F\u4E00\u952E\u6307\u5411\
  \u503C\u7684\u952E-\u503C\u5BF9\u96C6\u5408\u3002\u5BF9\u4E8E\u5373\u65F6\u5B58\u50A8\
  \u548C\u68C0\u7D22\u6570\u636E\u975E\u5E38\u65B9\u4FBF\uFF0C\u4F7F\u60A8\u7684\u4EE3\
  \u7801\u66F4\u52A0\u7B80\u6D01\uFF0C\u8BA9\u60A8\u7684\u751F\u6D3B\u66F4\u52A0\u8F7B\
  \u677E\u3002"
lastmod: '2024-03-13T22:44:47.344420-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elixir\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u6620\
  \u5C04\uFF08Maps\uFF09\uFF0C\u5B83\u4EEC\u662F\u7531\u552F\u4E00\u952E\u6307\u5411\
  \u503C\u7684\u952E-\u503C\u5BF9\u96C6\u5408\u3002\u5BF9\u4E8E\u5373\u65F6\u5B58\u50A8\
  \u548C\u68C0\u7D22\u6570\u636E\u975E\u5E38\u65B9\u4FBF\uFF0C\u4F7F\u60A8\u7684\u4EE3\
  \u7801\u66F4\u52A0\u7B80\u6D01\uFF0C\u8BA9\u60A8\u7684\u751F\u6D3B\u66F4\u52A0\u8F7B\
  \u677E\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Elixir中，关联数组被称为映射（Maps），它们是由唯一键指向值的键-值对集合。对于即时存储和检索数据非常方便，使您的代码更加简洁，让您的生活更加轻松。

## 如何操作：

创建一个映射非常直接。您使用`%{}`语法，如下所示：

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

通过使用键来访问值：

```elixir
IO.puts my_map["name"]
```
输出：`Alex`

要添加或更新值，您可以使用`Map.put/3`函数：

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
输出：%{"age" => 32, "location" => "NY", "name" => "Alex"}

使用`Map.delete/2`来移除键同样简单：

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
输出：%{"location" => "NY", "name" => "Alex"}

## 深入探讨

Elixir中的映射是旧的键-值存储类型的演进，比如Ruby中的散列表或Python中的字典。它们允许更高效的查找和插入，使其成为现代Elixir编程的首选。值得注意的是，在映射出现之前，Elixir使用了已经弃用的HashDict和Dict模块。

然而，对于需要有序数据的场景，您可能会看到Elixir中的关键字列表。这些是元组列表，对于较小的集合来说效率很高，但对于大数据集来说，性能不如映射友好。

请记住，映射以"扁平"结构存储它们的键，直接访问嵌套值有点棘手。对于深层嵌套，您可能会考虑通过`get_in`、`put_in`、`update_in`和`get_and_update_in`函数结构化访问，这些方法允许对嵌套数据进行更动态地操作。

总之，虽然映射是您在Elixir中处理关联数组需求的首选，但该语言为每种场景提供了丰富的数据结构，鼓励您为任务选择正确的工具。
