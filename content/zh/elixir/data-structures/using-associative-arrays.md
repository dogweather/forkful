---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:43.216852-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u4E2D\u7684\u6620\u5C04\u662F\u65E7\
  \u7684\u952E-\u503C\u5B58\u50A8\u7C7B\u578B\u7684\u6F14\u8FDB\uFF0C\u6BD4\u5982\
  Ruby\u4E2D\u7684\u6563\u5217\u8868\u6216Python\u4E2D\u7684\u5B57\u5178\u3002\u5B83\
  \u4EEC\u5141\u8BB8\u66F4\u9AD8\u6548\u7684\u67E5\u627E\u548C\u63D2\u5165\uFF0C\u4F7F\
  \u5176\u6210\u4E3A\u73B0\u4EE3Elixir\u7F16\u7A0B\u7684\u9996\u9009\u3002\u503C\u5F97\
  \u6CE8\u610F\u7684\u662F\uFF0C\u5728\u6620\u5C04\u51FA\u73B0\u4E4B\u524D\uFF0CElixir\u4F7F\
  \u7528\u4E86\u5DF2\u7ECF\u5F03\u7528\u7684HashDict\u548CDict\u6A21\u5757\u3002\u2026"
lastmod: '2024-04-05T22:51:00.571074-06:00'
model: gpt-4-0125-preview
summary: "\u7136\u800C\uFF0C\u5BF9\u4E8E\u9700\u8981\u6709\u5E8F\u6570\u636E\u7684\
  \u573A\u666F\uFF0C\u60A8\u53EF\u80FD\u4F1A\u770B\u5230Elixir\u4E2D\u7684\u5173\u952E\
  \u5B57\u5217\u8868\u3002\u8FD9\u4E9B\u662F\u5143\u7EC4\u5217\u8868\uFF0C\u5BF9\u4E8E\
  \u8F83\u5C0F\u7684\u96C6\u5408\u6765\u8BF4\u6548\u7387\u5F88\u9AD8\uFF0C\u4F46\u5BF9\
  \u4E8E\u5927\u6570\u636E\u96C6\u6765\u8BF4\uFF0C\u6027\u80FD\u4E0D\u5982\u6620\u5C04\
  \u53CB\u597D\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
