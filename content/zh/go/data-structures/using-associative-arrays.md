---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:59.078163-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728Go\u4E2D\u521B\u5EFA\u548C\u521D\u59CB\
  \u5316\u6620\u5C04\u53EF\u4EE5\u901A\u8FC7\u591A\u79CD\u65B9\u5F0F\u5B8C\u6210\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\u6765\u5E2E\u52A9\u4F60\u5F00\
  \u59CB\uFF1A."
lastmod: '2024-03-13T22:44:47.133594-06:00'
model: gpt-4-0125-preview
summary: "\u5728Go\u4E2D\u521B\u5EFA\u548C\u521D\u59CB\u5316\u6620\u5C04\u53EF\u4EE5\
  \u901A\u8FC7\u591A\u79CD\u65B9\u5F0F\u5B8C\u6210\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u57FA\u672C\u793A\u4F8B\u6765\u5E2E\u52A9\u4F60\u5F00\u59CB\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作:
在Go中创建和初始化映射可以通过多种方式完成。这里有一个基本示例来帮助你开始：

```go
package main

import "fmt"

func main() {
    // 声明并初始化一个映射
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // 输出: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

要添加或更新元素，你可以这样给一个键赋值：

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// 输出: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

通过键访问一个值很直接：

```go
fmt.Println("红色的十六进制代码是:", colors["red"])
// 输出: 红色的十六进制代码是: #FF0000
```

要删除一个元素，使用`delete`函数：

```go
delete(colors, "red")
fmt.Println(colors)
// 输出: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

遍历映射使用for循环：

```go
for color, hex := range colors {
    fmt.Printf("键: %s 值: %s\n", color, hex)
}
```

记住，在Go中映射是无序的。迭代的顺序不是固定的。

## 深入探究
在Go中，映射是通过哈希表实现的。映射中的每个条目都包括两项：一个键和一个值。键被哈希化以存储条目，这允许对小数据集执行常数时间操作，平均时间复杂性为O(1)，适当的哈希可以降低到O(n)的最坏情况，这是由于许多哈希冲突导致的。

对新手Go程序员来说，一个重要的提示是映射类型是引用类型。这意味着当你把一个映射传递给一个函数时，该函数内对映射的任何更改都对调用者可见。这与比如说，将一个结构体传递给函数不同，除非通过指针传递，否则结构体是被复制的。

虽然映射对于涉及关联数组的大多数用例来说都是非常灵活和高效的，在性能关键型应用程序中，使用具有更可预测性能特性的数据结构可能会更有利，尤其是如果键分布可能导致频繁的冲突。

另一个值得考虑的替代方案是自Go 1.9以来可用的`sync.Map`，它设计用于键只写入一次但多次读取的用例，为这些场景提供了效率改进。然而，对于常规的Go应用程序，常规映射的使用是惯用的，并且经常是推荐的做法，因为它的简单性和在语言中的直接支持。
