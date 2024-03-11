---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:59.078163-07:00
description: "\u5728Go\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u6620\
  \u5C04\uFF08maps\uFF09\uFF0C\u5B83\u5141\u8BB8\u4F60\u5B58\u50A8\u952E\u503C\u5BF9\
  \uFF0C\u5176\u4E2D\u6BCF\u4E2A\u552F\u4E00\u7684\u952E\u6620\u5C04\u5230\u4E00\u4E2A\
  \u503C\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u6620\u5C04\u6765\u9AD8\u6548\u5730\u68C0\
  \u7D22\u3001\u4FEE\u6539\u6570\u636E\uFF0C\u4EE5\u53CA\u7EF4\u62A4\u4E00\u4E2A\u53EF\
  \u4EE5\u901A\u8FC7\u552F\u4E00\u952E\u5FEB\u901F\u8BBF\u95EE\u7684\u5143\u7D20\u96C6\
  \u5408\u3002"
lastmod: '2024-03-11T00:14:20.886561-06:00'
model: gpt-4-0125-preview
summary: "\u5728Go\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u6620\u5C04\
  \uFF08maps\uFF09\uFF0C\u5B83\u5141\u8BB8\u4F60\u5B58\u50A8\u952E\u503C\u5BF9\uFF0C\
  \u5176\u4E2D\u6BCF\u4E2A\u552F\u4E00\u7684\u952E\u6620\u5C04\u5230\u4E00\u4E2A\u503C\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u6620\u5C04\u6765\u9AD8\u6548\u5730\u68C0\u7D22\
  \u3001\u4FEE\u6539\u6570\u636E\uFF0C\u4EE5\u53CA\u7EF4\u62A4\u4E00\u4E2A\u53EF\u4EE5\
  \u901A\u8FC7\u552F\u4E00\u952E\u5FEB\u901F\u8BBF\u95EE\u7684\u5143\u7D20\u96C6\u5408\
  \u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在Go中，关联数组被称为映射（maps），它允许你存储键值对，其中每个唯一的键映射到一个值。程序员使用映射来高效地检索、修改数据，以及维护一个可以通过唯一键快速访问的元素集合。

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
