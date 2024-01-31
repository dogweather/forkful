---
title:                "使用关联数组"
date:                  2024-01-30T19:11:21.298329-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

关联数组，在 Go 中称为映射（maps），让您能够使用键值对来存储和访问数据。它们对于管理可以通过唯一键快速查找值的集合至关重要，简化了程序中的数据操作和检索。

## 如何使用：

在 Go 中，映射非常容易使用。这里是一个简单的指南来开始：

1. **声明和初始化映射**

```Go
package main

import "fmt"

func main() {
    // 初始化一个为空的映射，键为字符串类型，值为int类型
    var scores map[string]int
    fmt.Println(scores) // 打印：map[]

    // 声明和初始化一个非空映射
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // 打印：map[green:#00ff00 red:#ff0000]
}
```

2. **添加和访问元素**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // 打印：5
}
```

3. **遍历映射**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // 输出顺序可能会有所不同，因为映射不保证顺序。
}
```

4. **删除元素**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // 删除前

    delete(meals, "lunch")
    fmt.Println(meals) // 删除后
}
```

## 深入探讨

自 Go 1 引入以来，映射提供了一种内置的方式来高效处理关联数组。与有序集合的切片不同，映射是无序的。这意味着映射元素的迭代顺序在不同执行中不保证相同，这是它能够动态处理键值对并具有显著灵活性的权衡。

Go 在底层将映射实现为哈希表，确保在大多数情况下，访问、插入和删除操作的平均复杂度为 O(1)。然而，值得注意的是，这种效率可能会因诸如哈希冲突等因素而有所不同。

对于需要有序键遍历的用例，您可能会考虑结合映射和切片使用，或探索提供额外数据结构（如有序映射或树）的第三方包。尽管有其限制，Go 的映射对于许多编程场景而言仍是一个强大且必不可少的工具。
