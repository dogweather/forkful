---
title:    "Gleam: 打印调试输出"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 为什么要使用Gleam打印调试输出

在编程过程中，调试是非常重要的一部分。通过打印调试输出，我们可以更清楚地了解程序的运行情况，从而更容易找到可能存在的问题。使用Gleam可以轻松地打印各种类型的调试信息，使调试过程变得更加高效、快速。

## 如何打印调试输出

Gleam提供了多种方法来打印调试输出，下面将介绍几种常用的方式。

### 使用 `gleam.debug` 函数

在Gleam中，我们可以使用 `gleam.debug` 函数来打印调试输出。该函数接受一个任意类型的参数，然后将其转换为字符串并打印出来。例如：

```Gleam
let name = "John"
let age = 25

gleam.debug("Hello, I am " ++ name ++ " and I am " ++ age ++ " years old.")
```

输出结果为：

```
Hello, I am John and I am 25 years old.
```

### 使用 `k ! g` 语句

除了使用 `gleam.debug` 函数，我们也可以使用 `k ! g` 语句来打印调试输出。这种方式更加简洁，但只能打印字符串类型的调试信息。例如：

```Gleam
let name = "John"

k ! g "My name is " ++ name
```

输出结果为：

```
My name is John
```

## 深入了解打印调试输出

除了以上两种方式，Gleam还提供了更多灵活的方法来打印调试输出。例如，我们可以使用 `gleam.debug_list` 函数来打印含有多个元素的列表：

```Gleam
let fruits = ["apple", "orange", "banana"]

gleam.debug_list("My favorite fruits are: ", fruits)
```

输出结果为：

```
My favorite fruits are: [apple, orange, banana]
```

此外，我们还可以使用 `gleam.debug_map` 函数来打印含有键值对的映射：

```Gleam
let scores = { "math": 95, "english": 90, "history": 85 }

gleam.debug_map("My scores are: ", scores)
```

输出结果为：

```
My scores are: {math => 95, english => 90, history => 85}
```

## 参考链接

- [Gleam 官方文档](https://gleam.run)
- [Gleam 调试模式介绍](https://gleam.run/documentation/debugging.md)
- [Gleam 调试输出函数文档](https://gleam.run/documentation/commands.md#gleamdebug)
- [Gleam 列表和映射文档](https://gleam.run/documentation/collections.md) 

# 请参阅

- [了解更多Gleam功能](https://gleam.run/documentation/getting_started.md)
- [Gleam社区论坛](https://gleam.run/community/)
- [Gleam教程合集](https://gleam.run/tutorials/)