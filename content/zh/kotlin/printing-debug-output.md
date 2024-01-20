---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

1. 打印调试输出是一种使程序呈现运行过程和结果的方法。
2. 程序员使用这个方法来捕捉和理解代码中的潜在问题。

## 如何做:

在 Kotlin 中，我们可以通过使用 `println()` 函数输出调试信息。看这个简单的代码例子：

```Kotlin
fun main() {
    val name = "Kotlin"
    println("Hello, $name!")
}
```

当你运行这个程序，你将会在控制台看到:

```Output
Hello, Kotlin!
```

## 深入了解:

1. 历史背景:
从编程最初的日子开始，打印调试输出一直是程序员的基本工具之一。

2. 替代方案:
虽然 `println()` 是最常用的，但 Kotlin 也允许你使用 `print()` 函数。这个函数的工作方式几乎与 `println()` 相同，但它不会在输出结束时添加换行符。

3. 实现细节:
`println()` 是 `print()` 的扩展。在打印字符串后，`println()` 还会打印一个行终止符。

## 可参见:

- Kotlin 文档: [Basic Syntax](https://kotlinlang.org/docs/basic-syntax.html)