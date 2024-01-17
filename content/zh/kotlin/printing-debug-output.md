---
title:                "打印调试输出"
html_title:           "Kotlin: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
打印调试输出是一种在程序中显示信息以帮助程序员进行调试的方法。程序员会使用这种方法来查看程序运行时的值，以及检查代码是否按照预期执行。

## 如何:
下面是使用Kotlin语言打印调试输出的示例代码和样本输出。

```Kotlin
fun main() {
    // 使用println()函数打印简单的文本输出
    println("Hello World!")

    // 声明一个变量并打印其值
    val num = 10
    println(num)
}
```

输出:
```
Hello World!
10
```

## 深入探究:
在过去，程序员会使用像log（）或print（）这样的函数来打印调试输出。但现在，大多数编程语言都提供了内置的打印函数，使得打印调试输出更加方便。当然，你也可以使用自定义的log和print函数来打印调试输出。

除了使用内置的打印函数，程序员也可以使用调试器来调试他们的代码。调试器是一种工具，允许程序员在代码中设置断点，以便在执行程序时观察变量的值。

## 参考:
- [Kotlin 官方文档](https://kotlinlang.org/docs/)
- [使用调试器调试代码](https://www.jetbrains.com/help/idea/debugging.html)