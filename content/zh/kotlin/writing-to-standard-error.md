---
title:                "写入标准错误"
html_title:           "Kotlin: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误，为什么程序员要这么做？
标准错误是指程序运行过程中发生错误时输出的信息。程序员通常会写入标准错误来帮助调试代码，以便更轻松地找出程序的错误原因。

## 如何进行标准错误输出
示例代码：

```Kotlin
fun main() {
    println("这是标准输出")
    System.err.println("这是标准错误输出")
}
```

输出结果：

```
这是标准输出
这是标准错误输出
```

## 深入了解
1. 历史背景：标准错误的概念来源于Unix系统，现在已在大多数编程语言中得到应用。
2. 备选方案：除了使用标准错误，程序员还可以使用日志记录或调试工具来查找错误。
3. 实现细节：标准错误是通过System类中的方法来实现，例如System.err.println()。

## 参考资料
- [Kotlin官方文档](https://kotlinlang.org/docs/tutorials/command-line.html#using-standard-error)
- [标准输出与标准错误的区别](https://www.liaoxuefeng.com/wiki/1252599548343744/1264738733521088)