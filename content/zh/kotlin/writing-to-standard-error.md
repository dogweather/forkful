---
title:                "Kotlin: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#为什么

在编写Kotlin程序时，我们经常会使用标准错误输出来帮助我们发现代码中的错误和问题。通过使用标准错误输出，我们可以更快地找到并修复代码中的bug，提高开发效率。

##如何做

要在Kotlin中使用标准错误输出，我们可以使用“System.err.println()”方法。这个方法会将我们想要输出的内容打印到标准错误流中，而不是标准输出流。让我们看一个例子：

```Kotlin
fun main() {
    System.err.println("Hello, world!")
}
```

这个程序会在控制台输出“Hello, world!”，但是它会被打印到标准错误流中，而不是标准输出流中。

输出结果：

`Hello, world!`

##深入了解

标准错误输出其实是一种异常处理方法。当程序运行时出现错误或异常，会被捕获并打印到标准错误流中。这有助于我们在调试期间快速发现并修复问题。同时，我们也可以通过重定向标准错误流来处理异常，将它们记录到日志文件中，方便后续分析和处理。

#另请参阅

- Kotlin官方文档： https://kotlinlang.org/docs/reference/
- 标准错误输出教程：https://www.javatpoint.com/kotlin-standard-error-stream
- Kotlin异常处理指南：https://kotlinlang.org/docs/reference/exceptions.html