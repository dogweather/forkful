---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么?
在Kotlin中，写入文本文件意味着用代码将字符串存到一个文本文件中。程序员这么做是为了保存数据、记录日志或配置信息。

## 怎么做:
```kotlin
import java.io.File

fun main() {
    val textContent = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textContent)
}
```
运行上述代码会在同一目录下创建`example.txt`文件，里面含有内容`Hello, Kotlin file writing!`。

## 深入探索:
写入文件在Kotlin前就存在了，Java的`java.io`类库为此奠定了基础。Kotlin在Java基础上建设，可用`java.io.File`或`java.nio.file.Files`等工具，但提供更简洁的API。如`writeText`和`printWriter`等Kotlin扩展函数，简化了代码。除了同步写操作，Kotlin Coroutines 支持异步写操作，适用于大文件或网络I/O防止UI线程阻塞。

## 看此:
- Kotlin官方文档: [文件操作](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- `java.io.File`类参考: [Java File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Kotlin Coroutines: [Kotlin 协程](https://kotlinlang.org/docs/reference/coroutines-overview.html)
