---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么是创建临时文件？为什么要这么做？
创建临时文件是指在程序运行过程中临时生成的文件。程序员通常会在需要存储临时数据或者进行临时操作时创建临时文件。

## 如何创建临时文件：
创建临时文件十分简单，只需要使用Kotlin的内置函数`createTempFile()`即可。以下是一个示例代码及输出：

```Kotlin
val tempFile = createTempFile()
println(tempFile.absolutePath)
```
Output:
```
/tmp/5074022256578221381.tmp
```

## 深入了解：
创建临时文件的概念并不是局限于Kotlin，它在其他编程语言中也同样适用。除了使用`createTempFile()`函数外，程序员还可以使用`File.createTempFile()`方法来创建临时文件。为了确保安全性，临时文件通常会在程序结束后被自动删除。这样可以避免占用过多的系统资源。

## 参考资料：
- [Kotlin官方文档：创建临时文件](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/create-temp-file.html)
- [Java文件类文档：创建临时文件](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))