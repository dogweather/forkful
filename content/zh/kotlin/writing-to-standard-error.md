---
title:                "Kotlin: “标准错误的写作”"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

当我们在进行软件开发时，经常会遇到一些错误信息。这些错误信息可以帮助我们更快地找出问题所在，从而加快调试和修复的速度。而使用Kotlin的开发者可以通过写入标准错误来输出这些错误信息，帮助他们更快地发现和解决问题。

## 如何做

### 使用系统方法

Kotlin提供了一个系统方法`System.err.println()`来向标准错误输出信息。例如，我们想在控制台输出错误信息`Error: File Not Found`，我们可以这样写：

```Kotlin
System.err.println("Error: File Not Found")
```

运行上述代码，控制台将输出：

```
Error: File Not Found
```

### 使用标准库

除了系统方法外，Kotlin也提供了一个`PrintWriter`类来向标准错误输出信息。使用`PrintWriter`可以更方便地格式化输出信息，并可以指定输出的位置。例如，我们想在一个文件中输出错误信息`Error: Invalid Input`，可以这样写：

```Kotlin
val file = File("error.log") // 创建一个文件
PrintWriter(file).use { out ->
    out.println("Error: Invalid Input")
}
```

运行上述代码，我们将在`error.log`文件中看到输出的错误信息。

## 深入了解

正如我们所见，通过写入标准错误，我们可以快速输出错误信息。但需要注意的是，标准错误的输出会影响程序的运行速度。因此，在实际开发中，我们需要根据需要来决定是否使用标准错误输出信息。

## 参考链接

- [Kotlin官网](https://kotlinlang.org/)
- [Kotlin标准库文档](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Java标准库文档](https://docs.oracle.com/en/java/javase/11/docs/api/index.html)

## 参见