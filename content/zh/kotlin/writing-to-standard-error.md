---
title:                "用中文翻译就好了没有进一步的评论"
html_title:           "Kotlin: 用中文翻译就好了没有进一步的评论"
simple_title:         "用中文翻译就好了没有进一步的评论"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

众所周知，Kotlin是一种流行的编程语言，它可以在多个应用领域使用。但是，有时候当我们在编写代码时，会遇到一些不可避免的错误。这时候，我们可以通过将错误信息输出到标准错误来帮助我们调试代码。在这篇文章中，我们将介绍如何在Kotlin中将信息输出到标准错误，并深入探讨这一过程的原理。

## 怎么做

在Kotlin中，我们可以使用标准错误流(System.err)来输出错误信息。下面是一个简单的例子：

```Kotlin
fun main() {
    System.err.println("这是一个测试错误信息。")
}
```

运行以上代码，我们可以在控制台中看到输出的错误信息。在这个例子中，我们使用了println()函数来输出信息，但是将信息输出到标准错误流而不是标准输出流(System.out)。

## 深入探讨

标准错误流和标准输出流的主要区别在于，标准输出流主要用来输出程序的运行结果，而标准错误流主要用来输出程序的错误信息。当程序出现异常或错误时，它们将会被输出到标准错误流。因此，通过将信息输出到标准错误流，我们可以更好地了解程序在运行过程中出现的问题，从而更快地调试代码。

除了使用println()函数之外，我们也可以使用print()或write()函数来输出信息到标准错误流。此外，我们还可以使用System.setErr()函数来指定输出流，从而将信息输出到指定的地方。这些方法可能对于一些高级的调试场景会更加有用。

## 查看更多

了解更多关于Kotlin的知识，请访问以下链接：

- [Kotlin官方网站](https://kotlinlang.org)
- [Kotlin中文社区](https://kotlincn.netlify.app)
- [Kotlin语言参考文档](https://kotlinlang.org/docs/reference/)

## 参考链接

了解更多关于将信息输出到标准错误的知识，请参考以下链接：

- [Kotlin标准库文档：System类](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.system/index.html)
- [Stack Overflow上关于输出到标准错误的问题讨论](https://stackoverflow.com/questions/16204448/is-there-a-simple-method-to-write-to-stderror-in-kotlin/16204533#16204533)