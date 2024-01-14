---
title:    "Kotlin: 将字符串转换为大写"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么使用Kotlin编写博客

Kotlin是一种现代化的编程语言，它可以帮助您更轻松地编写代码并提高生产力。在本文中，我们将探讨如何使用Kotlin来将字符串转换为大写，并深入了解这项功能的原理。

## 如何使用Kotlin将字符串转换为大写

```Kotlin
fun capitalize(string: String): String { 
  return string.toUpperCase()
} 

println(capitalize("hello, world")) 
```
输出: "HELLO, WORLD"

Kotlin提供了一个内置函数toUpperCase()，它可以将字符串转换为大写形式。我们只需要在函数中传入要转换的字符串，然后将结果打印出来即可。

## 深入了解字符串大写转换

在实际的编程中，我们经常遇到需要将字符串进行格式化的情况。而转换为大写则是其中一种常用的方式。Kotlin的toUpperCase()函数基于Unicode规则进行转换，因此即使是特殊字符也可以被正确转换为大写形式。

此外，Kotlin还提供了其他一些与字符串大小写相关的函数，例如toLowerCase()函数可以将字符串转换为小写形式，而capitalize()函数可以将字符串的首字母转换为大写形式。

## 参考链接

- [Kotlin官方网站](https://kotlinlang.org/)
- [Kotlin String文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin字符串处理函数](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)

## 参见

- [Kotlin中的字符串操作](https://blog.csdn.net/qq_27940385/article/details/79461249)
- [如何进行Kotlin字符串转换](https://blog.jetbrains.com/blog/2017/07/26/did-you-know-kotlin-1-1-create-javaclass-by-string-jstring-format-javakotlin-difference/)