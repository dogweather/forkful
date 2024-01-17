---
title:                "将一个字符串变换成大写"
html_title:           "Kotlin: 将一个字符串变换成大写"
simple_title:         "将一个字符串变换成大写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 什么和为什么？
首先，将字符串中的字母改为大写是一种常见的编程技巧。程序员会这样做是为了让字符串在输出时更容易阅读，并且在比较和匹配字符串时更加准确。

## 如何：
```Kotlin
  val str = "hello world"
  val capitalizedStr = str.toUpperCase()
  println(capitalizedStr)
```
输出结果：HELLO WORLD
上面的代码展示了如何用Kotlin将一个字符串全部转换为大写。首先，我们需要声明一个字符串变量str并赋值为"hello world"，然后通过调用toUpperCase()函数，将该变量中的所有字母转换为大写形式。最后，使用println()函数打印出转换后的字符串。

## 深入了解：
将字符串中的字母改为大写实际上是一种格式化字符串的方法，可以使字符串更加统一，易于处理。在早期的编程语言中，如C和Java，通常需要使用循环和条件语句来实现这一操作。但是在Kotlin中，我们可以直接使用toUpperCase()函数来实现，更加简洁高效。

## 参考资料：
- [Kotlin文档](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [C语言中的字符串处理](https://www.programiz.com/c-programming/c-strings)