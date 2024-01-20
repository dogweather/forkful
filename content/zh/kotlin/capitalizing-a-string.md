---
title:                "字符串首字母大写"
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么是字符串大写以及为什么要使用它?
字符串大写就是将字符串中的所有小写字符转换为大写。程序员大写字符串，是因为有时我们需要统一文本格式，如文档标题或用户输入。

## How to: 如何操作：
```Kotlin
fun main() {
    val text = "hello world"
    val capitalizedText = text.uppercase()

    println(capitalizedText) // 输出：HELLO WORLD
}
```

## Deep Dive 深入探讨
字符串大写是文本处理的一个基础操作，早已存在于许多编程语言当中。Kotlin 提供了 `.uppercase()` 函数方便实现这个功能。然而，在早期版本的 Kotlin（1.0），我们使用的是`.toUpperCase()`。由于国际化的考量，Kotlin 1.5 引入了`.uppercase()`，它更智能地处理不同语言中字符的大写转换。

对于小写转换，同样的，我们使用`.lowercase()`来代替`.toLowerCase()`。

使用`.uppercase()`和`.lowercase()`函数时，Kotlin 基于当前的`Locale`来转换字符。如果你需要针对特定区域设置进行转换，可以传递`Locale`参数。

其他语言，如Python有`str.upper()`，JavaScript有`.toUpperCase()`，都提供类似功能。

## See Also 参考链接
- Kotlin Documentation for Strings: [https://kotlinlang.org/docs/strings.html#string-functions](https://kotlinlang.org/docs/strings.html#string-functions)
- Comparison of string methods across different languages: [https://en.wikipedia.org/wiki/String_operations](https://en.wikipedia.org/wiki/String_operations)
- Unicode and Internationalization considerations: [https://unicode.org/](https://unicode.org/)