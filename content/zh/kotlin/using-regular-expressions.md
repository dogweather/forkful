---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么&为什么？)
正则表达式是文字模式，用于匹配、检索和替换文本。程序员用它快速处理复杂文本，实现搜索、数据验证等功能。

## How to (如何操作)
```kotlin
fun main() {
    val text = "To be, or not to be, that is the question."
    val regex = Regex("[tT]o\\sbe")
    val matches = regex.findAll(text)

    matches.forEach { matchResult ->
        println(matchResult.value)
    }
}
```
输出：
```
To be
to be
```

## Deep Dive (深入了解)
正则表达式起源于1950年代的神经生物学研究。现今替代品有字符串startsWith, contains等方法，但不如正则强大。Kotlin使用`java.util.regex`实现正则表达式功能。

## See Also (参阅)
- Kotlin官方文档: [Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- 正则表达式教程: [regex101](https://regex101.com/)
- JetBrains' Kotlin 论坛: [Kotlin Discussions](https://discuss.kotlinlang.org/)