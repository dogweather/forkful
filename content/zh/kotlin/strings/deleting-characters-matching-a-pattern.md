---
date: 2024-01-20 17:42:32.276596-07:00
description: "\u5220\u9664\u5B57\u7B26\u5339\u914D\u6A21\u5F0F\u662F\u627E\u51FA\u7B26\
  \u5408\u7279\u5B9A\u89C4\u5219\u7684\u5B57\u7B26\uFF0C\u7136\u540E\u628A\u5B83\u4EEC\
  \u79FB\u9664\u6389\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6570\
  \u636E\u6E05\u6D17\u3001\u683C\u5F0F\u5316\uFF0C\u6216\u8005\u7B80\u5316\u5B57\u7B26\
  \u4E32\u5904\u7406\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.701786-06:00'
model: gpt-4-1106-preview
summary: "\u5220\u9664\u5B57\u7B26\u5339\u914D\u6A21\u5F0F\u662F\u627E\u51FA\u7B26\
  \u5408\u7279\u5B9A\u89C4\u5219\u7684\u5B57\u7B26\uFF0C\u7136\u540E\u628A\u5B83\u4EEC\
  \u79FB\u9664\u6389\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6570\
  \u636E\u6E05\u6D17\u3001\u683C\u5F0F\u5316\uFF0C\u6216\u8005\u7B80\u5316\u5B57\u7B26\
  \u4E32\u5904\u7406\u4EFB\u52A1\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么？
删除字符匹配模式是找出符合特定规则的字符，然后把它们移除掉。程序员这么做是为了数据清洗、格式化，或者简化字符串处理任务。

## How to: 如何做？
```kotlin
fun main() {
    val regex = Regex("[aeiou]") // 定义一个只包含元音字母的模式
    val input = "Hello, World!"
    val result = input.replace(regex, "") // 删除所有匹配的字符

    println(result) // 输出: Hll, Wrld!
}
```

## Deep Dive 深度探索
Kotlin 提供简洁的正则表达式 API 来处理模式匹配。这个特性源自 Kotlin 旨在改善 Java 语言繁杂冗长的处理方法。你也可以使用 `filterNot` 来实现相同效果，但正则表达式因其强大的模式匹配能力而更常使用。实现的细节底层依赖 Java 的 `Pattern` 和 `Matcher` 类。

## See Also 查看更多
- Kotlin 正则表达式官方文档: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- `Pattern` 类 in Java: [Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- `Matcher` 类 in Java: [Matcher](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html)
