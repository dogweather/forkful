---
date: 2024-01-20 17:42:32.276596-07:00
description: "How to: \u5982\u4F55\u505A\uFF1F Kotlin \u63D0\u4F9B\u7B80\u6D01\u7684\
  \u6B63\u5219\u8868\u8FBE\u5F0F API \u6765\u5904\u7406\u6A21\u5F0F\u5339\u914D\u3002\
  \u8FD9\u4E2A\u7279\u6027\u6E90\u81EA Kotlin \u65E8\u5728\u6539\u5584 Java \u8BED\
  \u8A00\u7E41\u6742\u5197\u957F\u7684\u5904\u7406\u65B9\u6CD5\u3002\u4F60\u4E5F\u53EF\
  \u4EE5\u4F7F\u7528 `filterNot` \u6765\u5B9E\u73B0\u76F8\u540C\u6548\u679C\uFF0C\u4F46\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u56E0\u5176\u5F3A\u5927\u7684\u6A21\u5F0F\u5339\u914D\
  \u80FD\u529B\u800C\u66F4\u5E38\u4F7F\u7528\u3002\u5B9E\u73B0\u7684\u7EC6\u8282\u5E95\
  \u5C42\u4F9D\u8D56 Java \u7684\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.021282-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1F Kotlin \u63D0\u4F9B\u7B80\u6D01\u7684\u6B63\u5219\
  \u8868\u8FBE\u5F0F API \u6765\u5904\u7406\u6A21\u5F0F\u5339\u914D\u3002\u8FD9\u4E2A\
  \u7279\u6027\u6E90\u81EA Kotlin \u65E8\u5728\u6539\u5584 Java \u8BED\u8A00\u7E41\
  \u6742\u5197\u957F\u7684\u5904\u7406\u65B9\u6CD5\u3002\u4F60\u4E5F\u53EF\u4EE5\u4F7F\
  \u7528 `filterNot` \u6765\u5B9E\u73B0\u76F8\u540C\u6548\u679C\uFF0C\u4F46\u6B63\u5219\
  \u8868\u8FBE\u5F0F\u56E0\u5176\u5F3A\u5927\u7684\u6A21\u5F0F\u5339\u914D\u80FD\u529B\
  \u800C\u66F4\u5E38\u4F7F\u7528\u3002\u5B9E\u73B0\u7684\u7EC6\u8282\u5E95\u5C42\u4F9D\
  \u8D56 Java \u7684 `Pattern` \u548C `Matcher` \u7C7B\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

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
