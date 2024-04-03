---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:38.425452-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u5728Kotlin\u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528\u6807\u51C6\u5E93\u51FD\u6570\u5BF9\u5B57\u7B26\u4E32\u8FDB\u884C\u9996\u5B57\
  \u6BCD\u5927\u5199\u5904\u7406\uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u3002Kotlin\u5904\
  \u7406\u5B57\u7B26\u4E32\u7684\u65B9\u6CD5\u4F7F\u5F97\u8FD9\u4E9B\u64CD\u4F5C\u76F4\
  \u63A5\u4E14\u7B80\u6D01\u3002 #."
lastmod: '2024-03-13T22:44:47.700665-06:00'
model: gpt-4-0125-preview
summary: "\u5728Kotlin\u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528\u6807\u51C6\u5E93\u51FD\
  \u6570\u5BF9\u5B57\u7B26\u4E32\u8FDB\u884C\u9996\u5B57\u6BCD\u5927\u5199\u5904\u7406\
  \uFF0C\u65E0\u9700\u7B2C\u4E09\u65B9\u5E93\u3002Kotlin\u5904\u7406\u5B57\u7B26\u4E32\
  \u7684\u65B9\u6CD5\u4F7F\u5F97\u8FD9\u4E9B\u64CD\u4F5C\u76F4\u63A5\u4E14\u7B80\u6D01\
  ."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何实现：
在Kotlin中，可以使用标准库函数对字符串进行首字母大写处理，无需第三方库。Kotlin处理字符串的方法使得这些操作直接且简洁。

### 将整个字符串大写：
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // 输出：HELLO, WORLD!
```

### 仅将第一个字符大写：
截至Kotlin 1.5，`capitalize()`函数已被弃用，并被`replaceFirstChar`和一个lambda函数组合替代，该lambda函数检查它是否为小写字母并将其转换为大写。

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // 输出：Hello, world!
```

这种方法保持了句子其余部分的原始形式，同时仅将第一个字母改为大写。
