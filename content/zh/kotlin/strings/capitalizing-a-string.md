---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:38.425452-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u6D89\u53CA\u5230\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\
  \u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF08\u5982\u679C\u5B83\u5C1A\u672A\u5927\u5199\
  \uFF09\uFF0C\u8FD9\u5BF9\u4E8E\u683C\u5F0F\u5316\u7528\u6237\u8F93\u5165\u6216\u4EE5\
  \u66F4\u6807\u51C6\u5316\u6216\u66F4\u53CB\u597D\u7684\u65B9\u5F0F\u5728\u7528\u6237\
  \u754C\u9762\u4E2D\u663E\u793A\u6587\u672C\u975E\u5E38\u6709\u7528\u3002\u7A0B\u5E8F\
  \u5458\u6267\u884C\u8FD9\u4E2A\u64CD\u4F5C\u662F\u4E3A\u4E86\u786E\u4FDD\u6570\u636E\
  \u4E00\u81F4\u6027\u6216\u6EE1\u8DB3\u8F6F\u4EF6\u5E94\u7528\u7A0B\u5E8F\u5185\u7684\
  \u7279\u5B9A\u683C\u5F0F\u8981\u6C42\u3002"
lastmod: '2024-03-13T22:44:47.700665-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5C06\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u6D89\u53CA\u5230\u5C06\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\
  \u7B26\u8F6C\u6362\u4E3A\u5927\u5199\uFF08\u5982\u679C\u5B83\u5C1A\u672A\u5927\u5199\
  \uFF09\uFF0C\u8FD9\u5BF9\u4E8E\u683C\u5F0F\u5316\u7528\u6237\u8F93\u5165\u6216\u4EE5\
  \u66F4\u6807\u51C6\u5316\u6216\u66F4\u53CB\u597D\u7684\u65B9\u5F0F\u5728\u7528\u6237\
  \u754C\u9762\u4E2D\u663E\u793A\u6587\u672C\u975E\u5E38\u6709\u7528\u3002\u7A0B\u5E8F\
  \u5458\u6267\u884C\u8FD9\u4E2A\u64CD\u4F5C\u662F\u4E3A\u4E86\u786E\u4FDD\u6570\u636E\
  \u4E00\u81F4\u6027\u6216\u6EE1\u8DB3\u8F6F\u4EF6\u5E94\u7528\u7A0B\u5E8F\u5185\u7684\
  \u7279\u5B9A\u683C\u5F0F\u8981\u6C42\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，将字符串首字母大写涉及到将字符串的第一个字符转换为大写（如果它尚未大写），这对于格式化用户输入或以更标准化或更友好的方式在用户界面中显示文本非常有用。程序员执行这个操作是为了确保数据一致性或满足软件应用程序内的特定格式要求。

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
