---
date: 2024-01-20 17:47:51.139055-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5F97\u5230\u4E00\u4E2A\u5B57\u7B26\u4E32\
  \u7684\u957F\u5EA6\u5C31\u662F\u77E5\u9053\u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\
  \u7B26\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u9A8C\u8BC1\u8F93\u5165\
  \u3001\u9650\u5236\u6587\u5B57\u91CF\u6216\u8005\u4EC5\u4EC5\u662F\u4E3A\u4E86\u64CD\
  \u4F5C\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.709531-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5F97\u5230\u4E00\u4E2A\u5B57\u7B26\u4E32\
  \u7684\u957F\u5EA6\u5C31\u662F\u77E5\u9053\u5B83\u5305\u542B\u591A\u5C11\u4E2A\u5B57\
  \u7B26\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u9A8C\u8BC1\u8F93\u5165\
  \u3001\u9650\u5236\u6587\u5B57\u91CF\u6216\u8005\u4EC5\u4EC5\u662F\u4E3A\u4E86\u64CD\
  \u4F5C\u6570\u636E\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么?)
在编程中，得到一个字符串的长度就是知道它包含多少个字符。我们这么做是为了验证输入、限制文字量或者仅仅是为了操作数据。

## How to: (如何做：)
Kotlin 里，用`.length`属性来得到字符串长度。就这么简单：

```Kotlin
fun main() {
    val greeting = "你好，世界！"
    println("字符串长度: ${greeting.length}")
}
```
输出将是：
```
字符串长度: 6
```

## Deep Dive (深入探究)
`length`属性返回的是`Int`类型的值，它表示Unicode字符的数量。在Kotlin中，这和Java字符串的实现是一致的。但要注意，如果字符串包含了代理对（surrogate pairs），比如某些emoji字符，`.length`可能不会返回你期待的结果，因为这些是由两个`Char`表示的。

如果你需要考虑这类字符，可以使用`codePointCount`方法：

```Kotlin
fun main() {
    val stringWithEmoji = "👋🌍"
    val actualLength = stringWithEmoji.codePointCount(0, stringWithEmoji.length)
    println("实际字符长度: $actualLength")
}
```
输出：
```
实际字符长度: 2
```

历史上，字符串长度的处理方法已经随着时间演进，尤其是因为Unicode和国际化的支持。Kotlin让事情变得简单，但了解背后的机制仍然很重要。

## See Also (另见)
- [Kotlin官方文档：字符串操作](https://kotlinlang.org/docs/collections-overview.html#字符串操作)
- [Unicode码位与Java/Kotlin中的代理对](https://developer.android.com/guide/topics/resources/string-resource#FormattingAndStyling)
- [Kotlin Playground: 在线尝试Kotlin代码](https://play.kotlinlang.org/)
