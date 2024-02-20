---
date: 2024-01-20 17:51:09.194177-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u503C\u5D4C\u5165\
  \u5B57\u7B26\u4E32\u4E2D\u7684\u6280\u672F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u65B9\u4FBF\u751F\u6210\u52A8\u6001\u3001\u5305\u542B\u53EF\u53D8\
  \u6570\u636E\u7684\u6587\u672C\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.733340
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u503C\u5D4C\u5165\
  \u5B57\u7B26\u4E32\u4E2D\u7684\u6280\u672F\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u662F\u4E3A\u4E86\u65B9\u4FBF\u751F\u6210\u52A8\u6001\u3001\u5305\u542B\u53EF\u53D8\
  \u6570\u636E\u7684\u6587\u672C\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串插值是将变量值嵌入字符串中的技术。程序员这么做是为了方便生成动态、包含可变数据的文本。

## How to: (如何操作：)
```kotlin
fun main() {
    val name = "张三"
    val age = 28
    println("姓名：$name, 年龄：$age") // 输出: 姓名：张三, 年龄：28

    val message = "下面是你的个人信息:\n姓名：$name\n年龄：${age + 2}"
    println(message) // 输出: 下面是你的个人信息:
                     // 姓名：张三
                     // 年龄：30
}
```

## Deep Dive (深入探索)
字符串插值最早出现在Shell和Perl脚本语言中，后来被Ruby等其他语言采用。它的优点是代码可读性强，可以直接观察到字符串的最终形态。在Kotlin中，通过`$`符号后跟变量名或`${}`包含表达式的方式来插值。在编译成字节码后，Kotlin的字符串插值由`StringBuilder`处理，保证了运行性能。

## See Also (延伸阅读)
- 《Kotlin in Action》 - 了解更深层次的Kotlin开发知识
- 探索字符串插值在其他编程语言中的使用情况
