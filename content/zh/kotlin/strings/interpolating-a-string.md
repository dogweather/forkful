---
date: 2024-01-20 17:51:09.194177-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:59.281250-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5B57\u7B26\u4E32\u63D2\u503C\u6700\u65E9\
  \u51FA\u73B0\u5728Shell\u548CPerl\u811A\u672C\u8BED\u8A00\u4E2D\uFF0C\u540E\u6765\
  \u88ABRuby\u7B49\u5176\u4ED6\u8BED\u8A00\u91C7\u7528\u3002\u5B83\u7684\u4F18\u70B9\
  \u662F\u4EE3\u7801\u53EF\u8BFB\u6027\u5F3A\uFF0C\u53EF\u4EE5\u76F4\u63A5\u89C2\u5BDF\
  \u5230\u5B57\u7B26\u4E32\u7684\u6700\u7EC8\u5F62\u6001\u3002\u5728Kotlin\u4E2D\uFF0C\
  \u901A\u8FC7`$`\u7B26\u53F7\u540E\u8DDF\u53D8\u91CF\u540D\u6216`${}`\u5305\u542B\
  \u8868\u8FBE\u5F0F\u7684\u65B9\u5F0F\u6765\u63D2\u503C\u3002\u5728\u7F16\u8BD1\u6210\
  \u5B57\u8282\u7801\u540E\uFF0CKotlin\u7684\u5B57\u7B26\u4E32\u63D2\u503C\u7531`StringBuilder`\u5904\
  \u7406\uFF0C\u4FDD\u8BC1\u4E86\u8FD0\u884C\u6027\u80FD\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
