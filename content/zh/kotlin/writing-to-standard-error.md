---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? / 什么以及为什么?
写入标准错误是将错误信息发送至程序的标准错误流（stderr），用于区分常规输出内容。程序员这样做是为了便于调试和记录错误，使它们易于跟踪并可通过错误管理工具进行处理。

## How to / 如何做：
```kotlin
fun main() {
    // 正常消息写入标准输出
    println("这是一个标准输出消息")

    // 错误消息写入标准错误
    System.err.println("这是一个错误消息")
}

输出:
这是一个标准输出消息
这是一个错误消息  // 这行会在标准错误流中显示
```

## Deep Dive / 深入探索
标准错误（stderr）自Unix诞生以来就存在。与标准输出（stdout）逻辑分离，stderr允许错误信息重定向和独立处理。与 `println` 写入stdout不同，使用 `System.err.println` 可以将信息发送到stderr。虽然在现代系统中，还可以使用日志框架来更精细地控制错误管理，但写入stderr仍是快速和简单的错误反馈方式。

## See Also / 另见
- Kotlin官方文档: [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- Unix标准流: [Wikipedia: Standard streams](https://en.wikipedia.org/wiki/Standard_streams)
- 日志框架介绍: [Logging in Kotlin](https://www.baeldung.com/kotlin/logging)
