---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
YAML是一种直观的数据序列化格式，用于配置文件和数据交换。程序员使用YAML因为它易于阅读，可读性好，适合人类和计算机。

## How to: 如何操作
```kotlin
import org.yaml.snakeyaml.Yaml
import java.io.InputStream

fun main() {
    val yaml = Yaml()
    val inputStream: InputStream = this::class.java.classLoader.getResourceAsStream("config.yaml")
    val data: Map<String, Any> = yaml.load(inputStream)

    println(data["name"])
    println(data["language"])
}

// config.yaml 文件内容:
// name: Kotlin Example
// language: Kotlin

// 输出:
// Kotlin Example
// Kotlin
```

## Deep Dive 深入了解
YAML起源于2001年，是"YAML Ain't Markup Language"（YAML不是标记语言）的递归缩写。JSON和XML是YAML的两种选择性数据序列化格式。在Kotlin中，处理YAML常用的库是SnakeYAML，它提供了对YAML的编码和解码功能。

## See Also 另请参阅
- YAML官方网站: [https://yaml.org](https://yaml.org)
- Kotlin官方文档: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
