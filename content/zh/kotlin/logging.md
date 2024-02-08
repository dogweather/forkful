---
title:                "日志记录"
aliases:
- zh/kotlin/logging.md
date:                  2024-01-26T01:06:43.389738-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/logging.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在其核心，日志记录是记录软件应用程序中的事件和数据到外部输出的实践，像是文件或控制台。程序员记录日志以追踪代码，解决问题，以及监控应用程序在野外的行为，提供不能以任何其他方式有效获得的关键洞见。

## 如何操作：

在 Kotlin 中，日志记录可以使用内置的 `println()` 函数来处理简单情况，或者使用像 SLF4J 和 Logback 或 Log4j 这样的更复杂的库来满足高级需求。

以下是使用 `println()` 的基本示例：

```Kotlin
fun main() {
    println("简单日志消息：应用程序启动。")
    // ... 一些应用程序逻辑在这里 ...
    try {
        // 模拟一个错误
        throw Exception("模拟错误")
    } catch (e: Exception) {
        println("错误日志消息：" + e.message)
    }
}
```

输出：
```
简单日志消息：应用程序启动。
错误日志消息：模拟错误
```

这里是一个配置了 SLF4J 和 Logback 的代码片段：

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("结构化日志消息：应用程序启动。")
    // ... 一些应用程序逻辑在这里 ...
    try {
        // 模拟一个错误
        throw Exception("模拟错误")
    } catch (e: Exception) {
        logger.error("结构化错误日志：", e)
    }
}
```

假设适当的 Logback 配置，输出会被格式化，可能在写入日志文件时看起来像这样：
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - 结构化日志消息：应用程序启动。
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - 结构化错误日志：
java.lang.Exception: 模拟错误
   at com.myapp.Main.main(Main.kt:10)
```

## 深入探究

历史上，随着应用程序和系统的复杂性增加，软件日志在同步发展。在早期，程序通常由开发者自己运行和调试，简单的打印语句就足够了。但随着系统联网，在不同环境、不同用户中运行，一个健壮且持久的日志系统变得至关重要。

在 Kotlin 变得流行之前，Java 开发者广泛采用了像 Log4j 并且后来是 SLF4J 的库。这些在 Kotlin 中启发了类似的实践，利用 Kotlin 与 Java 库的互操作性。SLF4J 充当一个抽象层，允许实际的日志实现被替换——通常 Logback 或 Log4j2 是首选。

Kotlin 还允许多平台日志解决方案在 JVM、JavaScript 和 Native 上工作，例如通过 `expect`/`actual` 机制，它抽象了平台特定的实现。

与专用的日志库相比，println 作为最简单的日志形式之所以坚持存在，是因为它不需要额外的设置或依赖；然而，由于缺乏像日志级别、日志轮转和结构化格式这样的功能，它通常不适合生产应用程序。

高级日志框架的其他常见功能包括：

- 日志级别（DEBUG、INFO、WARN、ERROR 等），用于分类日志消息的紧迫性。
- 输出到各种目标，如控制台、文件、数据库或网络服务。
- 自动日志轮转和保留政策。
- 对于微服务架构的分布式追踪支持。
- 使用 JSON 等格式的结构化日志，这与日志分析系统很好地整合。

这些工具和特性对于维护一个可靠、可观察的系统尤其在复杂、分布式或高度扩展的环境中至关重要。

## 另请参阅

想要进一步学习和洞见 Kotlin 日志记录，请查看：

- SLF4J (简单的 Java 日志门面) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback，Log4j 的后续者 [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Kotlin 多平台文档上关于 'expect' 和 'actual' 声明的：[https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Kotlin 结构化日志记录指南：[https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
