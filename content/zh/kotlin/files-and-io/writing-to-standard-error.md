---
title:                "写入标准错误"
aliases:
- /zh/kotlin/writing-to-standard-error/
date:                  2024-02-03T19:33:48.889722-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

将信息写入标准错误（stderr）是将错误消息和诊断输出到一个独立的流中，与标准输出（stdout）不同，这允许更好的错误处理和日志解析。程序员这样做是为了便于调试，并确保错误信息可以被轻松识别和必要时重定向，保持清晰的输出日志或用户消息。

## 如何操作：

在Kotlin中，使用`System.err.println()`可以实现写入stderr。这个方法与`System.out.println()`相似，但是将输出定向到标准错误流，而不是标准输出流。

```kotlin
fun main() {
    System.err.println("这是一个错误消息！")
}
```

示例输出：
```
这是一个错误消息！
```

对于更结构化或复杂的应用程序，尤其是那些涉及到如Logback或SLF4J这样的日志框架，您可以配置日志记录器在特定日志级别（例如，错误）写入stderr。

使用SLF4J与Logback：

1. 首先，将SLF4J API和Logback实现添加到您的`build.gradle`中：

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. 接下来，在`src/main/resources/logback.xml`中配置Logback以将错误级别的消息定向到stderr：

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. 然后，在您的Kotlin代码中使用SLF4J来记录错误消息：

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("这是一个错误日志消息！")
}
```

示例输出（至stderr）：
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - 这是一个错误日志消息！
```
