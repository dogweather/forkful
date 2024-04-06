---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.889722-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Kotlin\u4E2D\uFF0C\u4F7F\u7528\
  `System.err.println()`\u53EF\u4EE5\u5B9E\u73B0\u5199\u5165stderr\u3002\u8FD9\u4E2A\
  \u65B9\u6CD5\u4E0E`System.out.println()`\u76F8\u4F3C\uFF0C\u4F46\u662F\u5C06\u8F93\
  \u51FA\u5B9A\u5411\u5230\u6807\u51C6\u9519\u8BEF\u6D41\uFF0C\u800C\u4E0D\u662F\u6807\
  \u51C6\u8F93\u51FA\u6D41\u3002"
lastmod: '2024-04-05T21:53:48.054486-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
