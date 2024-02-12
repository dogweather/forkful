---
title:                "写入标准错误"
aliases:
- /zh/java/writing-to-standard-error/
date:                  2024-02-03T19:33:32.558323-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
将错误信息和诊断信息写入标准错误（stderr）涉及到将这些信息输出到控制台或终端。程序员这样做是为了将错误信息与标准输出（stdout）分开，便于调试和日志分析。

## 如何操作：

### Java中的基本stderr输出
Java提供了一个直接的方式来通过`System.err.print()`或`System.err.println()`写入stderr。以下是如何做到的：

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("错误：不能除以零。");
        }
    }
}
```

示例输出：

```
错误：不能除以零。
```

这将直接将错误信息打印到标准错误流。

### 使用日志记录器进行高级错误处理
对于需要更复杂的错误处理和日志记录的应用程序，通常使用像SLF4J与Logback或Log4J2这样的日志库。这允许在管理错误输出方面提供更多的灵活性，包括文件重定向、过滤和格式化。

#### 使用Logback的示例

首先，将Logback的依赖项添加到`pom.xml`（Maven）或`build.gradle`（Gradle）文件。对于Maven：

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

然后，你可以使用以下代码来记录错误：

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("错误：不能除以零。", e);
        }
    }
}
```

这将根据Logback配置将错误消息连同堆栈跟踪输出到控制台或文件。

使用像Logback这样的日志框架提供了对错误处理的更多控制，使管理大型应用程序和系统变得更加容易。
