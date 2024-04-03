---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:32.558323-07:00
description: "\u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u6D89\u53CA\u5230\u5C06\u8FD9\u4E9B\u4FE1\
  \u606F\u8F93\u51FA\u5230\u63A7\u5236\u53F0\u6216\u7EC8\u7AEF\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C06\u9519\u8BEF\u4FE1\u606F\u4E0E\u6807\u51C6\
  \u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4FBF\u4E8E\u8C03\u8BD5\u548C\u65E5\
  \u5FD7\u5206\u6790\u3002"
lastmod: '2024-03-13T22:44:47.643457-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u6D89\u53CA\u5230\u5C06\u8FD9\u4E9B\u4FE1\
  \u606F\u8F93\u51FA\u5230\u63A7\u5236\u53F0\u6216\u7EC8\u7AEF\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C06\u9519\u8BEF\u4FE1\u606F\u4E0E\u6807\u51C6\
  \u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4FBF\u4E8E\u8C03\u8BD5\u548C\u65E5\
  \u5FD7\u5206\u6790\u3002."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
