---
title:                "日志记录"
aliases:
- /zh/java/logging/
date:                  2024-01-26T01:06:57.862292-07:00
model:                 gpt-4-1106-preview
simple_title:         "日志记录"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/logging.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
日志记录本质上是记录发生在软件应用程序内部的事件的过程。程序员记录这些事件以捕获运行时信息、调试问题、监控系统行为，并为安全和合规目的创建审计迹象。

## 如何操作：
以下是使用内置的 `java.util.logging` 包在Java中开始进行日志记录的简单方法。

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("记录一个INFO级别的消息");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "发生异常", e);
        }
    }
}
```

这将产生以下类似的输出：

```
Jul 03, 2023 2:00:00 PM AppLogging main
INFO: 记录一个INFO级别的消息
Jul 03, 2023 2:00:00 PM AppLogging main
SEVERE: 发生异常
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## 深入了解
Java中的日志已经有了相当大的发展。传统上，日志更多是以系统输出和自行编写的机制为主。然而，标准化的需求导致了如 `Log4j` 和 `SLF4J` 等日志API的出现。`java.util.logging` 包在JDK 1.4中引入，提供了一种标准化的消息日志方法。

`java.util.logging`（JUL）的替代品包括Log4j 2和SLF4J。尽管JUL内置于Java中，因此不需要额外的依赖项，但Log4j 2和SLF4J提供了更多高级功能，如更细致的控制日志配置、异步日志记录和更好的性能。

从实现角度来看，日志可以是同步的，即每条日志消息在产生它的线程中处理，也可以是异步的，即消息被交给另一个线程处理。异步日志记录可以提高性能，但引入了难度，因为必须处理并发并确保应用程序崩溃时不会丢失日志消息。

## 另请参阅
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracle官方日志概述](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [关于java.util.logging的教程](https://www.vogella.com/tutorials/Logging/article.html)
