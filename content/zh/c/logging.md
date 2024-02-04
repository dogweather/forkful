---
title:                "日志记录"
date:                  2024-02-03T17:58:58.143358-07:00
model:                 gpt-4-0125-preview
simple_title:         "日志记录"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/logging.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

在C语言中，日志记录涉及在程序运行时记录程序流程和显著事件，提供其行为和性能的可观察审查。程序员利用日志记录进行调试，监控软件健康状况，以及确保系统安全。

## 如何实现:

在C语言中，日志记录可以通过基本的文件操作或使用更复杂的库来实现。为了简单起见，我们从标准输入输出库开始。以下代码片段展示了基本的日志记录实现。

记录简单消息：

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // 以附加模式打开日志文件
    
    if (logFile == NULL) {
        perror("打开日志文件出错。");
        return -1;
    }
    
    fprintf(logFile, "开始应用程序。\n");
    
    // 你的应用程序逻辑在这里
    
    fprintf(logFile, "应用程序成功结束。\n");
    fclose(logFile);
    
    return 0;
}
```

`application.log`中的输出：

```
开始应用程序。
应用程序成功结束。
```

包含时间戳和日志级别的更详细日志：

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // 移除换行符
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("打开日志文件出错。");
        return -1;
    }
    
    logMessage(logFile, "INFO", "应用程序启动");
    // 你的应用程序逻辑在这里
    logMessage(logFile, "ERROR", "一个示例错误");
    
    fclose(logFile);
    
    return 0;
}
```

`detailed.log`中的输出：

```
[Thu Mar 10 14:32:01 2023] INFO - 应用程序启动
[Thu Mar 10 14:32:02 2023] ERROR - 一个示例错误
```

## 深入探讨

正如演示所示，C语言中的日志记录依赖于简单的文件操作，这虽然有效，但并不像其他语言中的日志记录设施那样强大或灵活，如Python的`logging`模块或Java的`Log4j`。为了在C语言中获得更高级的日志记录能力，开发人员通常转向像Unix-like系统上的`syslog`这样提供系统范围内日志管理的库，或者是`log4c`这样的第三方库。

从历史上看，日志记录一直是编程的一个组成部分，追溯到早期编程实践，其中通过物理打印输出来追踪和理解程序流程和错误。随着系统的发展，日志记录变得更加复杂，现在支持各种严重性级别，日志轮转和异步日志记录。

虽然C的标准库提供了实现日志记录的基本工具，但其限制通常导致创建自定义日志记录框架或采用外部库以获得更多功能丰富且灵活的日志解决方案。尽管存在这些限制，理解和实现C语言中的基本日志记录对于调试和维护软件至关重要，特别是在需要最小化外部依赖的环境中。
